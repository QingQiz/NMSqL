use super::VmMem::VmMem;
use intrusive_collections::intrusive_adapter;
use intrusive_collections::rbtree::{Cursor, CursorMut};
use intrusive_collections::{Bound, KeyAdapter, RBTree, RBTreeLink};
use std::cell::Cell;
struct AggData {
  link: RBTreeLink,
  key: VmMem,
  value: Vec<VmMem>,
}
intrusive_adapter!(AggAdaptor=Box<AggData>: AggData{link: RBTreeLink});
impl<'a> KeyAdapter<'a> for AggAdaptor {
  type Key = VmMem;
  fn get_key(&self, value: &'a AggData) -> Self::Key {
    value.key.clone()
  }
}
#[derive(Default)]
pub struct VmAgg {
  data: RBTree<AggAdaptor>,
  current: VmMem,
  len: usize,
}

impl VmAgg {
  pub fn focus(self: &mut Self, key: VmMem) -> bool {
    let found = self.data.find(&key);
    self.current = key.clone();
    if_chain! {
      if let Some(x) = found.get();
      if x.key == key;
      then {
        true
      }
      else {
        self.data.insert(Box::new(AggData {
          link: RBTreeLink::new(),
          key: key,
          value: {
            let mut ret = Vec::new();
            ret.resize(self.len,VmMem::MEM_NULL);
            ret
          }}));
        false
      }
    }
  }
  fn getUpperBoundMutCursorByCursor(self: &mut Self) -> CursorMut<AggAdaptor> {
    self.data.lower_bound_mut(Bound::Excluded(&self.current))
  }
  fn getMutCursorByCurrent(self: &mut Self) -> CursorMut<AggAdaptor> {
    self.data.find_mut(&self.current)
  }
  fn popCurrentAgg(self: &mut Self) -> Result<Box<AggData>, String> {
    self
      .getMutCursorByCurrent()
      .remove()
      .ok_or_else(|| String::from("current agg ended"))
  }
  fn pushCurrentAgg(self: &mut Self, data: Box<AggData>) {
    self.getUpperBoundMutCursorByCursor().insert_before(data);
  }
  fn AggDataIndex(data: &mut Box<AggData>, index: usize) -> Result<&mut VmMem, String> {
    let value = &mut data.as_mut().value;
    if index >= value.len() {
      Err(format!(
        "index out of bounds: index={} len={}",
        index,
        value.len()
      ))
    } else {
      Ok(&mut value[index])
    }
  }
  pub fn incr(self: &mut Self, index: usize, num: i32) -> Result<(), String> {
    let mut data = self.popCurrentAgg()?;
    let value = VmAgg::AggDataIndex(&mut data, index)?;
    if let VmMem::MEM_INT(x) = value {
      *x += num;
      self.pushCurrentAgg(data);
      Ok(())
    } else if let VmMem::MEM_NULL = value {
      *value = VmMem::MEM_INT(num);
      self.pushCurrentAgg(data);
      Ok(())
    } else {
      Err(format!(
        "value is not int, cannot be added, value={:?}",
        value
      ))
    }
  }
  pub fn next(self: &mut Self) -> bool {
    let mut cursor = self.getMutCursorByCurrent();
    if cursor.is_null() {
      false
    } else {
      cursor.remove();
      if cursor.is_null() {
        false
      } else {
        self.current = cursor.get().unwrap().key.clone();
        true
      }
    }
  }
  pub fn set(self: &mut Self, index: usize, value: VmMem) -> Result<(), String> {
    let mut data = self.popCurrentAgg()?;
    *VmAgg::AggDataIndex(&mut data, index)? = value;
    self.pushCurrentAgg(data);
    Ok(())
  }
  pub fn get(self: &mut Self, index: usize) -> Result<VmMem, String> {
    let mut data = self.popCurrentAgg()?;
    let ret = VmAgg::AggDataIndex(&mut data, index)?.clone();
    self.pushCurrentAgg(data);
    Ok(ret)
  }
  pub fn resizeCurrent(self: &mut Self, size: usize) -> Result<(), String> {
    self.data.clear();
    self.len = size;
    self.current = VmMem::MEM_NULL;
    self.focus(VmMem::MEM_NULL);
    Ok(())
  }
}
#[cfg(test)]
mod VmAggTest {
  use super::super::VmMem::VmMem;
  use super::super::VmUtil::VmTestUtils::*;
  use super::AggData;
  use super::RBTreeLink;
  use super::VmAgg;
  fn getVmAgg() -> VmAgg {
    let mut ret = VmAgg::default();
    ret.data.insert(Box::new(AggData {
      key: VmMem::MEM_INT(1),
      value: vec![VmMem::MEM_INT(1), VmMem::MEM_INT(2)],
      link: RBTreeLink::new(),
    }));
    ret.data.insert(Box::new(AggData {
      key: VmMem::MEM_INT(2),
      value: vec![VmMem::MEM_INT(2), VmMem::MEM_INT(3)],
      link: RBTreeLink::new(),
    }));
    ret
  }
  #[test]
  fn test_VmAgg_focus_in() {
    let mut agg = getVmAgg();
    assert!(agg.focus(VmMem::MEM_INT(1)));
    assert_eq!(agg.current, VmMem::MEM_INT(1));
  }
  #[test]
  fn test_VmAgg_focus_notIn() {
    let mut agg = getVmAgg();
    assert!(!agg.focus(VmMem::MEM_INT(0)));
    assert_eq!(agg.current, VmMem::MEM_INT(0));
    assert!(agg.focus(VmMem::MEM_INT(0)));
  }
  #[test]
  fn test_VmAgg_incr_work() -> Result<(), String> {
    let mut agg = getVmAgg();
    agg.focus(VmMem::MEM_INT(1));
    agg.incr(0, 2);
    assert_eq!(agg.get(0)?, VmMem::MEM_INT(3));
    assert_eq!(agg.get(1)?, VmMem::MEM_INT(2));
    agg.focus(VmMem::MEM_INT(2));
    assert_eq!(agg.get(0)?, VmMem::MEM_INT(2));
    assert_eq!(agg.get(1)?, VmMem::MEM_INT(3));
    Ok(())
  }
  #[test]
  fn test_VmAgg_incr_fail() {
    let mut agg = getVmAgg();
    assert_eq!(agg.incr(0, 2), Err(String::from("current agg ended")));
    agg.focus(VmMem::MEM_INT(1));
    assert_eq!(
      agg.incr(2, 2),
      Err(String::from("index out of bounds: index=2 len=2"))
    );
    agg.data.insert(Box::new(AggData {
      link: RBTreeLink::new(),
      key: VmMem::MEM_INT(3),
      value: vec![VmMem::MEM_DOUBLE(2.0)],
    }));
    agg.focus(VmMem::MEM_INT(3));
    assert_eq!(
      agg.incr(0, 1),
      Err(format!(
        "value is not int, cannot be added, value={:?}",
        VmMem::MEM_DOUBLE(2.0)
      ))
    );
  }
  #[test]
  fn test_VmAgg_next_work() {
    let mut agg = getVmAgg();
    agg.focus(VmMem::MEM_INT(1));
    assert!(agg.next());
    assert_eq!(agg.current, VmMem::MEM_INT(2));
    assert!(!agg.next());
    agg.focus(VmMem::MEM_INT(2));
    assert!(!agg.next());
    assert!(!agg.next());
  }
  #[test]
  fn test_VmAgg_get_work() {
    let mut agg = getVmAgg();
    agg.focus(VmMem::MEM_INT(1));
    assert_eq!(agg.get(0), Ok(VmMem::MEM_INT(1)));
    assert_eq!(agg.get(1), Ok(VmMem::MEM_INT(2)));
    assert_eq!(
      agg.get(2),
      Err(String::from("index out of bounds: index=2 len=2"))
    );
    agg.focus(VmMem::MEM_INT(2));
    assert_eq!(agg.get(0), Ok(VmMem::MEM_INT(2)));
    assert_eq!(agg.get(1), Ok(VmMem::MEM_INT(3)));
    assert_eq!(
      agg.get(2),
      Err(String::from("index out of bounds: index=2 len=2"))
    );
  }
  #[test]
  fn test_VmAgg_set_work() {
    let mut agg = getVmAgg();
    agg.focus(VmMem::MEM_INT(1));
    assert_eq!(agg.get(0), Ok(VmMem::MEM_INT(1)));
    assert_eq!(agg.set(0, VmMem::MEM_INT(2)), Ok(()));
    assert_eq!(agg.get(0), Ok(VmMem::MEM_INT(2)));
  }
  #[test]
  fn test_VmAgg_resize_work() {
    let mut agg = getVmAgg();
    assert_eq!(agg.resizeCurrent(0), Err(String::from("current agg ended")));
    agg.focus(VmMem::MEM_INT(1));
    agg.resizeCurrent(1);
    assert_eq!(
      agg.getMutCursorByCurrent().get().unwrap().value,
      vec![VmMem::default()]
    );
    agg.resizeCurrent(3);
    assert_eq!(
      agg.getMutCursorByCurrent().get().unwrap().value,
      vec![VmMem::default(), VmMem::default(), VmMem::default()]
    );
  }
}
