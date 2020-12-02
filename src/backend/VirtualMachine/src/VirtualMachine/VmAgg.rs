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
}

impl VmAgg {
  pub fn focus(self: &mut Self, key: VmMem) -> bool {
    let found = self.data.find(&key);
    if_chain! {
      if let Some(x) = found.get();
      if x.key == key;
      then {
        self.current = key;
        true
      }
      else {
        self.data.insert(Box::new(AggData {
                         link: RBTreeLink::new(),
                         key: key.clone(),
                         value: Vec::new()}));
        false
      }
    }
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
    self.getMutCursorByCurrent().insert_before(data);
    self.getMutCursorByCurrent().move_prev();
  }
  fn AggDataIndex(data: &mut Box<AggData>, index: usize) -> Result<&mut VmMem, String> {
    let value = &mut data.as_mut().value;
    if index > value.len() {
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
    let mut data = self.popCurrentAgg()?;
    data.as_mut().value.resize(size, VmMem::default());
    self.pushCurrentAgg(data);
    Ok(())
  }
}
