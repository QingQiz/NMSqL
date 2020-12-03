use super::VmMem::VmMemString;
use std::collections::HashSet;

#[derive(Debug, Clone, Default)]
pub struct VmSet {
  set: HashSet<VmMemString>,
}

impl VmSet {
  pub fn insert(self: &mut Self, data: VmMemString) {
    self.set.insert(data);
  }
  pub fn found(self: &Self, data: VmMemString) -> bool {
    match self.set.get(&data) {
      None => false,
      Some(x) => true,
    }
  }
  pub fn clear(self: &mut Self) {
    self.set.clear();
  }
  pub fn empty(self: &Self) -> bool {
    self.set.is_empty()
  }
}

#[cfg(test)]
mod VmSetTest {
  use super::super::VmUtil::VmTestUtils::*;
  use super::VmSet;
  use std::collections::HashSet;
  fn getVmSet() -> VmSet {
    let mut set = HashSet::new();
    set.insert(getVmMemStringI32(1));
    set.insert(getVmMemStringI32(2));
    set.insert(getVmMemStringI32(3));
    set.insert(getVmMemStringF64(1.0));
    set.insert(getVmMemStringF64(2.0));
    set.insert(getVmMemStringStringA(100));
    set.insert(getVmMemStringStringA(200));
    set.insert(getVmMemStringNull());
    assert_eq!(set.len(), 8);
    VmSet { set }
  }
  #[test]
  fn test_VmSet_insert() {
    let mut set = getVmSet();
    set.insert(getVmMemStringI32(1));
    assert_eq!(set.set.len(), 8);
    set.insert(getVmMemStringI32(4));
    assert_eq!(
      set.set.get(&getVmMemStringI32(4)),
      Some(&getVmMemStringI32(4))
    );
    assert_eq!(set.set.len(), 9);
    set.insert(getVmMemStringF64(1.0));
    assert_eq!(set.set.len(), 9);
    set.insert(getVmMemStringF64(3.0));
    assert_eq!(
      set.set.get(&getVmMemStringF64(3.0)),
      Some(&getVmMemStringF64(3.0))
    );
    assert_eq!(set.set.len(), 10);
    set.insert(getVmMemStringNull());
    assert_eq!(set.set.len(), 10);
    set.insert(getVmMemStringStringA(200));
    assert_eq!(set.set.len(), 10);
    set.insert(getVmMemStringStringA(300));
    assert_eq!(
      set.set.get(&getVmMemStringStringA(300)),
      Some(&getVmMemStringStringA(300))
    );
    assert_eq!(set.set.len(), 11);
  }
  #[test]
  fn test_VmSet_clear() {
    let mut set = getVmSet();
    set.clear();
    assert!(set.set.is_empty());
  }
  #[test]
  fn test_VmSet_found() {
    let mut set = getVmSet();
    assert!(set.found(getVmMemStringNull()));
    assert!(set.found(getVmMemStringI32(1)));
    assert!(!set.found(getVmMemStringI32(10)));
    assert!(set.found(getVmMemStringF64(1.0)));
    assert!(!set.found(getVmMemStringF64(10.0)));
    assert!(set.found(getVmMemStringStringA(100)));
    assert!(!set.found(getVmMemStringStringA(300)));
  }
  #[test]
  fn test_VmSet_empty() {
    let mut set = getVmSet();
    assert!(!set.empty());
    set.clear();
    assert!(set.empty());
  }
}
