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
}
