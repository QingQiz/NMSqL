use super::VmMem::{VmMem, VmMemString, VmMemStringIterator};
use super::VmUtil::*;
#[derive(Default, Clone)]
pub struct VmTmpCursor {
  keys: Vec<Vec<VmMemString>>,
  values: Vec<Vec<VmMemString>>,
  nowIdx: usize,
  keyAsData: bool,
}
impl VmTmpCursor {
  pub fn new() -> Self {
    Self::default()
  }
  pub fn put(&mut self, key: &VmMemString, value: &VmMemString) {
    let key = VmMemString::new(key[..].to_vec());
    let value = VmMemString::new(value[..].to_vec());
    self
      .keys
      .push(key.iter().map(|x| VmMemString::new(x.to_vec())).collect());
    self
      .values
      .push(value.iter().map(|x| VmMemString::new(x.to_vec())).collect());
  }
  pub fn columnValue(&self, idx: usize) -> Result<VmMemString, String> {
    if self.nowIdx < self.values.len() && idx < self.values[self.nowIdx].len() {
      Ok(self.values[self.nowIdx][idx].clone())
    } else {
      Err(format!(
        "index out of bounds with nowIdx={} idx={}",
        self.nowIdx, idx,
      ))
    }
  }
  pub fn columnKey(&self, idx: usize) -> Result<VmMemString, String> {
    if self.nowIdx < self.values.len() && idx < self.values[self.nowIdx].len() {
      Ok(self.keys[self.nowIdx][idx].clone())
    } else {
      Err(format!(
        "index out of bounds with nowIdx={} idx={}",
        self.nowIdx, idx,
      ))
    }
  }
  pub fn next(&mut self) {
    self.nowIdx += 1;
  }
  pub fn rewind(&mut self) {
    self.nowIdx = 0;
  }
  pub fn keyAsData(&mut self) {
    self.keyAsData = !self.keyAsData
  }
  pub fn isEnd(&mut self) -> bool {
    self.nowIdx >= self.keys.len()
  }
}
