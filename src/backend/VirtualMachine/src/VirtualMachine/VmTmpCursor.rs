use super::VmMem::{VmMem, VmMemString, VmMemStringIterator};
use super::VmUtil::*;
#[derive(Default, Clone)]
pub struct VmTmpCursor {
  keys: Vec<Vec<VmMem>>,
  values: Vec<Vec<VmMem>>,
  nowIdx: usize,
  keyAsData: bool,
}
impl VmTmpCursor {
  pub fn new() -> Self {
    Self::default()
  }
  pub fn put(&mut self, key: &VmMemString, value: &VmMemString) {
    self.keys.push(
      key
        .iter()
        .map(|x| {
          let len = (x[0] as usize) << 8 | (x[1] as usize);
          match x[2] {
            1 => VmMem::MEM_INT(vecU8ToI32(&x[4..])),
            2 => VmMem::MEM_DOUBLE(vecU8ToF64(&x[4..])),
            3 => VmMem::MEM_STRING(VmMemString::new(x[4..].to_vec())),
            4 => VmMem::MEM_NULL,
            _ => unreachable!(),
          }
        })
        .collect(),
    );
    self.values.push(
      value
        .iter()
        .map(|x| {
          let len = (x[0] as usize) << 8 | (x[1] as usize);
          match x[2] {
            1 => VmMem::MEM_INT(vecU8ToI32(&x[4..])),
            2 => VmMem::MEM_DOUBLE(vecU8ToF64(&x[4..])),
            3 => VmMem::MEM_STRING(VmMemString::new(x[4..].to_vec())),
            4 => VmMem::MEM_NULL,
            _ => unreachable!(),
          }
        })
        .collect(),
    );
    unimplemented!()
  }
  pub fn columnValue(&self, idx: usize) -> Result<VmMem, String> {
    if self.nowIdx < self.values.len() && idx < self.values[self.nowIdx].len() {
      Ok(self.values[self.nowIdx][idx].clone())
    } else {
      Err(format!(
        "index out of bounds with nowIdx={} idx={}",
        self.nowIdx, idx,
      ))
    }
  }
  pub fn columnKey(&self, idx: usize) -> Result<VmMem, String> {
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
