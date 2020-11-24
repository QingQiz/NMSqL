use super::VmMem::VmMemString;

#[derive(Default, Clone)]
pub struct VmList {
  datas: Vec<VmMemString>,
  nowRead: usize,
}

impl VmList {
  pub fn clear(self: &mut Self) {
    self.datas.clear();
    self.nowRead = 0;
  }
  pub fn push(self: &mut Self, data: VmMemString) {
    self.datas.push(data);
  }
  pub fn rewind(self: &mut Self) {
    self.nowRead = 0;
  }
  pub fn read(self: &mut Self) -> Option<VmMemString> {
    if self.nowRead == self.datas.len() {
      None
    } else {
      let ret = self.datas[self.nowRead].clone();
      self.nowRead += 1;
      Some(ret)
    }
  }
}
