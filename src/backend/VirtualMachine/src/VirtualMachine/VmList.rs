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

#[cfg(test)]
mod VmListTest {
  use super::super::VmMem::VmMemString;
  use super::super::VmUtil::VmTestUtils::*;
  use super::super::VmUtil::*;
  use super::VmList;
  fn getVmMemStringA() -> VmMemString {
    getVmMemStringI32(1)
  }
  fn getVmMemStringB() -> VmMemString {
    getVmMemStringF64(1.0)
  }
  fn getVmMemStringC() -> VmMemString {
    getVmMemStringString(16)
  }
  fn getVmList() -> VmList {
    let datas = vec![getVmMemStringA(), getVmMemStringB(), getVmMemStringC()];
    VmList { datas, nowRead: 0 }
  }
  #[test]
  fn test_VmList_clear() {
    let mut list = getVmList();
    list.nowRead = 1;
    list.clear();
    assert_eq!(list.datas, vec![]);
    assert_eq!(list.nowRead, 0);
  }
  #[test]
  fn test_VmList_push() {
    let mut list = getVmList();
    list.nowRead = 1;
    list.push(getVmMemStringA());
    assert_eq!(list.datas.len(), 4);
    assert_eq!(list.datas.last(), Some(&getVmMemStringA()));
    assert_eq!(list.nowRead, 1);
  }
  #[test]
  fn test_VmList_rewind() {
    let mut list = getVmList();
    list.nowRead = 2;
    list.rewind();
    assert_eq!(list.nowRead, 0);
    assert_eq!(list.datas, getVmList().datas);
  }
  #[test]
  fn test_VmList_read() {
    let mut list = getVmList();
    assert_eq!(list.read(), Some(getVmMemStringA()));
    assert_eq!(list.read(), Some(getVmMemStringB()));
    assert_eq!(list.read(), Some(getVmMemStringC()));
    assert_eq!(list.read(), None);
  }
}
