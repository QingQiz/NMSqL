use super::VmUtil::{f64ToVecU8, i32ToVecU8, vecU8ToF64, vecU8ToI32};

/// string in format of [len(2),flag(1),null(1),string(len)]  
#[derive(Debug, Clone, PartialEq, PartialOrd, Default, Eq, Hash)]
pub struct VmMemString {
  pub string: Vec<u8>,
}

impl VmMemString {
  pub const MEM_FLAG_INT: u8 = 1;
  pub const MEM_FLAG_DOUBLE: u8 = 2;
  pub const MEM_FLAG_STRING: u8 = 3;
  pub const MEM_FLAG_NULL: u8 = 4;
  pub fn new(string: Vec<u8>) -> Self {
    VmMemString { string }
  }
  pub fn removeHead(self: Self) -> Vec<u8> {
    self[4..].to_vec()
  }
  pub fn iter(self: &Self) -> VmMemStringIterator {
    VmMemStringIterator::new(self)
  }
  fn canGetData(self: &Self, lo: usize) -> bool {
    lo + 3 < self.len()
  }
  pub fn getLen(self: &Self, lo: usize) -> Option<usize> {
    if self.canGetData(lo) {
      Some((self[lo] as usize) << 8 | (self[lo + 1] as usize))
    } else {
      None
    }
  }
  pub fn getFlag(data: &[u8]) -> Option<u8> {
    if data.len() >= 4 {
      Some(data[2])
    } else {
      None
    }
  }
}

impl std::ops::Deref for VmMemString {
  type Target = Vec<u8>;
  fn deref(&self) -> &Self::Target {
    &self.string
  }
}

pub struct VmMemStringIterator<'a> {
  data: &'a VmMemString,
  lo: usize,
}

impl<'a> VmMemStringIterator<'a> {
  fn new(data: &'a VmMemString) -> Self {
    VmMemStringIterator { data, lo: 0 }
  }
}

impl<'a> Iterator for VmMemStringIterator<'a> {
  type Item = &'a [u8];
  fn next(&mut self) -> Option<Self::Item> {
    if_chain!(
        if let Some(len) = self.data.getLen(self.lo);
        if self.lo+3+len < self.data.len();
        then {
            let rawLo = self.lo;
            self.lo+=4+len;
            Some(&self.data[rawLo..self.lo])
        }
        else {
            None
        }
    )
  }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum VmMem {
  MEM_INT(i32),
  MEM_DOUBLE(f64),
  MEM_STRING(VmMemString),
  MEM_NULL,
}
impl VmMem {
  /// turn value into string and make it like C string
  pub fn stringify(self: Self) -> VmMemString {
    match self {
      VmMem::MEM_INT(x) => VmMem::genVmString(i32ToVecU8(x), VmMemString::MEM_FLAG_INT),
      VmMem::MEM_DOUBLE(x) => VmMem::genVmString(f64ToVecU8(x), VmMemString::MEM_FLAG_DOUBLE),
      VmMem::MEM_NULL => VmMemString::new(vec![0, 0, VmMemString::MEM_FLAG_NULL, 0]),
      VmMem::MEM_STRING(x) => x,
    }
  }
  pub fn integerify(self: Self) -> i32 {
    match self {
      VmMem::MEM_INT(x) => x,
      VmMem::MEM_DOUBLE(x) => x as i32,
      VmMem::MEM_NULL => 0,
      VmMem::MEM_STRING(x) => match VmMemString::getFlag(&*x) {
        Some(VmMemString::MEM_FLAG_NULL) => 0,
        Some(VmMemString::MEM_FLAG_INT) => vecU8ToI32(&x.removeHead()),
        Some(VmMemString::MEM_FLAG_DOUBLE) => vecU8ToF64(&x.removeHead()) as i32,
        _ => unreachable!(),
      },
    }
  }
  pub fn realify(self: Self) -> f64 {
    match self {
      VmMem::MEM_INT(x) => x as f64,
      VmMem::MEM_DOUBLE(x) => x,
      VmMem::MEM_NULL => 0.0,
      VmMem::MEM_STRING(x) => match VmMemString::getFlag(&*x) {
        Some(VmMemString::MEM_FLAG_NULL) => 0 as f64,
        Some(VmMemString::MEM_FLAG_INT) => vecU8ToI32(&x.removeHead()) as f64,
        Some(VmMemString::MEM_FLAG_DOUBLE) => vecU8ToF64(&x.removeHead()),
        _ => unreachable!(),
      },
    }
  }
  pub fn genVmString(string: Vec<u8>, flag: u8) -> VmMemString {
    let len = string.len();
    VmMemString::new(
      [
        vec![((len >> 8) & 0xff) as u8, ((len) & 0xff) as u8],
        vec![flag, 0],
        string,
      ]
      .concat(),
    )
  }
}

#[cfg(test)]
mod VmMemTest {
  use super::super::VmUtil::VmTestUtils::*;
  use super::super::VmUtil::*;
  use super::VmMem;
  use super::VmMemString;

  fn getI32OneVmMemString() -> VmMemString {
    getVmMemStringI32(1)
  }

  fn getF64OneVmMemString() -> VmMemString {
    getVmMemStringF64(1.0)
  }

  fn getLongStringVmMemString() -> VmMemString {
    getVmMemStringString(500)
  }

  #[test]
  fn test_VmMem_genVmString_work() {
    let i32Vec = vec![0u8, 0u8, 0u8, 1u8];
    let i32Result = getI32OneVmMemString();
    assert_eq!(
      i32Result,
      VmMem::genVmString(i32Vec, VmMemString::MEM_FLAG_INT)
    );
    let f64Vec = f64ToVecU8(1.0);
    let f64Result = getF64OneVmMemString();
    assert_eq!(
      f64Result,
      VmMem::genVmString(f64Vec, VmMemString::MEM_FLAG_DOUBLE)
    );
    let stringVec = vec!['a' as u8; 500];
    let stringResult = getLongStringVmMemString();
    assert_eq!(
      stringResult,
      VmMem::genVmString(stringVec, VmMemString::MEM_FLAG_STRING)
    );
  }
  #[test]
  fn test_VmMem_Stringify_work() {
    assert_eq!(VmMem::MEM_INT(1).stringify(), getI32OneVmMemString());
    assert_eq!(VmMem::MEM_DOUBLE(1.0).stringify(), getF64OneVmMemString());
    assert_eq!(
      getLongStringVmMemString(),
      VmMem::MEM_STRING(getLongStringVmMemString()).stringify()
    );
    assert_eq!(
      VmMem::MEM_NULL.stringify(),
      VmMemString::new(vec![0u8, 0, VmMemString::MEM_FLAG_NULL, 0])
    );
  }
  #[test]
  fn test_VmMem_Integerify_work() {
    assert_eq!(VmMem::MEM_INT(1).integerify(), 1);
    assert_eq!(VmMem::MEM_DOUBLE(1.1).integerify(), 1);
    assert_eq!(VmMem::MEM_NULL.integerify(), 0);
    assert_eq!(
      VmMem::MEM_STRING(VmMem::MEM_INT(1).stringify()).integerify(),
      1
    );
    assert_eq!(
      VmMem::MEM_STRING(VmMem::MEM_DOUBLE(1.1).stringify()).integerify(),
      1
    );
    assert_eq!(
      VmMem::MEM_STRING(VmMem::MEM_NULL.stringify()).integerify(),
      0
    );
  }
  #[test]
  #[should_panic]
  fn test_VmMem_Integerify_fail() {
    VmMem::MEM_STRING(getLongStringVmMemString()).integerify();
  }
  #[test]
  fn test_VmMem_Realify_work() {
    assert_eq!(VmMem::MEM_INT(1).realify(), 1.0);
    assert_eq!(VmMem::MEM_DOUBLE(1.1).realify(), 1.1);
    assert_eq!(VmMem::MEM_NULL.realify(), 0.0);
    assert_eq!(
      VmMem::MEM_STRING(VmMem::MEM_INT(1).stringify()).realify(),
      1.0
    );
    assert_eq!(
      VmMem::MEM_STRING(VmMem::MEM_DOUBLE(1.1).stringify()).realify(),
      1.1
    );
    assert_eq!(
      VmMem::MEM_STRING(VmMem::MEM_NULL.stringify()).realify(),
      0.0
    );
  }
  #[test]
  #[should_panic]
  fn test_VmMem_Realify_fail() {
    VmMem::MEM_STRING(getLongStringVmMemString()).realify();
  }
}

#[cfg(test)]
mod VmMemStringTest {
  use super::super::VmUtil::*;
  use super::VmMem;
  use super::VmMemString;
  use super::VmMemStringIterator;
  #[test]
  fn test_VmMemString_removeHead_work() {
    let toTest = VmMemString::new(vec![0u8, 1u8, VmMemString::MEM_FLAG_STRING, 0, 'a' as u8]);
    assert_eq!(toTest.removeHead(), vec!['a' as u8]);
    let toTest = VmMemString::new(vec![0u8, 0u8, VmMemString::MEM_FLAG_STRING, 0]);
    assert_eq!(toTest.removeHead(), vec![]);
  }
  #[test]
  fn test_VmMemString_canGetData_work() {
    let toTest = VmMemString::new(vec![0u8, 0u8, 0u8]);
    assert!(!toTest.canGetData(0));
    let toTest = VmMemString::new(vec![0u8, 0u8, 0u8, 0u8]);
    assert!(toTest.canGetData(0));
  }
  #[test]
  fn test_VmMemString_getLen_work() {
    let toTest = VmMemString::new(vec![0u8, 0u8, 0u8]);
    assert_eq!(toTest.getLen(0), None);
    let toTest = VmMemString::new(
      [
        vec![2u8, 1u8, VmMemString::MEM_FLAG_STRING, 0u8],
        vec!['a' as u8; 0x201],
      ]
      .concat(),
    );
    assert_eq!(toTest.getLen(0), Some(0x201));
  }
  #[test]
  fn test_VmMemString_getFlag_work() {
    let toTest = VmMemString::new(vec![0u8, 0, 0]);
    assert_eq!(VmMemString::getFlag(&toTest), None);
    let toTest = VmMemString::new(vec![0u8, 0, VmMemString::MEM_FLAG_STRING, 0]);
    assert_eq!(
      VmMemString::getFlag(&toTest),
      Some(VmMemString::MEM_FLAG_STRING)
    );
    let toTest = VmMemString::new(vec![0u8, 4, VmMemString::MEM_FLAG_INT, 0, 0, 0, 0, 0]);
    assert_eq!(
      VmMemString::getFlag(&toTest),
      Some(VmMemString::MEM_FLAG_INT)
    );
    let toTest = VmMemString::new(
      [
        vec![0u8, 8, VmMemString::MEM_FLAG_DOUBLE, 0],
        f64ToVecU8(1.0),
      ]
      .concat(),
    );
    assert_eq!(
      VmMemString::getFlag(&toTest),
      Some(VmMemString::MEM_FLAG_DOUBLE)
    );
    let toTest = VmMemString::new(vec![0u8, 0u8, VmMemString::MEM_FLAG_NULL, 0]);
    assert_eq!(
      VmMemString::getFlag(&toTest),
      Some(VmMemString::MEM_FLAG_NULL)
    );
  }
  #[test]
  fn test_VmMemStringIterator_wort() {
    let toTest = VmMemString::new(
      [
        vec![0u8, 0u8, VmMemString::MEM_FLAG_NULL, 0],
        vec![0u8, 4u8, VmMemString::MEM_FLAG_INT, 0],
        i32ToVecU8(0x12345678),
        vec![0u8, 8u8, VmMemString::MEM_FLAG_DOUBLE, 0],
        f64ToVecU8(1.0),
        vec![0u8, 255, VmMemString::MEM_FLAG_STRING, 0],
        vec!['a' as u8; 255],
      ]
      .concat(),
    );
    let mut toTestIter = toTest.iter();
    assert_eq!(
      toTestIter.next(),
      Some(vec![0u8, 0u8, VmMemString::MEM_FLAG_NULL, 0].as_slice()),
    );
    assert_eq!(
      toTestIter.next(),
      Some(
        [
          vec![0u8, 4u8, VmMemString::MEM_FLAG_INT, 0],
          i32ToVecU8(0x12345678),
        ]
        .concat()
        .as_slice()
      ),
    );
    assert_eq!(
      toTestIter.next(),
      Some(
        [
          vec![0u8, 8u8, VmMemString::MEM_FLAG_DOUBLE, 0],
          f64ToVecU8(1.0),
        ]
        .concat()
        .as_slice()
      ),
    );
    assert_eq!(
      toTestIter.next(),
      Some(
        [
          vec![0u8, 255, VmMemString::MEM_FLAG_STRING, 0],
          vec!['a' as u8; 255],
        ]
        .concat()
        .as_slice()
      ),
    );
    assert_eq!(toTestIter.next(), None);
  }
}
