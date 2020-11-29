use super::VmMem::VmMemString;
use super::VmUtil::{vecU8ToF64, vecU8ToI32};
use std::cmp::Ordering;
#[derive(Default, Clone, PartialOrd, PartialEq, Eq)]
pub struct VmSorter {
  datas: Vec<Vec<VmMemString>>,
  // true:desc false:asc
  orders: Vec<bool>,
}

impl VmSorter {
  pub fn sort(self: &mut Self) {
    let orders = self.orders.clone();
    self.datas.sort_by(|a, b| {
      let mut ita = a[0].iter();
      let mut itb = b[0].iter();
      let mut index = 0;
      loop {
        let akey = ita.next();
        let bkey = itb.next();
        if let (&None, &None) = (&akey, &bkey) {
          break Ordering::Equal;
        }
        if let (Some(akey), Some(bkey)) = (akey, bkey) {
          if akey == bkey {
            index += 1;
            continue;
          }
          let aflag = VmMemString::getFlag(akey);
          let bflag = VmMemString::getFlag(bkey);
          if aflag != bflag {
            panic!("error in sort: aflag={:?} bflag={:?}", aflag, bflag);
          }
          match aflag {
            Some(VmMemString::MEM_FLAG_INT) => {
              let avalue: i32 = vecU8ToI32(&akey[4..]);
              let bvalue: i32 = vecU8ToI32(&bkey[4..]);
              if index < orders.len() && orders[index] {
                break bvalue.cmp(&avalue);
              } else {
                break avalue.cmp(&bvalue);
              }
            }
            Some(VmMemString::MEM_FLAG_DOUBLE) => {
              let avalue: f64 = vecU8ToF64(&akey[4..]);
              let bvalue: f64 = vecU8ToF64(&bkey[4..]);
              if index < orders.len() && orders[index] {
                break bvalue.partial_cmp(&avalue).expect(
                  format!("fail to compare: avalue={} bvalue={}", avalue, bvalue).as_str(),
                );
              } else {
                break avalue.partial_cmp(&bvalue).expect(
                  format!("fail to compare: avalue={} bvalue={}", avalue, bvalue).as_str(),
                );
              }
            }
            Some(VmMemString::MEM_FLAG_STRING) => {
              let avalue = String::from_utf8_lossy(&akey[4..]);
              let bvalue = String::from_utf8_lossy(&bkey[4..]);
              if index < orders.len() && orders[index] {
                break bvalue.cmp(&avalue);
              } else {
                break avalue.cmp(&bvalue);
              }
            }
            _ => {
              index += 1;
              continue;
            }
          }
        } else {
          panic!("error in sort: akey={:?} bkey={:?}", akey, bkey);
        }
      }
    });
    self.datas.reverse();
  }
  pub fn push(self: &mut Self, key: VmMemString, value: VmMemString) {
    self.datas.push(vec![key, value]);
  }
  pub fn setDesc(self: &mut Self, index: usize) {
    if index >= self.orders.len() {
      self.orders.resize(index + 1, false);
    }
    self.orders[index] = true;
  }
  pub fn clear(self: &mut Self) {
    self.datas.clear();
    self.orders.clear();
  }
  pub fn pop(self: &mut Self) -> Option<VmMemString> {
    match self.datas.len() {
      0 => None,
      _ => Some(self.datas.pop().unwrap().pop().unwrap()),
    }
  }
  pub fn topKey(self: &mut Self) -> Option<VmMemString> {
    match self.datas.len() {
      0 => None,
      _ => Some(self.datas.last().unwrap()[0].clone()),
    }
  }
}

#[cfg(test)]
mod VmSorterTest {
  use super::super::VmMem::VmMemString;
  use super::super::VmUtil::VmTestUtils::*;
  use super::VmSorter;
  use rand::seq::SliceRandom;
  #[test]
  fn test_VmSorter_push_work() {
    let mut toTest = VmSorter::default();
    toTest.push(
      getManyVmMemString(vec![getVmMemStringI32(1)]),
      getManyVmMemString(vec![getVmMemStringI32(1)]),
    );
    assert_eq!(toTest.datas.len(), 1);
    assert_eq!(
      toTest.datas.pop(),
      Some(vec![
        getManyVmMemString(vec![getVmMemStringI32(1)]),
        getManyVmMemString(vec![getVmMemStringI32(1)]),
      ])
    );
  }
  #[test]
  fn test_VmSorter_setDesc_work() {
    let mut toTest = VmSorter::default();
    toTest.setDesc(0);
    assert_eq!(toTest.orders, vec![true]);
    toTest.setDesc(2);
    assert_eq!(toTest.orders, vec![true, false, true]);
  }
  #[test]
  fn test_VmSorter_clear_work() {
    let mut toTest = VmSorter::default();
    toTest.push(getVmMemStringI32(1), getVmMemStringI32(1));
    toTest.push(getVmMemStringI32(2), getVmMemStringI32(2));
    toTest.setDesc(1);
    assert_eq!(toTest.datas.len(), 2);
    assert_eq!(toTest.orders.len(), 2);
    toTest.clear();
    assert_eq!(toTest.datas.len(), 0);
    assert_eq!(toTest.orders.len(), 0);
  }
  #[test]
  fn test_VmSorter_pop_work() {
    let mut toTest = VmSorter::default();
    toTest.push(getVmMemStringI32(1), getVmMemStringI32(3));
    toTest.push(getVmMemStringI32(2), getVmMemStringI32(4));
    assert_eq!(toTest.pop(), Some(getVmMemStringI32(4)));
    assert_eq!(toTest.pop(), Some(getVmMemStringI32(3)));
    assert_eq!(toTest.pop(), None);
  }
  #[test]
  fn test_VmSorter_topKey_work() {
    let mut toTest = VmSorter::default();
    toTest.push(getVmMemStringI32(1), getVmMemStringI32(3));
    toTest.push(getVmMemStringI32(2), getVmMemStringI32(4));
    assert_eq!(toTest.topKey(), Some(getVmMemStringI32(2)));
    toTest.pop();
    assert_eq!(toTest.topKey(), Some(getVmMemStringI32(1)));
    toTest.pop();
    assert_eq!(toTest.topKey(), None);
  }
  #[test]
  #[should_panic(expected = "error in sort: aflag=")]
  fn test_VmSorter_sort_diffType() {
    let mut toTest = VmSorter::default();
    toTest.push(getVmMemStringI32(1), getVmMemStringI32(3));
    toTest.push(getVmMemStringF64(2.0), getVmMemStringI32(4));
    toTest.sort();
  }
  fn getToSort() -> VmSorter {
    let mut toTest = VmSorter::default();
    toTest.push(
      getManyVmMemString(vec![
        getVmMemStringI32(1),
        getVmMemStringF64(-1.0),
        getVmMemStringStringA(100),
      ]),
      getVmMemStringI32(0),
    );
    toTest.push(
      getManyVmMemString(vec![
        getVmMemStringI32(1),
        getVmMemStringF64(-1.0),
        getVmMemStringStringA(100),
      ]),
      getVmMemStringI32(1),
    );
    toTest.push(
      getManyVmMemString(vec![
        getVmMemStringI32(1),
        getVmMemStringF64(-1.0),
        getVmMemStringStringA(200),
      ]),
      getVmMemStringI32(2),
    );
    toTest.push(
      getManyVmMemString(vec![
        getVmMemStringI32(1),
        getVmMemStringF64(-1.0),
        getVmMemStringStringB(1),
      ]),
      getVmMemStringI32(3),
    );
    toTest.push(
      getManyVmMemString(vec![
        getVmMemStringI32(1),
        getVmMemStringF64(0.0),
        getVmMemStringStringA(200),
      ]),
      getVmMemStringI32(4),
    );
    toTest.push(
      getManyVmMemString(vec![
        getVmMemStringI32(1),
        getVmMemStringF64(2.0),
        getVmMemStringStringA(200),
      ]),
      getVmMemStringI32(5),
    );
    toTest.push(
      getManyVmMemString(vec![
        getVmMemStringI32(2),
        getVmMemStringF64(-1.0),
        getVmMemStringStringA(100),
      ]),
      getVmMemStringI32(6),
    );
    toTest.push(
      getManyVmMemString(vec![
        getVmMemStringI32(2),
        getVmMemStringF64(-1.0),
        getVmMemStringStringA(200),
      ]),
      getVmMemStringI32(7),
    );
    toTest.push(
      getManyVmMemString(vec![
        getVmMemStringI32(2),
        getVmMemStringF64(-1.0),
        getVmMemStringStringB(1),
      ]),
      getVmMemStringI32(8),
    );
    toTest.push(
      getManyVmMemString(vec![
        getVmMemStringI32(2),
        getVmMemStringF64(0.0),
        getVmMemStringStringA(200),
      ]),
      getVmMemStringI32(9),
    );
    toTest.push(
      getManyVmMemString(vec![
        getVmMemStringI32(2),
        getVmMemStringF64(2.0),
        getVmMemStringStringA(200),
      ]),
      getVmMemStringI32(10),
    );
    toTest.push(
      getManyVmMemString(vec![
        getVmMemStringI32(3),
        getVmMemStringF64(-1.0),
        getVmMemStringStringA(100),
      ]),
      getVmMemStringI32(11),
    );
    toTest.push(
      getManyVmMemString(vec![
        getVmMemStringI32(3),
        getVmMemStringF64(-1.0),
        getVmMemStringStringA(200),
      ]),
      getVmMemStringI32(12),
    );
    toTest.push(
      getManyVmMemString(vec![
        getVmMemStringI32(3),
        getVmMemStringF64(-1.0),
        getVmMemStringStringB(1),
      ]),
      getVmMemStringI32(13),
    );
    toTest.push(
      getManyVmMemString(vec![
        getVmMemStringI32(3),
        getVmMemStringF64(0.0),
        getVmMemStringStringA(200),
      ]),
      getVmMemStringI32(14),
    );
    toTest.push(
      getManyVmMemString(vec![
        getVmMemStringI32(3),
        getVmMemStringF64(2.0),
        getVmMemStringStringA(200),
      ]),
      getVmMemStringI32(15),
    );
    toTest
  }
  #[test]
  fn test_VmSorter_sort_work_noDesc() {
    let mut toTest = getToSort();
    let mut rng = rand::thread_rng();
    toTest.datas.shuffle(&mut rng);
    toTest.sort();
    let mut num = 0;
    while let Some(x) = toTest.topKey() {
      if num == 1 || num == 0 {
        let tmp = toTest.pop();
        assert!(tmp == Some(getVmMemStringI32(1)) || tmp == Some(getVmMemStringI32(0)));
      } else {
        assert_eq!(toTest.pop(), Some(getVmMemStringI32(num)));
      }
      num += 1;
    }
    assert_eq!(num, 16);
  }
  #[test]
  fn test_VmSorter_sort_work_withDesc() {
    let mut toTest = VmSorter::default();
    toTest.push(
      getManyVmMemString(vec![getVmMemStringI32(1), getVmMemStringI32(2)]),
      getVmMemStringI32(1),
    );
    toTest.push(
      getManyVmMemString(vec![getVmMemStringI32(1), getVmMemStringI32(1)]),
      getVmMemStringI32(2),
    );
    toTest.push(
      getManyVmMemString(vec![getVmMemStringI32(2), getVmMemStringI32(2)]),
      getVmMemStringI32(3),
    );
    toTest.push(
      getManyVmMemString(vec![getVmMemStringI32(2), getVmMemStringI32(1)]),
      getVmMemStringI32(4),
    );
    toTest.setDesc(1);
    toTest.sort();
    assert_eq!(toTest.pop(), Some(getVmMemStringI32(1)));
    assert_eq!(toTest.pop(), Some(getVmMemStringI32(2)));
    assert_eq!(toTest.pop(), Some(getVmMemStringI32(3)));
    assert_eq!(toTest.pop(), Some(getVmMemStringI32(4)));
    assert_eq!(toTest.pop(), None);
  }
  #[test]
  #[should_panic(expected = "error in sort: akey=")]
  fn test_VmSorter_sort_diffLen() {
    let mut toTest = VmSorter::default();
    toTest.push(
      getManyVmMemString(vec![getVmMemStringI32(1), getVmMemStringI32(2)]),
      getVmMemStringI32(1),
    );
    toTest.push(
      getManyVmMemString(vec![getVmMemStringI32(1)]),
      getVmMemStringI32(1),
    );
    toTest.sort();
  }
}
