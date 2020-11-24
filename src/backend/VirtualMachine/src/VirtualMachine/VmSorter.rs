use super::VmMem::VmMemString;
use std::cmp::Ordering;
#[derive(Default, Clone)]
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
          let avalue = String::from_utf8_lossy(&akey[4..]);
          let bvalue = String::from_utf8_lossy(&bkey[4..]);
          match aflag {
            VmMemString::MEM_FLAG_INT => {
              let avalue: i128 = avalue
                .parse()
                .expect(format!("fail to parse: avalue={}", avalue).as_str());
              let bvalue: i128 = bvalue
                .parse()
                .expect(format!("fail to parse: avalue={}", avalue).as_str());
              if orders[index] {
                break bvalue.cmp(&avalue);
              } else {
                break avalue.cmp(&bvalue);
              }
            }
            VmMemString::MEM_FLAG_DOUBLE => {
              let avalue: f64 = avalue
                .parse()
                .expect(format!("fail to parse: avalue={}", avalue).as_str());
              let bvalue: f64 = bvalue
                .parse()
                .expect(format!("fail to parse: avalue={}", avalue).as_str());
              if orders[index] {
                break bvalue.partial_cmp(&avalue).expect(
                  format!("fail to compare: avalue={} bvalue={}", avalue, bvalue).as_str(),
                );
              } else {
                break avalue.partial_cmp(&bvalue).expect(
                  format!("fail to compare: avalue={} bvalue={}", avalue, bvalue).as_str(),
                );
              }
            }
            VmMemString::MEM_FLAG_STRING => {
              if orders[index] {
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
