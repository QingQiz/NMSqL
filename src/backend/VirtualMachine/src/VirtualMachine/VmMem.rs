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
  pub fn getFlag(data: &[u8]) -> u8 {
    data[2]
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
      VmMem::MEM_INT(x) => {
        let mut ret: Vec<u8> = x.to_string().as_bytes().iter().map(|&x| x).collect();
        VmMem::genVmString(ret, VmMemString::MEM_FLAG_INT)
      }
      VmMem::MEM_DOUBLE(x) => {
        let mut ret: Vec<u8> = x.to_string().as_bytes().iter().map(|&x| x).collect();
        VmMem::genVmString(ret, VmMemString::MEM_FLAG_DOUBLE)
      }
      VmMem::MEM_NULL => VmMemString::new(vec![0, 0, VmMemString::MEM_FLAG_NULL, 0]),
      VmMem::MEM_STRING(x) => x,
    }
  }
  pub fn integerify(self: Self) -> i32 {
    match self {
      VmMem::MEM_INT(x) => x,
      VmMem::MEM_DOUBLE(x) => x as i32,
      VmMem::MEM_NULL => 0,
      VmMem::MEM_STRING(x) => String::from_utf8_lossy(&(x.as_slice()[4..]))
        .parse()
        .expect(
          format!(
            "expect a integer got {}",
            String::from_utf8_lossy(x.as_slice())
          )
          .as_str(),
        ),
    }
  }
  pub fn realify(self: Self) -> f64 {
    match self {
      VmMem::MEM_INT(x) => x as f64,
      VmMem::MEM_DOUBLE(x) => x,
      VmMem::MEM_NULL => 0.0,
      VmMem::MEM_STRING(x) => String::from_utf8_lossy(&(x.as_slice()[4..]))
        .parse()
        .expect(
          format!(
            "expect a number got {}",
            String::from_utf8_lossy(x.as_slice())
          )
          .as_str(),
        ),
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
  pub fn getStringRawLength(self: &Self) -> usize {
    if let VmMem::MEM_STRING(s) = self {
      let mut len = 0;
      for i in 0..2 {
        len = (len << 8) + (s[i] as usize);
      }
      len
    } else {
      0
    }
  }
  fn getLen(data: &Vec<u8>, lo: usize) -> Result<usize, String> {
    if VmMem::canGetData(data, lo) {
      Ok(((data[lo] as usize) << 8) + (data[lo + 1] as usize))
    } else {
      Err(String::from("too many columns"))
    }
  }
  fn canGetData(data: &Vec<u8>, lo: usize) -> bool {
    lo + 3 < data.len()
  }
  pub fn getColumn(data: Vec<u8>, num: usize) -> Result<Vec<u8>, String> {
    Ok(VmMem::getColumnSlice(&data, num)?.to_vec())
  }
  pub fn getColumnSlice(data: &Vec<u8>, num: usize) -> Result<&[u8], String> {
    let mut lo = 0;
    for i in 0..num {
      lo += VmMem::getLen(&data, lo)? + 4;
    }
    let len = VmMem::getLen(&data, lo)?;
    Ok(&data[lo..lo + 4 + len])
  }
}
