use super::VmMem::{VmMem, VmMemString};
use crate::wrapper::rustLayer as DbWrapper;
use crate::wrapper::rustLayer::Cursor;

#[derive(Clone)]
pub struct VmCursor {
  cursor: *mut Cursor,
  key: VmMemString,
  isIdx: bool,
  keyAsData: bool,
  keyCache: VmMemString,
  valueCache: VmMemString,
  transactionId: i32,
  idleNext: bool,
}
impl Default for VmCursor {
  fn default() -> Self {
    VmCursor {
      cursor: 0 as *mut Cursor,
      key: VmMemString::default(),
      isIdx: false,
      keyAsData: false,
      keyCache: VmMemString::default(),
      valueCache: VmMemString::default(),
      transactionId: -1,
      idleNext: false,
    }
  }
}
impl VmCursor {
  pub fn new(cursorName: &String, flag: i32, transactionId: i32) -> Self {
    let mut ret = VmCursor {
      cursor: DbWrapper::open(
        transactionId,
        &{
          let mut ret = cursorName.as_bytes().to_vec();
          ret.push(0);
          ret
        },
        flag,
      ),
      key: VmMemString::default(),
      isIdx: false,
      keyAsData: false,
      keyCache: VmMemString::default(),
      valueCache: VmMemString::default(),
      transactionId,
      idleNext: false,
    };
    ret.rewind();
    ret
  }
  fn getCursor(self: &Self) -> Result<*mut Cursor, String> {
    Ok(self.cursor)
  }
  pub fn setIsIdx(self: &mut Self, flag: bool) {
    self.isIdx = flag;
  }
  pub fn rewind(self: &mut Self) -> Result<(), String> {
    DbWrapper::reset(self.transactionId, self.getCursor()?);
    self.clear();
    self.keyCache = VmMemString::new(DbWrapper::getKey(self.transactionId, self.cursor));
    self.valueCache = VmMemString::new(DbWrapper::getValue(self.transactionId, self.cursor));
    Ok(())
  }
  pub fn getKey(self: &Self) -> &VmMemString {
    &self.keyCache
  }
  pub fn getValue(self: &Self) -> &VmMemString {
    &self.valueCache
  }
  pub fn getAddress(self: &Self) -> i32 {
    DbWrapper::getAddress(self.transactionId, self.cursor)
  }
  /// return false if it's the end of cursor
  pub fn next(self: &mut Self) -> Result<(), String> {
    if self.idleNext {
      self.idleNext = false;
      return Ok(());
    }
    DbWrapper::next(self.transactionId, self.getCursor()?);
    self.keyCache = VmMemString::new(DbWrapper::getKey(self.transactionId, self.cursor));
    self.valueCache = VmMemString::new(DbWrapper::getValue(self.transactionId, self.cursor));
    Ok(())
  }
  pub fn clear(self: &mut Self) {
    self.keyAsData = false;
    self.isIdx = false;
    self.key = VmMemString::default();
    self.keyCache = VmMemString::default();
    self.valueCache = VmMemString::default();
  }
  pub fn close(self: &mut Self) -> Result<(), String> {
    DbWrapper::close(self.transactionId, self.getCursor()?);
    self.clear();
    Ok(())
  }
  pub fn insert(self: &mut Self, key: &VmMemString, value: &VmMemString) -> Result<(), String> {
    DbWrapper::insert(self.transactionId, self.getCursor()?, key, value);
    self.keyCache = key.clone();
    self.valueCache = value.clone();
    Ok(())
  }
  pub fn find(self: &mut Self, key: &VmMemString) -> Result<(), String> {
    DbWrapper::find(self.transactionId, self.getCursor()?, key);
    self.keyCache = VmMemString::new(DbWrapper::getKey(self.transactionId, self.cursor));
    self.valueCache = VmMemString::new(DbWrapper::getValue(self.transactionId, self.cursor));
    Ok(())
  }
  pub fn erase(self: &mut Self) -> Result<(), String> {
    self.idleNext = true;
    DbWrapper::erase(self.transactionId, self.getCursor()?);
    self.keyCache = VmMemString::new(DbWrapper::getKey(self.transactionId, self.cursor));
    self.valueCache = VmMemString::new(DbWrapper::getValue(self.transactionId, self.cursor));
    Ok(())
  }
  pub fn getKeyAsData(self: &Self) -> bool {
    self.keyAsData
  }
  pub fn setKeyAsData(self: &mut Self, flag: bool) {
    self.keyAsData = flag;
  }
  pub fn isEnd(self: &mut Self) -> bool {
    if self.valueCache.len() == 0 {
      true
    } else if self.isIdx {
      let mut keyCache = self.keyCache.iter();
      let mut key = self.key.iter();
      loop {
        let k = key.next();
        let c = keyCache.next();
        if let None = k {
          if let None = c {
            break false;
          }
          break true;
        } else if let None = c {
          break true;
        } else {
          let k = VmMem::MEM_STRING(VmMemString::new(k.unwrap().to_vec()));
          let c = VmMem::MEM_STRING(VmMemString::new(c.unwrap().to_vec()));
          if k != c {
            break true;
          }
        }
      }
    } else {
      false
    }
  }
  pub fn setIdx(self: &mut Self, key: &VmMemString) {
    self.isIdx = true;
    self.key = key.clone();
  }
}
