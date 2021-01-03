use super::VmMem::VmMemString;
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
    VmCursor {
      cursor: DbWrapper::open(transactionId, &cursorName.as_bytes().to_vec(), flag),
      key: VmMemString::default(),
      isIdx: false,
      keyAsData: false,
      keyCache: VmMemString::default(),
      valueCache: VmMemString::default(),
      transactionId,
      idleNext: false,
    }
  }
  fn getCursor(self: &Self) -> Result<*mut Cursor, String> {
    if self.cursor as usize == 0 {
      Err(String::from("cursor is not initialized"))
    } else {
      Ok(self.cursor)
    }
  }
  pub fn setIsIdx(self: &mut Self, flag: bool) {
    self.isIdx = flag;
  }
  pub fn rewind(self: &mut Self) -> Result<(), String> {
    DbWrapper::reset(self.transactionId, self.getCursor()?);
    self.clear();
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
    Ok(())
  }
  pub fn find(self: &mut Self, key: &VmMemString) -> Result<(), String> {
    DbWrapper::find(self.transactionId, self.getCursor()?, key);
    Ok(())
  }
  pub fn erase(self: &mut Self) -> Result<(), String> {
    self.idleNext = true;
    DbWrapper::erase(self.transactionId, self.getCursor()?);
    Ok(())
  }
  pub fn getKeyAsData(self: &Self) -> bool {
    self.keyAsData
  }
  pub fn setKeyAsData(self: &mut Self, flag: bool) {
    self.keyAsData = flag;
  }
  pub fn isEnd(self: &mut Self) -> bool {
    if self.keyCache.len() == 0 || self.isIdx && self.keyCache != self.key {
      true
    } else {
      false
    }
  }
  pub fn setIdx(self: &mut Self, key: &VmMemString) {
    self.isIdx = true;
    self.key = key.clone();
  }
}
