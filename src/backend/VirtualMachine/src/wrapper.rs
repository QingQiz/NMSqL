pub mod rustLayer {
  cfg_if::cfg_if! {
    if #[cfg(test)] {
      use super::cLayer::mock as cLayer;
    } else {
      use super::cLayer;
    }
  }
  pub use super::cLayer::Cursor;
  pub use super::cLayer::CursorType;
  use std::ffi;
  pub const CURSOR_READ_ONLY: i32 = 1;
  pub const CURSOR_WRITE: i32 = 2;
  unsafe fn getLen(ptr: *const u8) -> usize {
    let mut len = 0;
    len += *ptr.offset(0) as usize;
    len <<= 8;
    len += *ptr.offset(1) as usize;
    len
  }
  unsafe fn getVecFromPtr(ptr: *const ffi::c_void) -> Vec<u8> {
    let mut ret = Vec::new();
    let ptr = ptr as *const u8;
    for i in 0..getLen(ptr) {
      ret.push(*ptr.offset(i as isize + 4));
    }
    ret
  }
  // open the index
  pub fn open(transactionId: i32, indexName: &Vec<u8>, flag: i32) -> *mut Cursor {
    unsafe { cLayer::open(transactionId, indexName.as_ptr() as *const i8, flag) }
  }
  // close the cursor
  pub fn close(transactionId: i32, cursor: *mut Cursor) -> i32 {
    unsafe { cLayer::close(transactionId, cursor) }
  }
  pub fn find(transactionId: i32, cursor: *mut Cursor, key: &Vec<u8>) -> i32 {
    unsafe { cLayer::find(transactionId, cursor, key.as_ptr() as *const ffi::c_void) }
  }
  // get the key of the data that current cursor points to
  pub fn getKey(transactionId: i32, cursor: *mut Cursor) -> Vec<u8> {
    unsafe { getVecFromPtr(cLayer::getKey(transactionId, cursor)) }
  }
  // get the value of the data that current cursor points to
  pub fn getValue(transactionId: i32, cursor: *mut Cursor) -> Vec<u8> {
    unsafe { getVecFromPtr(cLayer::getValue(transactionId, cursor)) }
  }
  // insert into Cursor with key and value
  pub fn insert(transactionId: i32, cursor: *mut Cursor, key: &Vec<u8>, value: &Vec<u8>) -> i32 {
    unsafe {
      cLayer::insert(
        transactionId,
        cursor,
        key.as_ptr() as *const ffi::c_void,
        value.as_ptr() as *const ffi::c_void,
      )
    }
  }
  // erase the element that cursor points to
  pub fn erase(transactionId: i32, cursor: *mut Cursor) -> i32 {
    unsafe { cLayer::erase(transactionId, cursor) }
  }
  // move the cursor to the next
  pub fn next(transactionId: i32, cursor: *mut Cursor) -> i32 {
    unsafe { cLayer::next(transactionId, cursor) }
  }
  // reset the cursor to the first
  pub fn reset(transactionId: i32, cursor: *mut Cursor) -> i32 {
    unsafe { cLayer::reset(transactionId, cursor) }
  }
  pub fn getAddress(transactionId: i32, cursor: *mut Cursor) -> i32 {
    unsafe { cLayer::getAddress(transactionId, cursor) }
  }
  pub fn createTable() -> i32 {
    unsafe { cLayer::createTable() }
  }
  pub fn createIndex() -> i32 {
    unsafe { cLayer::createIndex() }
  }
  pub fn clear(page: i32) -> i32 {
    unsafe { cLayer::clear(page) }
  }
  pub fn destroy(page: i32) -> i32 {
    unsafe { cLayer::destroy(page) }
  }
  pub fn reorganize() -> i32 {
    unsafe { cLayer::reorganize() }
  }
  pub fn getCookies() -> i32 {
    unsafe { cLayer::getCookies() }
  }
  pub fn setCookies(cookies: i32) {
    unsafe {
      cLayer::setCookies(cookies);
    }
  }
  pub fn transaction(transactionId: &mut i32) -> i32 {
    unsafe { cLayer::transaction(transactionId as *mut i32) }
  }
  pub fn commit(transactionId: i32) -> i32 {
    unsafe { cLayer::commit(transactionId) }
  }
  pub fn rollback(transactionId: i32) -> i32 {
    unsafe { cLayer::rollback(transactionId) }
  }
  pub type nmsql_callback = ::std::option::Option<
    unsafe extern "C" fn(
      arg1: *mut ::std::os::raw::c_void,
      arg2: ::std::os::raw::c_int,
      arg3: *const *const ::std::os::raw::c_char,
      arg4: *const *const ::std::os::raw::c_char,
    ) -> ::std::os::raw::c_int,
  >;
  #[no_mangle]
  extern "C" fn exec(
    ir: *mut ::std::os::raw::c_char,
    callback: nmsql_callback,
    args: *mut ::std::os::raw::c_void,
  ) -> ::std::os::raw::c_int {
    println!("in exec");
    let mut vm = crate::VirtualMachine::VirtualMachine::VirtualMachine::new();
    crate::VirtualMachine::VmRunner::runOperation(
      String::from_utf8_lossy(&{
        let mut ret = Vec::new();
        unsafe {
          for i in 0.. {
            let x = *ir.offset(i);
            if x == 0 {
              break;
            }
            ret.push(x as u8);
          }
        }
        ret
      })
      .to_string(),
      &mut vm,
      callback,
      args,
    );
    1
  }
}
pub mod cLayer {
  pub type CursorType = u32;
  pub type pgno_t = i32;
  #[repr(C)]
  #[derive(Debug, Copy, Clone)]
  pub struct Cursor {
    pub cursorType: CursorType,
    pub cursor: *mut ::std::os::raw::c_void,
  }

  use mockall::automock;

  #[automock(mod mock;)]
  extern "C" {
    pub fn open(
      transactionId: ::std::os::raw::c_int,
      indexName: *const ::std::os::raw::c_char,
      flag: ::std::os::raw::c_int,
    ) -> *mut Cursor;
    pub fn close(
      transactionId: ::std::os::raw::c_int,
      cursor: *mut Cursor,
    ) -> ::std::os::raw::c_int;
    pub fn create(
      dbTable: *const ::std::os::raw::c_char,
      indexName: *const ::std::os::raw::c_char,
      indexType: CursorType,
      indexColumnCnt: ::std::os::raw::c_int,
      indexColumns: *mut *const ::std::os::raw::c_char,
    ) -> ::std::os::raw::c_int;
    pub fn find(
      transactionId: ::std::os::raw::c_int,
      cursor: *mut Cursor,
      key: *const ::std::os::raw::c_void,
    ) -> ::std::os::raw::c_int;
    pub fn getKey(
      transactionId: ::std::os::raw::c_int,
      cursor: *mut Cursor,
    ) -> *mut ::std::os::raw::c_void;
    pub fn getValue(
      transactionId: ::std::os::raw::c_int,
      cursor: *mut Cursor,
    ) -> *mut ::std::os::raw::c_void;
    pub fn insert(
      transactionId: ::std::os::raw::c_int,
      cursor: *mut Cursor,
      key: *const ::std::os::raw::c_void,
      value: *const ::std::os::raw::c_void,
    ) -> ::std::os::raw::c_int;
    pub fn erase(
      transactionId: ::std::os::raw::c_int,
      cursor: *mut Cursor,
    ) -> ::std::os::raw::c_int;
    pub fn next(transactionId: ::std::os::raw::c_int, cursor: *mut Cursor)
      -> ::std::os::raw::c_int;
    pub fn reset(
      transactionId: ::std::os::raw::c_int,
      cursor: *mut Cursor,
    ) -> ::std::os::raw::c_int;
    pub fn getAddress(
      transactionId: ::std::os::raw::c_int,
      cursor: *mut Cursor,
    ) -> ::std::os::raw::c_int;
    pub fn createTable() -> pgno_t;
    pub fn createIndex() -> pgno_t;
    pub fn clear(page: pgno_t) -> i32;
    pub fn destroy(page: pgno_t) -> i32;
    pub fn reorganize() -> ::std::os::raw::c_int;
    pub fn getMetaData(tableName: *const ::std::os::raw::c_char) -> *mut ::std::os::raw::c_void;
    pub fn getCookies() -> ::std::os::raw::c_int;
    pub fn setCookies(cookies: ::std::os::raw::c_int) -> ::std::os::raw::c_int;
    pub fn getTableColumns(
      tableName: *const ::std::os::raw::c_char,
    ) -> *mut *mut ::std::os::raw::c_char;
    pub fn transaction(transactionId: *mut ::std::os::raw::c_int) -> ::std::os::raw::c_int;
    pub fn commit(transactionId: ::std::os::raw::c_int) -> ::std::os::raw::c_int;
    pub fn rollback(transactionId: ::std::os::raw::c_int) -> ::std::os::raw::c_int;
  }
}
#[cfg(test)]
mod CLayerTest {

  use super::super::VirtualMachine::VmMem::VmMem;
  use super::super::VirtualMachine::VmMem::VmMemString;
  use super::cLayer;
  #[test]
  fn ffi_work() {
    unsafe {
      cLayer::open(
        1,
        VmMem::genVmString(
          vec!['a' as u8, 'b' as u8, 'c' as u8],
          VmMemString::MEM_FLAG_INT,
        )
        .as_ptr() as *const i8,
        1,
      );
    }
  }
}
