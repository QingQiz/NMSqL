pub mod wrapper {
  pub mod rustLayer {
    pub const CURSOR_READ_ONLY: i32 = 1;
    pub const CURSOR_WRITE: i32 = 2;
    #[derive(Debug, Copy, Clone)]
    pub enum CursorType {
      CURSOR_BTREE,
      CURSOR_LIST,
    }
    #[derive(Debug, Copy, Clone)]
    pub struct Cursor {
      pub cursorType: CursorType,
    }
    // open the index
    pub fn open(indexName: &str, flag: i32) -> *mut Cursor {
      unimplemented!()
    }
    // close the cursor
    pub fn close(cursor: *mut Cursor) -> i32 {
      unimplemented!()
    }
    // create dbTable with indexName indexType indexColumnCnt and indexColumns
    pub fn create(
      dbTable: &str,
      indexName: &str,
      indexType: CursorType,
      indexColumnCnt: i32,
      indexColumns: &[&str],
    ) -> i32 {
      unimplemented!();
    }
    // get the key of the data that current cursor points to
    pub fn getKey(cursor: *mut Cursor) {
      unimplemented!()
    }
    // get the value of the data that current cursor points to
    pub fn getValue(cursor: *mut Cursor) {
      unimplemented!()
    }
    // insert into Cursor with key and value
    pub fn insert<T, U>(cursor: *mut Cursor, key: &T, value: &U) -> i32 {
      unimplemented!()
    }
    // erase the element that cursor points to
    pub fn erase(cursor: *mut Cursor) -> i32 {
      unimplemented!()
    }
    // move the cursor to the next
    pub fn next(cursor: *mut Cursor) -> i32 {
      unimplemented!()
    }
    // reset the cursor to the first
    pub fn reset(cursor: *mut Cursor) -> i32 {
      unimplemented!()
    }
    pub fn createTable(sql: &str) -> i32 {
      unimplemented!()
    }
    pub fn reorganize() -> i32 {
      unimplemented!()
    }
    pub fn getMetaData(tableName: &str) {
      unimplemented!()
    }
    pub fn getCookies() -> i32 {
      unimplemented!()
    }
    pub fn setCookies(cookies: i32) {
      unimplemented!()
    }
    pub fn getTableColumns(tableName: &str) -> &[&str] {
      unimplemented!()
    }
  }
  pub mod cLayer {
    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
  }
}
