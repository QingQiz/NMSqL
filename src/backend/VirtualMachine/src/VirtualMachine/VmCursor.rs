use super::VmMem::VmMemString;
use crate::wrapper::rustLayer::Cursor;

#[derive(Clone)]
pub struct VmCursor {
  pub cursor: *mut Cursor,
  pub key: VmMemString,
  pub isIdx: bool,
  pub keyAsData: bool,
}
impl Default for VmCursor {
  fn default() -> Self {
    VmCursor {
      cursor: 0 as *mut Cursor,
      key: VmMemString::default(),
      isIdx: false,
      keyAsData: false,
    }
  }
}
impl VmCursor {
  pub fn new(cursor: *mut Cursor) -> Self {
    VmCursor {
      cursor: cursor,
      key: VmMemString::default(),
      isIdx: false,
      keyAsData: false,
    }
  }
}
