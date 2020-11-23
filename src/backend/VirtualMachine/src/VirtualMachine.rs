pub mod VirtualMachine {
  use crate::wrapper::wrapper::rustLayer as DbWrapper;
  use crate::wrapper::wrapper::rustLayer::Cursor;
  use std::cmp::Ordering;
  #[derive(Debug, Copy, Clone, FromPrimitive)]
  enum VmOpType {
    OP_Transaction,
    OP_Commit,
    OP_Rollback,
    OP_ReadCookie,
    OP_SetCookie,
    OP_VerifyCookie,
    OP_Open,
    OP_OpenTemp,
    OP_OpenWrite,
    OP_Close,
    OP_MoveTo,
    OP_Fcnt,
    OP_NewRecno,
    OP_Put,
    OP_Distinct,
    OP_Found,
    OP_NotFound,
    OP_Delete,
    OP_Column,
    OP_KeyAsData,
    OP_Recno,
    OP_FullKey,
    OP_Rewind,
    OP_Next,
    OP_Destroy,
    OP_Clear,
    OP_CreateIndex,
    OP_CreateTable,
    OP_Reorganize,
    OP_BeginIdx,
    OP_NextIdx,
    OP_PutIdx,
    OP_DeleteIdx,
    OP_MemLoad,
    OP_MemStore,
    OP_ListOpen,
    OP_ListWrite,
    OP_ListRewind,
    OP_ListRead,
    OP_ListClose,
    OP_SortOpen,
    OP_SortPut,
    OP_SortMakeRec,
    OP_SortMakeKey,
    OP_Sort,
    OP_SortNext,
    OP_SortKey,
    OP_SortCallback,
    OP_SortClose,
    OP_FileOpen,
    OP_FileRead,
    OP_FileColumn,
    OP_FileClose,
    OP_AggReset,
    OP_AggFocus,
    OP_AggIncr,
    OP_AggNext,
    OP_AggSet,
    OP_AggGet,
    OP_SetInsert,
    OP_SetFound,
    OP_SetNotFound,
    OP_SetClear,
    OP_MakeRecord,
    OP_MakeKey,
    OP_MakeIdxKey,
    OP_Goto,
    OP_JIf,
    OP_Halt,
    OP_ColumnCount,
    OP_ColumnName,
    OP_Callback,
    OP_Integer,
    OP_String,
    OP_Null,
    OP_Pop,
    OP_Dup,
    OP_Pull,
    OP_Add,
    OP_AddImm,
    OP_Subtract,
    OP_Multiply,
    OP_Divide,
    OP_Min,
    OP_Max,
    OP_JLike,
    OP_JGlob,
    OP_JEq,
    OP_JNe,
    OP_JLt,
    OP_JLe,
    OP_JGt,
    OP_JGe,
    OP_JIsNull,
    OP_JNotNull,
    OP_Negative,
    OP_And,
    OP_Or,
    OP_Not,
    OP_Concat,
    OP_Noop,
    OP_Strlen,
    OP_Substr,
    OP_SetIf,
    OP_SetLike,
    OP_SetGlob,
    OP_SetEq,
    OP_SetNe,
    OP_SetLt,
    OP_SetLe,
    OP_SetGt,
    OP_SetGe,
    OP_SetIsNull,
    OP_SetNotNull,
    OP_SortSetDesc,
  }

  #[derive(Debug, Clone)]
  struct VmOp {
    lineNo: i32,
    vmOpType: VmOpType,
    p1: i32,
    p2: i32,
    p3: String,
  }

  impl VmOp {
    /// sql format "lineNo|VmOpType|p1|p2|p3"
    pub fn new(rawString: &str) -> Self {
      let vmOpParts: Vec<&str> = rawString.splitn(5, "|").collect();
      if vmOpParts.len() != 5 {
        panic!(
          "the number of '|' in OperationString {} is less than 4",
          rawString
        );
      }
      VmOp {
        lineNo: vmOpParts[0].parse().expect(
          format!(
            "line number should be a interger, but found {}",
            vmOpParts[0]
          )
          .as_str(),
        ),
        vmOpType: num::FromPrimitive::from_i32(
          vmOpParts[1]
            .parse()
            .expect(format!("VmOpType expects a integer reads {}", vmOpParts[1]).as_str()),
        )
        .expect(format!("unknown operation: {}", rawString).as_str()),
        p1: vmOpParts[2]
          .parse()
          .expect(format!("p1 expects a integer reads {}", vmOpParts[2]).as_str()),
        p2: vmOpParts[3]
          .parse()
          .expect(format!("p1 expects a integer reads {}", vmOpParts[3]).as_str()),
        p3: String::from(vmOpParts[4]),
      }
    }
  }

  #[derive(Clone)]
  struct VmCursor {
    cursor: *mut Cursor,
    key: VmMemString,
    isIdx: bool,
    keyAsData: bool,
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
    fn new(cursor: *mut Cursor) -> Self {
      VmCursor {
        cursor: cursor,
        key: VmMemString::default(),
        isIdx: false,
        keyAsData: false,
      }
    }
  }

  /// string in format of [len(2),flag(1),null(1),string(len)]  
  #[derive(Debug, Clone, PartialEq, PartialOrd, Default)]
  struct VmMemString {
    string: Vec<u8>,
  }

  impl VmMemString {
    const MEM_FLAG_INT: u8 = 1;
    const MEM_FLAG_DOUBLE: u8 = 2;
    const MEM_FLAG_STRING: u8 = 3;
    const MEM_FLAG_NULL: u8 = 4;
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

  struct VmMemStringIterator<'a> {
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
  enum VmMem {
    MEM_INT(i128),
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
    pub fn integerify(self: Self) -> i128 {
      match self {
        VmMem::MEM_INT(x) => x,
        VmMem::MEM_DOUBLE(x) => x as i128,
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

  #[derive(Default, Clone)]
  struct VmList {
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

  #[derive(Default, Clone)]
  struct VmSorter {
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

  #[derive(Default)]
  struct VirtualMachine {
    vmOps: Vec<VmOp>,
    vmCursors: Vec<VmCursor>,
    stack: Vec<VmMem>,
    resultColumnNames: Vec<String>,
    fcnt: i32,
    lists: Vec<VmList>,
    sorters: Vec<VmSorter>,
  }

  /// method for stack
  impl VirtualMachine {
    pub fn new() -> Self {
      VirtualMachine::default()
    }
    /// return the poped values from top to bottom
    pub fn popStack(self: &mut Self, n: i32) -> Result<Vec<VmMem>, String> {
      if self.stack.len() < n as usize {
        Err(format!(
          "stack size: {}, pop {} element(s)",
          self.stack.len(),
          n
        ))
      } else {
        let newSize = self.stack.len() - n as usize;

        Ok({
          let mut ret = Vec::new();
          for i in 0..n {
            ret.push(self.stack.pop().unwrap());
          }
          ret
        })
      }
    }
    pub fn pushStack(self: &mut Self, value: VmMem) {
      self.stack.push(value);
    }
  }

  /// method for cursor
  impl VirtualMachine {
    pub fn setCursor(self: &mut Self, num: usize, cursor: VmCursor) {
      if (self.vmCursors.len() <= num) {
        self.vmCursors.resize(num + 1, VmCursor::default());
      }
      self.closeCursor(num);
      self.vmCursors[num] = cursor;
    }
    pub fn getCursor(self: &mut Self, num: usize) -> Result<&mut VmCursor, String> {
      if num < self.vmCursors.len() {
        Ok(&mut self.vmCursors[num])
      } else {
        Err(format!("no cursor numbered {}", num))
      }
    }
    pub fn closeCursor(self: &Self, num: usize) {
      if num < self.vmCursors.len() && self.vmCursors[num].cursor as usize != 0 {
        DbWrapper::close(self.vmCursors[num].cursor);
      }
    }
  }

  /// method for list
  impl VirtualMachine {
    pub fn openList(self: &mut Self, num: usize) {
      if num <= self.lists.len() {
        self.lists.resize(num + 1, VmList::default());
      }
      self.lists[num].clear();
    }
    fn getList(self: &mut Self, num: usize) -> Result<&mut VmList, String> {
      if num >= self.lists.len() {
        Err(format!(
          "list out of bounds with len={} index={}",
          self.lists.len(),
          num
        ))
      } else {
        Ok(&mut self.lists[num])
      }
    }
    pub fn writeList(self: &mut Self, num: usize, data: VmMemString) -> Result<(), String> {
      self.getList(num)?.push(data);
      Ok(())
    }
    pub fn rewindList(self: &mut Self, num: usize) -> Result<(), String> {
      self.getList(num)?.rewind();
      Ok(())
    }
    pub fn readList(self: &mut Self, num: usize) -> Result<Option<VmMemString>, String> {
      Ok(self.getList(num)?.read())
    }
    pub fn closeList(self: &mut Self, num: usize) -> Result<(), String> {
      self.getList(num)?.clear();
      Ok(())
    }
  }

  /// method for sorter
  impl VirtualMachine {
    pub fn openSorter(self: &mut Self, num: usize) {
      if num >= self.sorters.len() {
        self.sorters.resize(num + 1, VmSorter::default());
      }
      self.sorters[num].clear();
    }
    pub fn getSorter(self: &mut Self, num: usize) -> Result<&mut VmSorter, String> {
      if num < self.sorters.len() {
        Ok(&mut self.sorters[num])
      } else {
        Err(format!(
          "sorter out of bound num={} size={}",
          num,
          self.sorters.len()
        ))
      }
    }
    pub fn sorterPut(
      self: &mut Self,
      num: usize,
      key: VmMemString,
      value: VmMemString,
    ) -> Result<(), String> {
      Ok(self.getSorter(num)?.push(key, value))
    }
    pub fn sortSorter(self: &mut Self, num: usize) -> Result<(), String> {
      Ok(self.getSorter(num)?.sort())
    }
    pub fn sorterGetValue(self: &mut Self, num: usize) -> Result<Option<VmMemString>, String> {
      Ok(self.getSorter(num)?.pop())
    }
    pub fn sorterGetKey(self: &mut Self, num: usize) -> Result<Option<VmMemString>, String> {
      Ok(self.getSorter(num)?.topKey())
    }
    pub fn sorterClose(self: &mut Self, num: usize) -> Result<(), String> {
      Ok(self.getSorter(num)?.clear())
    }
    pub fn sorterSetDesc(self: &mut Self, num: usize, column: usize) -> Result<(), String> {
      Ok(self.getSorter(num)?.setDesc(column))
    }
  }

  fn parseOperations(operations: String) -> Vec<VmOp> {
    operations.split("\n").into_iter().map(VmOp::new).collect()
  }

  fn popOneMem(vm: &mut VirtualMachine) -> Result<VmMem, String> {
    Ok(vm.popStack(1)?.into_iter().next().unwrap())
  }

  fn runOperation(operations: String) -> Result<String, String> {
    let ops = parseOperations(operations);
    let mut pc = 0;
    let mut ret = String::new();
    let mut vm = VirtualMachine::new();
    loop {
      if pc == ops.len() {
        return Ok(ret);
      }
      let nowOp = &ops[pc];
      match nowOp.vmOpType {
        // implement of every operations
        VmOpType::OP_Transaction => {
          DbWrapper::transaction();
        }
        VmOpType::OP_Commit => {
          DbWrapper::commit();
        }
        VmOpType::OP_Rollback => {
          DbWrapper::rollback();
        }
        VmOpType::OP_ReadCookie => {
          vm.pushStack(VmMem::MEM_INT(DbWrapper::getCookies() as i128));
        }
        VmOpType::OP_SetCookie => {
          DbWrapper::setCookies(nowOp.p1);
        }
        VmOpType::OP_VerifyCookie => {
          let cookies = DbWrapper::getCookies();
          if cookies != nowOp.p1 {
            return Err(String::from("cookies is not right"));
          }
        }
        VmOpType::OP_Open => vm.setCursor(
          nowOp.p1 as usize,
          VmCursor::new(DbWrapper::open(
            nowOp.p3.as_str(),
            DbWrapper::CURSOR_READ_ONLY,
          )),
        ),
        VmOpType::OP_OpenTemp => unimplemented!(),
        VmOpType::OP_OpenWrite => vm.setCursor(
          nowOp.p1 as usize,
          VmCursor::new(DbWrapper::open(nowOp.p3.as_str(), DbWrapper::CURSOR_WRITE)),
        ),
        VmOpType::OP_Close => vm.closeCursor(nowOp.p1 as usize),
        VmOpType::OP_MoveTo => {
          let poped = popOneMem(&mut vm)?;
          let cursor = vm.getCursor(nowOp.p1 as usize)?;
          // TODO: need discuss
          DbWrapper::find(cursor.cursor, &poped.stringify());
          vm.fcnt += 1;
        }
        VmOpType::OP_Fcnt => {
          vm.pushStack(VmMem::MEM_INT(vm.fcnt as i128));
        }
        VmOpType::OP_NewRecno => unimplemented!(),
        VmOpType::OP_Put => {
          let value = match popOneMem(&mut vm)? {
            VmMem::MEM_STRING(x) => x,
            _ => return Err(String::from("incorrect value format")),
          };
          let key = match popOneMem(&mut vm)? {
            VmMem::MEM_STRING(x) => x,
            _ => return Err(String::from("incorrect key format")),
          };
          let cursor = vm.getCursor(nowOp.p1 as usize)?;
          DbWrapper::insert(cursor.cursor, &key, &value);
        }
        VmOpType::OP_Distinct | VmOpType::OP_Found | VmOpType::OP_NotFound => {
          let poped = popOneMem(&mut vm)?;
          if let VmOpType::OP_Distinct = nowOp.vmOpType {
            vm.pushStack(poped.clone());
          }
          let mut found = false;
          if let VmMem::MEM_STRING(key) = poped {
            let cursor = vm.getCursor(nowOp.p1 as usize)?;
            DbWrapper::find(cursor.cursor, &key);
            let findKey = DbWrapper::getKey(cursor.cursor);
            if findKey.len() != 0 && key.string == findKey {
              found = true;
            }
            match nowOp.vmOpType {
              VmOpType::OP_Distinct | VmOpType::OP_NotFound => {
                if !found {
                  pc = nowOp.p2 as usize - 1;
                }
              }
              VmOpType::OP_Found => {
                if found {
                  pc = nowOp.p2 as usize - 1;
                }
              }
              _ => {
                unreachable!();
              }
            }
          } else {
            return Err(String::from("find should use a value made by IR MakeKey"));
          }
        }
        VmOpType::OP_Delete => {
          let cursor = vm.getCursor(nowOp.p1 as usize)?;
          DbWrapper::erase(cursor.cursor);
        }
        VmOpType::OP_Column => {
          let cursor = vm.getCursor(nowOp.p1 as usize)?;
          let data = match cursor.keyAsData {
            true => DbWrapper::getKey(cursor.cursor),
            false => DbWrapper::getValue(cursor.cursor),
          };
          vm.pushStack(VmMem::MEM_STRING(VmMemString::new(VmMem::getColumn(
            data,
            nowOp.p2 as usize,
          )?)));
        }
        VmOpType::OP_KeyAsData => {
          let mut cursor = vm.getCursor(nowOp.p1 as usize)?;
          if nowOp.p2 == 0 {
            cursor.keyAsData = false;
          } else {
            cursor.keyAsData = true;
          }
        }
        VmOpType::OP_Recno => unimplemented!(),
        VmOpType::OP_FullKey => {
          let cursor = vm.getCursor(nowOp.p1 as usize)?;
          let key = DbWrapper::getKey(cursor.cursor);
          vm.pushStack(VmMem::MEM_STRING(VmMemString::new(key)));
        }
        VmOpType::OP_Rewind => {
          let cursor = vm.getCursor(nowOp.p1 as usize)?;
          DbWrapper::reset(cursor.cursor);
        }
        VmOpType::OP_Next => {
          let cursor = vm.getCursor(nowOp.p1 as usize)?;
          let key = DbWrapper::getKey(cursor.cursor);
          if key.len() == 0 {
            pc = nowOp.p2 as usize - 1;
          } else {
            DbWrapper::next(cursor.cursor);
          }
        }
        VmOpType::OP_Destroy => unimplemented!(),
        VmOpType::OP_Clear => unimplemented!(),
        VmOpType::OP_CreateIndex => unimplemented!(),
        VmOpType::OP_CreateTable => unimplemented!(),
        VmOpType::OP_Reorganize => {
          DbWrapper::reorganize();
        }
        VmOpType::OP_BeginIdx => {
          let poped = popOneMem(&mut vm)?;
          if let VmMem::MEM_STRING(key) = poped {
            let mut cursor = vm.getCursor(nowOp.p1 as usize)?;
            DbWrapper::find(cursor.cursor, &key);
            cursor.key = key;
          } else {
            return Err(String::from("find should use a value made by IR MakeKey"));
          }
        }
        VmOpType::OP_NextIdx => {
          // TODO: design needed
          let mut cursor = vm.getCursor(nowOp.p1 as usize)?;
          let nowKey = DbWrapper::getKey(cursor.cursor);
          if nowKey.len() == 0 {
            // end
            pc = nowOp.p2 as usize - 1;
          } else {
          }
        }
        VmOpType::OP_PutIdx => unimplemented!(),
        VmOpType::OP_DeleteIdx => unimplemented!(),
        VmOpType::OP_MemLoad => unimplemented!(),
        VmOpType::OP_MemStore => unimplemented!(),
        VmOpType::OP_ListOpen => {
          vm.openList(nowOp.p1 as usize);
        }
        VmOpType::OP_ListWrite => {
          let poped = popOneMem(&mut vm)?;
          vm.writeList(nowOp.p1 as usize, poped.stringify());
        }
        VmOpType::OP_ListRewind => {
          vm.rewindList(nowOp.p1 as usize)?;
        }
        VmOpType::OP_ListRead => {
          if let Some(x) = vm.readList(nowOp.p1 as usize)? {
            vm.pushStack(VmMem::MEM_STRING(x));
          } else {
            pc = nowOp.p2 as usize - 1;
          }
        }
        VmOpType::OP_ListClose => {
          vm.closeList(nowOp.p1 as usize)?;
        }
        VmOpType::OP_SortOpen => {
          vm.openSorter(nowOp.p1 as usize);
        }
        VmOpType::OP_SortPut => {
          let key = popOneMem(&mut vm)?.stringify();
          let value = popOneMem(&mut vm)?.stringify();
          vm.sorterPut(nowOp.p1 as usize, key, value)?;
        }
        VmOpType::OP_SortMakeRec => unimplemented!(),
        VmOpType::OP_SortMakeKey => unimplemented!(),
        VmOpType::OP_Sort => {
          vm.sortSorter(nowOp.p1 as usize)?;
        }
        VmOpType::OP_SortNext => {
          let value = vm.sorterGetValue(nowOp.p1 as usize)?;
          match value {
            None => {
              pc = nowOp.p2 as usize - 1;
            }
            Some(x) => vm.pushStack(VmMem::MEM_STRING(x)),
          }
        }
        VmOpType::OP_SortKey => {
          let key = vm.sorterGetKey(nowOp.p1 as usize)?;
          match key {
            None => {
              pc = nowOp.p2 as usize - 1;
            }
            Some(x) => vm.pushStack(VmMem::MEM_STRING(x)),
          }
        }
        VmOpType::OP_SortCallback => unimplemented!(),
        VmOpType::OP_SortClose => {
          vm.sorterClose(nowOp.p1 as usize)?;
        }
        VmOpType::OP_FileOpen => unimplemented!(),
        VmOpType::OP_FileRead => unimplemented!(),
        VmOpType::OP_FileColumn => unimplemented!(),
        VmOpType::OP_FileClose => unimplemented!(),
        VmOpType::OP_AggReset => unimplemented!(),
        VmOpType::OP_AggFocus => unimplemented!(),
        VmOpType::OP_AggIncr => unimplemented!(),
        VmOpType::OP_AggNext => unimplemented!(),
        VmOpType::OP_AggSet => unimplemented!(),
        VmOpType::OP_AggGet => unimplemented!(),
        VmOpType::OP_SetInsert => unimplemented!(),
        VmOpType::OP_SetFound => unimplemented!(),
        VmOpType::OP_SetNotFound => unimplemented!(),
        VmOpType::OP_SetClear => unimplemented!(),
        VmOpType::OP_MakeRecord => {
          // TODO: need change
          let poped = vm.popStack(nowOp.p1)?;
          let result = poped
            .into_iter()
            .rev()
            .map(VmMem::stringify)
            .map(|x| x.string)
            .collect::<Vec<Vec<u8>>>()
            .concat();
          vm.pushStack(VmMem::MEM_STRING(VmMemString::new(result)));
        }
        VmOpType::OP_MakeKey => {
          // TODO: need change
          let poped = vm.popStack(nowOp.p1)?;
          if nowOp.p2 != 0 {
            for x in poped.iter().rev() {
              vm.pushStack(x.clone())
            }
          }
          let result = poped
            .into_iter()
            .rev()
            .map(VmMem::stringify)
            .map(|x| x.string)
            .collect::<Vec<Vec<u8>>>()
            .concat();
          vm.pushStack(VmMem::MEM_STRING(VmMemString::new(result)));
        }
        VmOpType::OP_MakeIdxKey => unimplemented!(),
        // goto
        VmOpType::OP_Goto => {
          pc = nowOp.p2 as usize - 1;
        }
        VmOpType::OP_JIf => {
          let a = popOneMem(&mut vm)?;
          let flag = match a {
            VmMem::MEM_INT(x) if x != 0 => 1,
            VmMem::MEM_STRING(x) if a.getStringRawLength() != 0 => 1,
            VmMem::MEM_DOUBLE(x) if x.ne(&0.0) => 1,
            _ => 0,
          };
          if nowOp.p1 != flag {
            pc = nowOp.p2 as usize - 1;
          }
        }
        // stop the vm
        VmOpType::OP_Halt => {
          pc = ops.len() - 1;
        }
        // push the result column number
        VmOpType::OP_ColumnCount => vm
          .resultColumnNames
          .resize(nowOp.p1 as usize, String::new()),
        // set the p1-th column's name
        VmOpType::OP_ColumnName => {
          vm.resultColumnNames[nowOp.p1 as usize] = String::from(nowOp.p3.clone());
        }
        // set the callback function
        VmOpType::OP_Callback => unimplemented!(),
        // push P1 to the stack
        VmOpType::OP_Integer => vm.pushStack(VmMem::MEM_INT(nowOp.p1 as i128)),
        // push string to the stack
        // a string may be used multiple times, so clone is needed here
        VmOpType::OP_String => vm.pushStack(VmMem::MEM_STRING(VmMem::genVmString(
          nowOp.p3.as_bytes().into_iter().map(|&x| x).collect(),
          VmMemString::MEM_FLAG_STRING,
        ))),
        // push null to the stack
        VmOpType::OP_Null => vm.pushStack(VmMem::MEM_NULL),
        // pop an element from the stack
        VmOpType::OP_Pop => {
          vm.popStack(1);
        }
        // push the p1-th element to the stack
        VmOpType::OP_Dup => {
          let targetIndex = vm.stack.len() - (1 + nowOp.p1) as usize;
          let value = vm.stack[targetIndex].clone();
          vm.pushStack(value);
        }
        // pop the p1-th element and push it to the stack
        VmOpType::OP_Pull => {
          let poped = vm.popStack(nowOp.p1)?;
          let newTop = vm.popStack(1)?;
          for x in poped.into_iter().rev() {
            vm.pushStack(x);
          }
          for x in newTop.into_iter() {
            vm.pushStack(x);
          }
        }
        VmOpType::OP_Add | VmOpType::OP_Multiply | VmOpType::OP_Subtract | VmOpType::OP_Divide => {
          let a = popOneMem(&mut vm)?;
          let b = popOneMem(&mut vm)?;
          if let (&VmMem::MEM_INT(a), &VmMem::MEM_INT(b)) = (&a, &b) {
            vm.pushStack(match nowOp.vmOpType {
              VmOpType::OP_Add => VmMem::MEM_INT(a + b),
              VmOpType::OP_Multiply => VmMem::MEM_INT(a * b),
              VmOpType::OP_Subtract => VmMem::MEM_INT(b - a),
              VmOpType::OP_Divide => match a {
                0 => VmMem::MEM_NULL,
                _ => VmMem::MEM_INT(b / a),
              },
              _ => unreachable!(),
            })
          } else {
            let a = a.realify();
            let b = b.realify();
            vm.pushStack(match nowOp.vmOpType {
              VmOpType::OP_Add => VmMem::MEM_DOUBLE(a + b),
              VmOpType::OP_Multiply => VmMem::MEM_DOUBLE(a * b),
              VmOpType::OP_Subtract => VmMem::MEM_DOUBLE(b - a),
              VmOpType::OP_Divide => VmMem::MEM_DOUBLE(b / a),
              _ => unreachable!(),
            })
          }
        }
        VmOpType::OP_AddImm => {
          let top = popOneMem(&mut vm)?;
          vm.pushStack(match top {
            VmMem::MEM_INT(x) => VmMem::MEM_INT(x + nowOp.p1 as i128),
            x @ _ => VmMem::MEM_DOUBLE(x.realify() + nowOp.p1 as f64),
          })
        }
        VmOpType::OP_Min => {
          let a = popOneMem(&mut vm)?;
          let b = popOneMem(&mut vm)?;
          if let (&VmMem::MEM_INT(a), &VmMem::MEM_INT(b)) = (&a, &b) {
            vm.pushStack(VmMem::MEM_INT(std::cmp::min(a, b)));
          } else {
            let a = a.realify();
            let b = b.realify();
            vm.pushStack(VmMem::MEM_DOUBLE(a.min(b)));
          }
        }
        VmOpType::OP_Max => {
          let a = popOneMem(&mut vm)?;
          let b = popOneMem(&mut vm)?;
          if let (&VmMem::MEM_INT(a), &VmMem::MEM_INT(b)) = (&a, &b) {
            vm.pushStack(VmMem::MEM_INT(std::cmp::max(a, b)));
          } else {
            let a = a.realify();
            let b = b.realify();
            vm.pushStack(VmMem::MEM_DOUBLE(a.max(b)));
          }
        }
        VmOpType::OP_JLike | VmOpType::OP_JGlob => {
          let any = match nowOp.vmOpType {
            VmOpType::OP_JLike => "%",
            VmOpType::OP_JGlob => "*",
            _ => unreachable!(),
          };
          let one = match nowOp.vmOpType {
            VmOpType::OP_JLike => "_",
            VmOpType::OP_JGlob => ".",
            _ => unreachable!(),
          };
          let pattern = String::from_utf8_lossy(
            vm.popStack(1)?
              .into_iter()
              .next()
              .unwrap()
              .stringify()
              .as_slice(),
          )
          .to_string();
          let string = String::from_utf8_lossy(
            vm.popStack(1)?
              .into_iter()
              .next()
              .unwrap()
              .stringify()
              .as_slice(),
          )
          .to_string();
          let regexPattern = pattern.replace(any, ".*").replace(one, ".");
          let re = regex::Regex::new(regexPattern.as_str())
            .expect(format!("unknown pattern {} ", pattern).as_str());
          if re.is_match(string.as_str()) {
            if nowOp.p1 == 0 {
              pc = (nowOp.p2 - 1) as usize;
            }
          } else {
            if nowOp.p1 == 1 {
              pc = (nowOp.p2 - 1) as usize;
            }
          }
        }
        VmOpType::OP_JEq => {
          let a = popOneMem(&mut vm)?;
          let b = popOneMem(&mut vm)?;
          if a == b {
            pc = (nowOp.p2 - 1) as usize;
          }
        }
        VmOpType::OP_JNe => {
          let a = popOneMem(&mut vm)?;
          let b = popOneMem(&mut vm)?;
          if a != b {
            pc = (nowOp.p2 - 1) as usize;
          }
        }
        VmOpType::OP_JLt => {
          let b = popOneMem(&mut vm)?;
          let a = popOneMem(&mut vm)?;
          if PartialOrd::lt(&a, &b) {
            pc = (nowOp.p2 - 1) as usize;
          }
        }
        VmOpType::OP_JLe => {
          let b = popOneMem(&mut vm)?;
          let a = popOneMem(&mut vm)?;
          if PartialOrd::le(&a, &b) {
            pc = (nowOp.p2 - 1) as usize;
          }
        }
        VmOpType::OP_JGt => {
          let b = popOneMem(&mut vm)?;
          let a = popOneMem(&mut vm)?;
          if PartialOrd::gt(&a, &b) {
            pc = (nowOp.p2 - 1) as usize;
          }
        }
        VmOpType::OP_JGe => {
          let b = popOneMem(&mut vm)?;
          let a = popOneMem(&mut vm)?;
          if PartialOrd::ge(&a, &b) {
            pc = (nowOp.p2 - 1) as usize;
          }
        }
        VmOpType::OP_JIsNull => {
          let a = popOneMem(&mut vm)?;
          if let VmMem::MEM_NULL = a {
            pc = (nowOp.p2 - 1) as usize;
          }
        }
        VmOpType::OP_JNotNull => {
          let a = popOneMem(&mut vm)?;
          match a {
            VmMem::MEM_NULL => {}
            _ => pc = (nowOp.p2 - 1) as usize,
          }
        }
        VmOpType::OP_Negative => {
          let a = popOneMem(&mut vm)?;
          if let &VmMem::MEM_INT(a) = &a {
            vm.pushStack(VmMem::MEM_INT(-a));
          } else {
            vm.pushStack(VmMem::MEM_DOUBLE(-a.realify()))
          }
        }
        VmOpType::OP_And => {
          let a = popOneMem(&mut vm)?.integerify();
          let b = popOneMem(&mut vm)?.integerify();
          vm.pushStack(VmMem::MEM_INT(a & b));
        }
        VmOpType::OP_Or => {
          let a = popOneMem(&mut vm)?.integerify();
          let b = popOneMem(&mut vm)?.integerify();
          vm.pushStack(VmMem::MEM_INT(a | b));
        }
        VmOpType::OP_Not => {
          let a = popOneMem(&mut vm)?.integerify();
          vm.pushStack(VmMem::MEM_INT(!a));
        }
        // concat the p1 elements from (p1-1)-th to 0-th use p3 as separator
        // pop if P2=0
        VmOpType::OP_Concat => {
          let poped = vm.popStack(nowOp.p1)?;
          if nowOp.p2 == 1 {
            for x in poped.iter() {
              vm.pushStack(x.clone());
            }
          }
          vm.pushStack(VmMem::MEM_STRING(VmMem::genVmString(
            poped
              .into_iter()
              .rev()
              .map(VmMem::stringify)
              .map(|x| x.removeHead())
              .collect::<Vec<Vec<u8>>>()
              .join(nowOp.p3.as_bytes()),
            VmMemString::MEM_FLAG_STRING,
          )));
        }
        VmOpType::OP_Noop => {}
        VmOpType::OP_Strlen => unimplemented!(),
        VmOpType::OP_Substr => unimplemented!(),
        VmOpType::OP_SetIf => unimplemented!(),
        VmOpType::OP_SetLike => unimplemented!(),
        VmOpType::OP_SetGlob => unimplemented!(),
        VmOpType::OP_SetEq => unimplemented!(),
        VmOpType::OP_SetNe => unimplemented!(),
        VmOpType::OP_SetLt => unimplemented!(),
        VmOpType::OP_SetLe => unimplemented!(),
        VmOpType::OP_SetGt => unimplemented!(),
        VmOpType::OP_SetGe => unimplemented!(),
        VmOpType::OP_SetIsNull => unimplemented!(),
        VmOpType::OP_SetNotNull => unimplemented!(),
        VmOpType::OP_SortSetDesc => {
          vm.sorterSetDesc(nowOp.p1 as usize, nowOp.p2 as usize)?;
        }
        _ => {
          return Err(format!("unknown operation: {:?}", nowOp));
        }
      }
      pc += 1;
    }
  }
}
