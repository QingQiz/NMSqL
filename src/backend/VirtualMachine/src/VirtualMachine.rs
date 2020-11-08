pub mod VirtualMachine {
  use crate::wrapper::wrapper::rustLayer as DbWrapper;
  use crate::wrapper::wrapper::rustLayer::Cursor;
  #[derive(Debug, Copy, Clone, FromPrimitive)]
  enum VmOpType {
    OP_Transaction = 1,
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
    OP_If,
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
    OP_Like,
    OP_Glob,
    OP_Eq,
    OP_Ne,
    OP_Lt,
    OP_Le,
    OP_Gt,
    OP_Ge,
    OP_IsNull,
    OP_NotNull,
    OP_Negative,
    OP_And,
    OP_Or,
    OP_Not,
    OP_Concat,
    OP_Noop,
    OP_Strlen,
    OP_Substr,
    OP_MAX,
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

  #[derive(Copy, Clone)]
  struct VmCursor {
    cursor: *mut Cursor,
  }
  #[derive(Debug, Clone, PartialEq, PartialOrd)]
  enum VmMem {
    MEM_INT(i128),
    MEM_DOUBLE(f64),
    MEM_NULL,
    MEM_STRING(Vec<u8>),
  }
  impl VmMem {
    /// turn value into string and make it like C string
    pub fn stringify(self: Self) -> Vec<u8> {
      match self {
        VmMem::MEM_INT(x) => {
          let mut ret: Vec<u8> = x.to_string().as_bytes().iter().map(|&x| x).collect();
          ret.push(0u8);
          ret
        }
        VmMem::MEM_DOUBLE(x) => {
          let mut ret: Vec<u8> = x.to_string().as_bytes().iter().map(|&x| x).collect();
          ret.push(0u8);
          ret
        }
        VmMem::MEM_NULL => vec![0],
        VmMem::MEM_STRING(mut x) => {
          if_chain! {
              if x.len() != 0;
              if let Some(&0u8) = x.last();
              then{}
              else {
                  x.push(0u8);
              }
          }
          x
        }
      }
    }
    pub fn integerify(self: Self) -> i128 {
      match self {
        VmMem::MEM_INT(x) => x,
        VmMem::MEM_DOUBLE(x) => x as i128,
        VmMem::MEM_NULL => 0,
        VmMem::MEM_STRING(x) => String::from_utf8_lossy(x.as_slice()).parse().expect(
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
        VmMem::MEM_STRING(x) => String::from_utf8_lossy(x.as_slice()).parse().expect(
          format!(
            "expect a number got {}",
            String::from_utf8_lossy(x.as_slice())
          )
          .as_str(),
        ),
      }
    }
  }
  #[derive(Default)]
  struct VirtualMachine {
    vmOps: Vec<VmOp>,
    vmCursors: Vec<VmCursor>,
    stack: Vec<VmMem>,
    resultColumnNames: Vec<String>,
  }

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
        // goto
        VmOpType::OP_Goto => {
          pc = nowOp.p2 as usize - 1;
        }
        // stop the vm
        VmOpType::OP_Halt => {
          pc = ops.len() - 1;
        }
        // push P1 to the stack
        VmOpType::OP_Integer => vm.pushStack(VmMem::MEM_INT(nowOp.p1 as i128)),
        // push string to the stack
        // a string may be used multiple times, so clone is needed here
        VmOpType::OP_String => vm.pushStack(VmMem::MEM_STRING(
          nowOp.p3.as_bytes().into_iter().map(|&x| x).collect(),
        )),
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
        // concat the p1 elements from (p1-1)-th to 0-th use p3 as separator
        // pop if P2=0
        VmOpType::OP_Concat => {
          let poped = vm.popStack(nowOp.p1)?;
          if nowOp.p2 == 1 {
            for x in poped.iter() {
              vm.pushStack(x.clone());
            }
          }
          vm.pushStack(VmMem::MEM_STRING(
            poped
              .into_iter()
              .rev()
              .map(VmMem::stringify)
              .collect::<Vec<Vec<u8>>>()
              .join(nowOp.p3.as_bytes()),
          ));
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
        VmOpType::OP_AddImm => {
          let top = popOneMem(&mut vm)?;
          vm.pushStack(match top {
            VmMem::MEM_INT(x) => VmMem::MEM_INT(x + nowOp.p1 as i128),
            x @ _ => VmMem::MEM_DOUBLE(x.realify() + nowOp.p1 as f64),
          })
        }
        VmOpType::OP_Eq => {
          let a = popOneMem(&mut vm)?;
          let b = popOneMem(&mut vm)?;
          if a == b {
            pc = (nowOp.p2 - 1) as usize;
          }
        }
        VmOpType::OP_Ne => {
          let a = popOneMem(&mut vm)?;
          let b = popOneMem(&mut vm)?;
          if a != b {
            pc = (nowOp.p2 - 1) as usize;
          }
        }
        VmOpType::OP_Lt => {
          let b = popOneMem(&mut vm)?;
          let a = popOneMem(&mut vm)?;
          if PartialOrd::lt(&a, &b) {
            pc = (nowOp.p2 - 1) as usize;
          }
        }
        VmOpType::OP_Le => {
          let b = popOneMem(&mut vm)?;
          let a = popOneMem(&mut vm)?;
          if PartialOrd::le(&a, &b) {
            pc = (nowOp.p2 - 1) as usize;
          }
        }
        VmOpType::OP_Gt => {
          let b = popOneMem(&mut vm)?;
          let a = popOneMem(&mut vm)?;
          if PartialOrd::gt(&a, &b) {
            pc = (nowOp.p2 - 1) as usize;
          }
        }
        VmOpType::OP_Ge => {
          let b = popOneMem(&mut vm)?;
          let a = popOneMem(&mut vm)?;
          if PartialOrd::ge(&a, &b) {
            pc = (nowOp.p2 - 1) as usize;
          }
        }
        VmOpType::OP_Like | VmOpType::OP_Glob => {
          let any = match nowOp.vmOpType {
            VmOpType::OP_Like => "%",
            VmOpType::OP_Glob => "*",
            _ => unreachable!(),
          };
          let one = match nowOp.vmOpType {
            VmOpType::OP_Like => "_",
            VmOpType::OP_Glob => ".",
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
        VmOpType::OP_Negative => {
          let a = popOneMem(&mut vm)?;
          if let &VmMem::MEM_INT(a) = &a {
            vm.pushStack(VmMem::MEM_INT(-a));
          } else {
            vm.pushStack(VmMem::MEM_DOUBLE(-a.realify()))
          }
        }
        VmOpType::OP_Not => {
          let a = popOneMem(&mut vm)?.integerify();
          vm.pushStack(VmMem::MEM_INT(!a));
        }
        VmOpType::OP_Noop => {}
        VmOpType::OP_If => {
          let a = popOneMem(&mut vm)?;
          match a {
            VmMem::MEM_INT(x) if x != 0 => {
              pc = (nowOp.p2 - 1) as usize;
            }
            VmMem::MEM_STRING(x) if x.len() != 0 => {
              pc = (nowOp.p2 - 1) as usize;
            }
            VmMem::MEM_DOUBLE(x) if x.ne(&0.0) => {
              pc = (nowOp.p2 - 1) as usize;
            }
            _ => {}
          }
        }
        VmOpType::OP_IsNull => {
          let a = popOneMem(&mut vm)?;
          if let VmMem::MEM_NULL = a {
            pc = (nowOp.p2 - 1) as usize;
          }
        }
        VmOpType::OP_NotNull => {
          let a = popOneMem(&mut vm)?;
          match a {
            VmMem::MEM_NULL => {}
            _ => pc = (nowOp.p2 - 1) as usize,
          }
        }
        VmOpType::OP_MakeRecord => {
          let poped = vm.popStack(nowOp.p1)?;
          let mut result = poped
            .into_iter()
            .rev()
            .filter(|x| match x {
              VmMem::MEM_NULL => false,
              _ => true,
            })
            .map(VmMem::stringify)
            .collect::<Vec<Vec<u8>>>()
            .concat();
          let len = {
            let len = result.len() as u16;
            vec![(len >> 8) as u8, (len & 0xff) as u8]
          };
          vm.pushStack(VmMem::MEM_STRING([len, result].concat()));
        }
        VmOpType::OP_MakeKey => {
          let poped = vm.popStack(nowOp.p1)?;
          if nowOp.p2 != 0 {
            for x in poped.iter().rev() {
              vm.pushStack(x.clone())
            }
          }
          vm.pushStack(VmMem::MEM_STRING(
            poped
              .into_iter()
              .rev()
              .map(VmMem::stringify)
              .collect::<Vec<Vec<u8>>>()
              .concat(),
          ))
        }
        VmOpType::OP_MakeIdxKey => {
          let poped = vm.popStack(nowOp.p1)?;
          let key = popOneMem(&mut vm)?.integerify();
          vm.pushStack(VmMem::MEM_STRING(
            [
              vec![
                ((key >> 24) & 0xff) as u8,
                ((key >> 16) & 0xff) as u8,
                ((key >> 8) & 0xff) as u8,
                ((key) & 0xff) as u8,
              ],
              poped
                .into_iter()
                .rev()
                .map(VmMem::stringify)
                .collect::<Vec<Vec<u8>>>()
                .concat(),
            ]
            .concat(),
          ))
        }
        VmOpType::OP_Transaction => unimplemented!(),
        VmOpType::OP_Commit => unimplemented!(),
        VmOpType::OP_Rollback => unimplemented!(),
        VmOpType::OP_ReadCookie => {
          vm.pushStack(VmMem::MEM_INT(DbWrapper::getCookies() as i128));
        }
        VmOpType::OP_SetCookie => {
          DbWrapper::setCookies(nowOp.p1);
        }
        _ => {
          return Err(format!("unknown operation: {:?}", nowOp));
        }
      }
      pc += 1;
    }
  }
}
