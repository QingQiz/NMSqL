// FIXME: delete this when release
#[allow(unused)]

pub mod VirtualMachine {
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
    cursor: Cursor,
  }
  #[derive(Debug, Clone)]
  enum VmMem {
    MEM_INT(i128),
    MEM_DOUBLE(f64),
    MEM_NULL,
    MEM_STRING(String),
  }
  impl VmMem {
    pub fn stringify(self: Self) -> String {
      match self {
        VmMem::MEM_INT(x) => x.to_string(),
        VmMem::MEM_DOUBLE(x) => x.to_string(),
        VmMem::MEM_NULL => String::new(),
        VmMem::MEM_STRING(x) => x,
      }
    }
    pub fn integerify(self: Self) -> i128 {
      match self {
        VmMem::MEM_INT(x) => x,
        VmMem::MEM_DOUBLE(x) => x as i128,
        VmMem::MEM_NULL => 0,
        VmMem::MEM_STRING(x) => x
          .parse()
          .expect(format!("expect a integer got {}", x).as_str()),
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
    // return the poped values from top to bottom
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
        VmOpType::OP_Goto => {
          pc = nowOp.p2 as usize;
        }
        VmOpType::OP_Halt => {
          pc = ops.len();
        }
        VmOpType::OP_Integer => vm.pushStack(VmMem::MEM_INT(nowOp.p1 as i128)),
        // a string may be used multiple times, a clone is needed here
        VmOpType::OP_String => vm.pushStack(VmMem::MEM_STRING(nowOp.p3.clone())),
        VmOpType::OP_Null => vm.pushStack(VmMem::MEM_NULL),
        VmOpType::OP_Pop => {
          vm.popStack(1);
        }
        VmOpType::OP_Dup => {
          let target = vm.stack.len() - (1 + nowOp.p1) as usize;
          let value = vm.stack[target].clone();
          vm.pushStack(value);
        }
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
        VmOpType::OP_ColumnCount => vm
          .resultColumnNames
          .resize(nowOp.p1 as usize, String::new()),
        VmOpType::OP_ColumnName => {
          vm.resultColumnNames[nowOp.p1 as usize] = String::from(nowOp.p3.clone());
        }
        VmOpType::OP_Callback => unimplemented!(),
        VmOpType::OP_Concat => unimplemented!(),
        _ => {
          return Err(format!("unknown operation: {:?}", nowOp));
        }
      }
    }
  }
}
