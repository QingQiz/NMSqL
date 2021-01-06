#[derive(Debug, Copy, Clone, FromPrimitive, Eq, PartialEq)]
pub enum VmOpType {
  OP_Unreachable = 0,
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
  OP_JSetFound,
  OP_JSetNotFound,
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
  OP_SetOpen,
  OP_SetSetFound,
  OP_SetSetNotFound,
  OP_SetSetEmpty,
  OP_DefaultKey,
  OP_Address,
  OP_Double,
}

impl Default for VmOpType {
  fn default() -> Self {
    VmOpType::OP_Unreachable
  }
}

#[derive(Debug, Clone, Default)]
pub struct VmOp {
  pub lineNo: i32,
  pub vmOpType: VmOpType,
  pub p1: i32,
  pub p2: i32,
  pub p3: String,
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
        .expect(format!("p2 expects a integer reads {}", vmOpParts[3]).as_str()),
      p3: String::from(vmOpParts[4]),
    }
  }
}

#[cfg(test)]
mod VmOpTest {
  use super::VmOp;
  use super::VmOpType;
  #[test]
  fn test_VmOp_new_work() {
    let s = "0|1|1|1|";
    let op = VmOp::new(s);
    assert_eq!(op.lineNo, 0);
    assert_eq!(op.vmOpType, VmOpType::OP_Transaction);
    assert_eq!(op.p1, 1);
    assert_eq!(op.p2, 1);
    assert_eq!(op.p3, "");
  }
  #[test]
  fn test_VmOp_new_work_manySplit() {
    let s = "0|1|1|1|a|b|c";
    let op = VmOp::new(s);
    assert_eq!(op.lineNo, 0);
    assert_eq!(op.vmOpType, VmOpType::OP_Transaction);
    assert_eq!(op.p1, 1);
    assert_eq!(op.p2, 1);
    assert_eq!(op.p3, "a|b|c");
  }
  #[test]
  #[should_panic(expected = "less than 4")]
  fn test_VmOp_new_lessSplit() {
    let s = "123";
    VmOp::new(s);
  }
  #[test]
  #[should_panic(expected = "line number should be a interger, but found")]
  fn test_VmOp_new_illegalLineNo() {
    let s = "0a|1|1|1|";
    let op = VmOp::new(s);
  }
  #[test]
  #[should_panic(expected = "VmOpType expects a integer reads")]
  fn test_VmOp_new_illegalVmOpType() {
    let s = "0|1a|1|1|";
    let op = VmOp::new(s);
  }
  #[test]
  #[should_panic(expected = "unknown operation:")]
  fn test_VmOp_new_illegalVmOpType1() {
    let s = "0|1000|1|1|";
    let op = VmOp::new(s);
  }
  #[test]
  #[should_panic(expected = "p1 expects a integer reads")]
  fn test_VmOp_new_illegalP1() {
    let s = "0|1|1a|1|";
    let op = VmOp::new(s);
  }
  #[test]
  #[should_panic(expected = "p2 expects a integer reads")]
  fn test_VmOp_new_illegalP2() {
    let s = "0|1|1|1a|";
    let op = VmOp::new(s);
  }
}
