use super::VirtualMachine::VirtualMachine;
use super::VmCursor::VmCursor;
use super::VmMem::VmMem;
use super::VmMem::VmMemString;
use super::VmOp::VmOp;
use super::VmOp::VmOpType;
use crate::wrapper::rustLayer as DbWrapper;

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
          if findKey.len() != 0 && *key == findKey {
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
      VmOpType::OP_SetInsert => {
        let string = if nowOp.p3.len() == 0 {
          let poped = popOneMem(&mut vm)?;
          poped.stringify()
        } else {
          VmMem::genVmString(nowOp.p3.as_bytes().to_vec(), VmMemString::MEM_FLAG_STRING)
        };
        vm.setInsert(nowOp.p1 as usize, string)?;
      }
      VmOpType::OP_SetFound => {
        let poped = popOneMem(&mut vm)?.stringify();
        if vm.setFound(nowOp.p1 as usize, poped)? {
          pc = nowOp.p2 as usize - 1;
        }
      }
      VmOpType::OP_SetNotFound => {
        let poped = popOneMem(&mut vm)?.stringify();
        if !vm.setFound(nowOp.p1 as usize, poped)? {
          pc = nowOp.p2 as usize - 1;
        }
      }
      VmOpType::OP_SetClear => vm.setClear(nowOp.p1 as usize)?,
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
      VmOpType::OP_SetOpen => {
        vm.openSet(nowOp.p1 as usize);
      }
      _ => {
        return Err(format!("unknown operation: {:?}", nowOp));
      }
    }
    pc += 1;
  }
}
