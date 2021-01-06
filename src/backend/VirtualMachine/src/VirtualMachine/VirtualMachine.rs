use super::VmAgg::VmAgg;
use super::VmCursor::VmCursor;
use super::VmList::VmList;
use super::VmMem::VmMem;
use super::VmMem::VmMemString;
use super::VmOp::VmOp;
use super::VmSet::VmSet;
use super::VmSorter::VmSorter;
use super::VmTmpCursor::VmTmpCursor;
use crate::wrapper::rustLayer as DbWrapper;

#[derive(Default)]
pub struct VirtualMachine {
  pub vmOps: Vec<VmOp>,
  pub vmCursors: Vec<VmCursor>,
  pub vmTmpCursors: Vec<VmTmpCursor>,
  pub cursorIsTmp: Vec<bool>,
  pub stack: Vec<VmMem>,
  pub resultColumnNames: Vec<Vec<u8>>,
  pub resultColumnNamePtrs: Option<Vec<*const u8>>,
  // pub resultColumnNamesPtr: Option<*const *const u8>,
  pub fcnt: i32,
  pub lists: Vec<VmList>,
  pub sorters: Vec<VmSorter>,
  pub sets: Vec<VmSet>,
  pub agg: VmAgg,
  pub transactionId: i32,
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
  pub fn setCursor(self: &mut Self, num: usize, cursorName: &String, flag: i32) {
    if (self.vmCursors.len() <= num) {
      self.vmCursors.resize(num + 1, VmCursor::default());
    }
    self.vmCursors[num] = VmCursor::new(cursorName, flag, self.transactionId);
    self.vmCursors[num].rewind();
  }
  fn getCursor(self: &mut Self, num: usize) -> Result<&mut VmCursor, String> {
    if num < self.vmCursors.len() {
      Ok(&mut self.vmCursors[num])
    } else {
      Err(format!("no cursor numbered {}", num))
    }
  }
  pub fn closeCursor(self: &mut Self, num: usize) -> Result<(), String> {
    if num < self.cursorIsTmp.len() && self.cursorIsTmp[num] {
      Ok(())
    } else {
      Ok(self.getCursor(num)?.close()?)
    }
  }
  pub fn cursorInsert(
    self: &mut Self,
    num: usize,
    key: &VmMemString,
    value: &VmMemString,
  ) -> Result<(), String> {
    if num < self.cursorIsTmp.len() && self.cursorIsTmp[num] {
      self.putTmpCursor(num, key, value)
    } else {
      self.getCursor(num)?.insert(key, value)
    }
  }
  pub fn cursorGetKey(self: &mut Self, num: usize) -> Result<&VmMemString, String> {
    Ok(self.getCursor(num)?.getKey())
  }
  pub fn cursorGetValue(self: &mut Self, num: usize) -> Result<&VmMemString, String> {
    Ok(self.getCursor(num)?.getValue())
  }
  pub fn cursorGetAddress(self: &mut Self, num: usize) -> Result<i32, String> {
    Ok(self.getCursor(num)?.getAddress())
  }
  pub fn cursorFindKey(self: &mut Self, num: usize, key: &VmMemString) -> Result<(), String> {
    self.getCursor(num)?.find(key)?;
    Ok(())
  }
  pub fn cursorDelete(self: &mut Self, num: usize) -> Result<(), String> {
    self.getCursor(num)?.erase()?;
    Ok(())
  }
  pub fn cursorKeyAsData(self: &mut Self, num: usize) -> Result<bool, String> {
    Ok(self.getCursor(num)?.getKeyAsData())
  }
  pub fn cursorSetKeyAsData(self: &mut Self, num: usize, flag: bool) -> Result<(), String> {
    self.getCursor(num)?.setKeyAsData(flag);
    Ok(())
  }
  pub fn cursorRewind(self: &mut Self, num: usize) -> Result<(), String> {
    if num < self.cursorIsTmp.len() && self.cursorIsTmp[num] {
      self.rewindTmpCursor(num)
    } else {
      self.getCursor(num)?.rewind()
    }
  }
  pub fn cursorNext(self: &mut Self, num: usize) -> Result<(), String> {
    if num < self.cursorIsTmp.len() && self.cursorIsTmp[num] {
      self.nextTmpCursor(num)
    } else {
      self.getCursor(num)?.next()
    }
  }
  pub fn cursorIsEnd(self: &mut Self, num: usize) -> Result<bool, String> {
    Ok(self.getCursor(num)?.isEnd())
  }
  pub fn cursorSetIdx(self: &mut Self, num: usize, key: &VmMemString) -> Result<(), String> {
    self.getCursor(num)?.setIdx(key);
    Ok(())
  }
}

impl VirtualMachine {
  pub fn isTmp(&mut self, num: usize) -> bool {
    num < self.cursorIsTmp.len() && self.cursorIsTmp[num]
  }
  pub fn openTmpCursor(&mut self, num: usize) {
    if self.cursorIsTmp.len() <= num {
      self.cursorIsTmp.resize(num + 1, false);
      self.vmTmpCursors.resize(num + 1, VmTmpCursor::default());
    }
    self.vmTmpCursors[num] = VmTmpCursor::default();
  }
  fn getTmpCursor(&mut self, num: usize) -> Result<&mut VmTmpCursor, String> {
    if num < self.vmTmpCursors.len() {
      Err(format!(
        "VmTmpCursor index out of bounds: len={} index={}",
        self.vmTmpCursors.len(),
        num
      ))
    } else if !self.cursorIsTmp[num] {
      Err(format!("this is not tmp cursor"))
    } else {
      Ok(&mut self.vmTmpCursors[num])
    }
  }
  pub fn putTmpCursor(
    &mut self,
    num: usize,
    key: &VmMemString,
    value: &VmMemString,
  ) -> Result<(), String> {
    Ok(self.getTmpCursor(num)?.put(key, value))
  }
  pub fn columnTmpCursor(&mut self, num: usize, idx: usize) -> Result<VmMem, String> {
    self.getTmpCursor(num)?.columnValue(idx)
  }
  pub fn nextTmpCursor(&mut self, num: usize) -> Result<(), String> {
    Ok(self.getTmpCursor(num)?.next())
  }
  pub fn rewindTmpCursor(&mut self, num: usize) -> Result<(), String> {
    Ok(self.getTmpCursor(num)?.rewind())
  }
  pub fn isEndTmpCursor(&mut self, num: usize) -> Result<bool, String> {
    Ok(self.getTmpCursor(num)?.isEnd())
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

/// method for set
impl VirtualMachine {
  pub fn openSet(self: &mut Self, num: usize) {
    if num >= self.sets.len() {
      self.sets.resize(num + 1, VmSet::default());
    }
    self.sets[num].clear();
  }
  pub fn getSet(self: &mut Self, num: usize) -> Result<&mut VmSet, String> {
    if num >= self.sets.len() {
      Err(format!(
        "set out of bound num={} size={}",
        num,
        self.sets.len()
      ))
    } else {
      Ok(&mut self.sets[num])
    }
  }
  pub fn setInsert(self: &mut Self, num: usize, data: VmMemString) -> Result<(), String> {
    Ok(self.getSet(num)?.insert(data))
  }
  pub fn setFound(self: &mut Self, num: usize, data: VmMemString) -> Result<bool, String> {
    Ok(self.getSet(num)?.found(data))
  }
  pub fn setClear(self: &mut Self, num: usize) -> Result<(), String> {
    Ok(self.getSet(num)?.clear())
  }
  pub fn setEmpty(self: &mut Self, num: usize) -> Result<bool, String> {
    Ok(self.getSet(num)?.empty())
  }
}

/// method for add

impl VirtualMachine {
  pub fn resizeAgg(self: &mut Self, size: usize) -> Result<(), String> {
    self.agg.resizeCurrent(size)
  }
  pub fn focusAgg(self: &mut Self, key: VmMem) -> bool {
    self.agg.focus(key)
  }
  pub fn incrAgg(self: &mut Self, index: usize, num: i32) -> Result<(), String> {
    self.agg.incr(index, num)
  }
  pub fn nextAgg(self: &mut Self) -> bool {
    self.agg.next()
  }
  pub fn getAgg(self: &mut Self, index: usize) -> Result<VmMem, String> {
    self.agg.get(index)
  }
  pub fn setAgg(self: &mut Self, index: usize, value: VmMem) -> Result<(), String> {
    self.agg.set(index, value)
  }
}

/// method for transaction
impl VirtualMachine {
  pub fn transaction(self: &mut Self) {
    DbWrapper::transaction(&mut self.transactionId);
  }
  pub fn rollback(self: &Self) {
    DbWrapper::rollback(self.transactionId);
  }
  pub fn commit(self: &Self) {
    DbWrapper::commit(self.transactionId);
  }
  pub fn getCookies(self: &Self) -> i32 {
    DbWrapper::getCookies()
  }
  pub fn setCookies(self: &Self, cookies: i32) {
    DbWrapper::setCookies(cookies);
  }
  pub fn reorganize(self: &Self) {
    DbWrapper::reorganize();
  }
}

#[cfg(test)]
mod VirtualMachineTest {
  use super::super::VmMem::VmMem;
  use super::VirtualMachine;
  #[test]
  fn test_VirtualMachine_pushStack_work() {
    let mut vm = VirtualMachine::new();
    vm.pushStack(VmMem::MEM_INT(1));
    vm.pushStack(VmMem::MEM_INT(2));
    assert_eq!(vm.stack, vec![VmMem::MEM_INT(1), VmMem::MEM_INT(2)]);
  }
  #[test]
  fn test_VirtualMachine_popStack_work() {
    let mut vm = VirtualMachine::new();
    vm.pushStack(VmMem::MEM_INT(1));
    vm.pushStack(VmMem::MEM_INT(2));
    assert_eq!(vm.popStack(1), Ok(vec![VmMem::MEM_INT(2)]));
    assert_eq!(vm.popStack(1), Ok(vec![VmMem::MEM_INT(1)]));
    assert_eq!(
      vm.popStack(1),
      Err(format!("stack size: {}, pop {} element(s)", 0, 1))
    );
    vm.pushStack(VmMem::MEM_INT(1));
    vm.pushStack(VmMem::MEM_INT(2));
    assert_eq!(
      vm.popStack(2),
      Ok(vec![VmMem::MEM_INT(2), VmMem::MEM_INT(1)])
    );
  }
}
