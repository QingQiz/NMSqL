use super::VmCursor::VmCursor;
use super::VmList::VmList;
use super::VmMem::VmMem;
use super::VmMem::VmMemString;
use super::VmOp::VmOp;
use super::VmSet::VmSet;
use super::VmSorter::VmSorter;
use crate::wrapper::rustLayer as DbWrapper;

#[derive(Default)]
pub struct VirtualMachine {
  pub vmOps: Vec<VmOp>,
  pub vmCursors: Vec<VmCursor>,
  pub stack: Vec<VmMem>,
  pub resultColumnNames: Vec<String>,
  pub fcnt: i32,
  pub lists: Vec<VmList>,
  pub sorters: Vec<VmSorter>,
  pub sets: Vec<VmSet>,
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
}
