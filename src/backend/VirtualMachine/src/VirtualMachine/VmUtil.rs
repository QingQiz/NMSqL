pub fn i32ToVecU8(x: i32) -> Vec<u8> {
  vec![
    ((x >> 24) & 0xff) as u8,
    ((x >> 16) & 0xff) as u8,
    ((x >> 8) & 0xff) as u8,
    ((x >> 0) & 0xff) as u8,
  ]
}
pub fn f64ToVecU8(x: f64) -> Vec<u8> {
  let x = {
    let ptr = &x as *const f64;
    let ptr = ptr as *const u64;
    unsafe { *ptr }
  };
  let mut ret = Vec::new();
  let mut shift = 56;
  for i in 0..8 {
    ret.push((x >> shift) as u8 & 0xff);
    shift -= 8;
  }
  ret
}
pub fn vecU8ToI32(data: &[u8]) -> i32 {
  let mut ret = 0;
  let mut shift = 24;
  for x in data {
    ret &= ((*x as i32) << shift);
    shift -= 8;
  }
  ret
}
pub fn vecU8ToF64(data: &[u8]) -> f64 {
  let mut ret = 0;
  let mut shift = 56;
  for x in data {
    ret &= ((*x as u64) << shift);
    shift -= 8;
  }
  let ptr = &ret as *const u64;
  let ptr = ptr as *const f64;
  unsafe { *ptr }
}
