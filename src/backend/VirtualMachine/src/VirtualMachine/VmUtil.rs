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
    ret += (((*x as u32) & 0xff) << shift);
    shift -= 8;
  }
  ret as i32
}
pub fn vecU8ToF64(data: &[u8]) -> f64 {
  let mut ret = 0;
  let mut shift = 56;
  for x in data {
    ret += (((*x as u64) & 0xff) << shift);
    shift -= 8;
  }
  let ptr = &ret as *const u64;
  let ptr = ptr as *const f64;
  unsafe { *ptr }
}

#[cfg(test)]
mod VmUtilTest {
  use super::*;
  #[test]
  fn test_i32ToVecU8_work() {
    let x = 0x87654321u32 as i32;
    let v = vec![0x87u8, 0x65u8, 0x43u8, 0x21u8];
    assert_eq!(i32ToVecU8(x), v);
  }
  #[test]
  fn test_vecU8ToI32_work() {
    let x = 0x87654321u32 as i32;
    let v = vec![0x87u8, 0x65u8, 0x43u8, 0x21u8];
    assert_eq!(vecU8ToI32(&v), x);
  }
  #[test]
  fn test_f64ToVecU8_work() {
    let x = 1234567890 as f64;
    let v = vec![
      0x41u8, 0xd2u8, 0x65u8, 0x80u8, 0xb4u8, 0x80u8, 0x00u8, 0x00u8,
    ];
    assert_eq!(f64ToVecU8(x), v);
  }
  #[test]
  fn test_vecU8ToF64_work() {
    let x = 1234567890 as f64;
    let v = vec![
      0x41u8, 0xd2u8, 0x65u8, 0x80u8, 0xb4u8, 0x80u8, 0x00u8, 0x00u8,
    ];
    assert_eq!(x, vecU8ToF64(&v));
  }
}

#[cfg(test)]
pub mod VmTestUtils {
  use super::super::VmMem::VmMemString;
  use super::*;
  pub fn getVmMemStringI32(x: i32) -> VmMemString {
    VmMemString::new([vec![0u8, 4u8, VmMemString::MEM_FLAG_INT, 0], i32ToVecU8(x)].concat())
  }
  pub fn getVmMemStringF64(x: f64) -> VmMemString {
    VmMemString::new(
      [
        vec![0u8, 8u8, VmMemString::MEM_FLAG_DOUBLE, 0],
        f64ToVecU8(x),
      ]
      .concat(),
    )
  }
  pub fn getVmMemStringStringA(x: usize) -> VmMemString {
    VmMemString::new(
      [
        vec![
          ((x & 0xff00) >> 8) as u8,
          ((x & 0xff) as u8),
          VmMemString::MEM_FLAG_STRING,
          0,
        ],
        vec!['a' as u8; x],
      ]
      .concat(),
    )
  }
  pub fn getVmMemStringStringB(x: usize) -> VmMemString {
    VmMemString::new(
      [
        vec![
          ((x & 0xff00) >> 8) as u8,
          ((x & 0xff) as u8),
          VmMemString::MEM_FLAG_STRING,
          0,
        ],
        vec!['b' as u8; x],
      ]
      .concat(),
    )
  }
  pub fn getVmMemStringNull() -> VmMemString {
    VmMemString::new(vec![0u8, 0, VmMemString::MEM_FLAG_NULL, 0])
  }
  pub fn getManyVmMemString(datas: Vec<VmMemString>) -> VmMemString {
    VmMemString::new(
      datas
        .iter()
        .map(|x| (**x).clone())
        .collect::<Vec<Vec<u8>>>()
        .concat(),
    )
  }
}
