use std::env;
use std::path::PathBuf;

fn main() {
  // Tell cargo to tell rustc to link the system bzip2
  // shared library.
  // FIXME: uncomment this
  println!("cargo:rustc-link-lib=DbEngineInterface");
  // FIXME: uncomment this
  println!("cargo:rustc-link-search=../lib");
}
