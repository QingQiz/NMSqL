fn main() {
  // Tell cargo to tell rustc to link the system bzip2
  // shared library.
  // FIXME: uncomment this
  println!("cargo:rustc-link-lib=DbEngine");
  // FIXME: uncomment this
  println!("cargo:rustc-link-search=../lib");
}
