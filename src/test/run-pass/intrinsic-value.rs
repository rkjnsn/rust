#![feature(intrinsics)]

extern "rust-intrinsic" {
    fn atomic_fence();
}

extern "Rust" fn foo() { }

fn main() {
    let bar: unsafe extern "Rust" fn() = foo;
    let _baz: unsafe fn() = bar;
    let bar: unsafe extern "rust-intrinsic" fn() = atomic_fence;
    //let _baz: unsafe fn() = bar;
}
