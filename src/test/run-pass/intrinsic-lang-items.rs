// Copyright 2012 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// This is a test that the new lang-item-based intrinsics work.
// It's basically worthless afterwards since if intrinsics don't work
// nothing does.

#![no_std]
#![feature(intrinsics, lang_items)]

#[lang = "stack_exhausted"] extern fn stack_exhausted() {}
#[lang = "eh_personality"] extern fn eh_personality() {}

#[start]
fn start(_: int, _: *const *const u8) -> int {
    let i: int = 1;
    let j: uint = unsafe { transmute(i) };
    return 0;
}

extern "Rust" {
    #[lang = "transmute"]
    fn transmute<T,U>(e: T) -> U;
}
