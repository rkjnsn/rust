// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#[crate_id = "std#0.10-pre"];
#[comment = "The Rust standard library"];
#[license = "MIT/ASL2"];
#[crate_type = "rlib"];
#[crate_type = "dylib"];
#[doc(html_logo_url = "http://www.rust-lang.org/logos/rust-logo-128x128-blk-v2.png",
      html_favicon_url = "http://www.rust-lang.org/favicon.ico",
      html_root_url = "http://static.rust-lang.org/doc/master")];
#[no_std];
#[feature(phase, macro_rules)];
#[allow(deprecated_owned_vector)];

#[phase(syntax, link)]
extern crate mini;

// When testing libstd, bring in libuv as the I/O backend so tests can print
// things and all of the std::io tests have an I/O interface to run on top
// of
#[cfg(test)] extern crate rustuv;
#[cfg(test)] extern crate native;
#[cfg(test)] extern crate green;

pub use mini::prelude;
pub use mini::int;
pub use mini::i8;
pub use mini::i16;
pub use mini::i32;
pub use mini::i64;
pub use mini::uint;
pub use mini::u8;
pub use mini::u16;
pub use mini::u32;
pub use mini::u64;
pub use mini::f32;
pub use mini::f64;
pub use mini::unit;
pub use mini::bool;
pub use mini::char;
pub use mini::tuple;
pub use mini::vec;
pub use mini::vec_ng;
pub use mini::str;
pub use mini::ascii;
pub use mini::ptr;
pub use mini::owned;
pub use mini::managed;
pub use mini::rc;
pub use mini::gc;
pub use mini::kinds;
pub use mini::ops;
pub use mini::cmp;
pub use mini::from_str;
pub use mini::num;
pub use mini::iter;
pub use mini::to_str;
pub use mini::clone;
pub use mini::hash;
pub use mini::container;
pub use mini::default;
pub use mini::any;
pub use mini::option;
pub use mini::result;
pub use mini::cell;
pub use mini::task;
pub use mini::comm;
pub use mini::local_data;
pub use mini::sync;
pub use mini::libc;
pub use mini::c_str;
pub use mini::os;
pub use mini::io;
pub use mini::path;
pub use mini::cast;
pub use mini::fmt;
pub use mini::cleanup;
pub use mini::logging;
pub use mini::mem;
pub use mini::repr;
pub use mini::reflect;
pub use mini::unstable;
pub use mini::intrinsics;
pub use mini::raw;
pub use mini::rt;

#[path = "../libmini/macros.rs"]
pub mod macros;

mod std {
    pub use os;
}
