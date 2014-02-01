// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#[crate_id = "prim#0.10-pre"];
#[license = "MIT/ASL2"];
#[crate_type = "rlib"];
#[crate_type = "dylib"];
#[doc(html_logo_url = "http://www.rust-lang.org/logos/rust-logo-128x128-blk.png",
      html_favicon_url = "http://www.rust-lang.org/favicon.ico",
      html_root_url = "http://static.rust-lang.org/doc/master")];

#[no_std];
#[feature(globs)];
#[feature(phase)];
#[feature(macro_rules)];

#[cfg(test)] #[phase(syntax)] extern mod std;

#[cfg(test)] extern mod realprim = "prim";
#[cfg(test)] extern mod std;
#[cfg(test)] extern mod rustuv;
#[cfg(test)] extern mod green;
#[cfg(test)] extern mod native;

#[cfg(test)] pub use kinds = realprim::kinds;

pub mod cast;
pub mod intrinsics;
#[cfg(not(test))] pub mod kinds;
pub mod mem;
pub mod ptr;
pub mod tuple;

/**
 * Swap the values at two mutable locations of the same type, without
 * deinitialising or copying either one.
 */
#[inline]
pub fn swap<T>(x: &mut T, y: &mut T) {
    unsafe {
        // Give ourselves some scratch space to work with
        let mut tmp: T = intrinsics::uninit();
        let t: *mut T = &mut tmp;

        // Perform the swap, `&mut` pointers never alias
        let x_raw: *mut T = x;
        let y_raw: *mut T = y;
        ptr::copy_nonoverlapping_memory(t, &*x_raw, 1);
        ptr::copy_nonoverlapping_memory(x, &*y_raw, 1);
        ptr::copy_nonoverlapping_memory(y, &*t, 1);

        // y and t now point to the same thing, but we need to completely forget `tmp`
        // because it's no longer relevant.
        cast::forget(tmp);
    }
}

/**
 * Replace the value at a mutable location with a new one, returning the old
 * value, without deinitialising or copying either one.
 */
#[inline]
pub fn replace<T>(dest: &mut T, mut src: T) -> T {
    swap(dest, &mut src);
    src
}

#[cfg(test)]
mod tests {
    use super::{swap, replace};

    #[test]
    fn test_swap() {
        let mut x = 31337;
        let mut y = 42;
        swap(&mut x, &mut y);
        assert_eq!(x, 42);
        assert_eq!(y, 31337);
    }

    #[test]
    fn test_replace() {
        let mut x = 1;
        let y = replace(&mut x, 2);
        assert!(x == 2);
        assert!(y == 1);
    }
}