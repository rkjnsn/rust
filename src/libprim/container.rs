// Copyright 2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

/// A trait to represent the abstract idea of a container. The only concrete
/// knowledge known is the number of elements contained within.
pub trait Container {
    /// Return the number of elements in the container
    fn len(&self) -> uint;

    /// Return true if the container contains no elements
    #[inline]
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

/// A trait to represent mutable containers
pub trait Mutable: Container {
    /// Clear the container, removing all values.
    fn clear(&mut self);
}

impl<'a, T> Container for &'a [T] {
    /// Returns the length of a vector
    #[inline]
    fn len(&self) -> uint {
        self.repr().len
    }
}

impl<T> Container for ~[T] {
    /// Returns the length of a vector
    #[inline]
    fn len(&self) -> uint {
        self.as_slice().len()
    }
}
