// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![feature(core_intrinsics)]
#![crate_name = "rtinst"]
#![no_std]
#![crate_type = "rlib"]

use core::sync::atomic::{AtomicUsize, ATOMIC_USIZE_INIT, Ordering};
use core::mem;
use core::intrinsics;

static HANDLER: AtomicUsize = ATOMIC_USIZE_INIT;

pub fn set_handler(f: fn(Event)) {
    HANDLER.store(f as usize, Ordering::SeqCst);
}

pub fn call(e: Event) {
    unsafe {
        let p = HANDLER.load(Ordering::SeqCst);
        if p != 0 {
            let f: fn(Event) = mem::transmute(p);
            f(e);
        }
    }
}

#[derive(Debug)]
pub struct TypeInfo {
    name: &'static str,
    size: usize,
}

pub fn ti<T>() -> TypeInfo {
    unsafe {
        TypeInfo {
            name: intrinsics::type_name::<T>(),
            size: intrinsics::size_of::<T>(),
        }
    }
}

#[derive(Debug)]
pub struct UnsizedTypeInfo {
    name: &'static str,
}

pub fn uti<T: ?Sized>() -> UnsizedTypeInfo {
    unsafe {
        UnsizedTypeInfo {
            name: intrinsics::type_name::<T>(),
        }
    }
}

#[derive(Debug)]
pub enum Event {
    // Alllocator
    Allocate { size: usize, align: usize, ptr: *const u8 },
    Reallocate { inptr: *const u8, old_size: usize, size: usize, align: usize, outptr: *const u8 },
    ReallocateInplace { ptr: *const u8, old_size: usize, size: usize, align: usize },
    Deallocate { ptr: *const u8, old_size: usize, align: usize },

    // Box
    BoxCreate { t: TypeInfo, ptr: *const u8 },
    BoxDrop { t: TypeInfo, ptr: *const u8 },

    // Rc
    RcCreate { t: TypeInfo, ptr: *const u8 },
    //RcRef { t: TypeInfo, ptr: *const u8 },
    //RcUnref { t: TypeInfo, ptr: *const u8 },
    RcDrop { t: UnsizedTypeInfo, ptr: *const u8 },

    // Arc
    ArcCreate { t: TypeInfo, ptr: *const u8 },
    //ArcRef { t: TypeInfo, ptr: *const u8 },
    //ArcUnref { t: TypeInfo, ptr: *const u8 },
    ArcDrop { t: UnsizedTypeInfo, ptr: *const u8 },

    // Vec
    VecCreate { t: TypeInfo, len: usize, capacity: usize, ptr: *const u8 },
    //VecLenChange { t: TypeInfo, len: usize, capacity: usize, ptr: *const u8 },
    VecResize { t: TypeInfo, len: usize, capacity: usize, old_ptr: *const u8, new_ptr: *const u8 },
    VecDrop { t: TypeInfo, len: usize, capacity: usize, ptr: *const u8 },

    // HashMap
    //HashMapCreate { t: TypeInfo, len: usize, capacity: usize, ptr: *const u8 },
    //HashMapLenChange { t: TypeInfo, len: usize, capacity: usize, ptr: *const u8 },
    //HashMapResize { t: TypeInfo, len: usize, capacity: usize, old_ptr: *const u8, new_ptr: *const u8 },
    //HashMapDrop { t: TypeInfo, ptr: *const u8 },

    // HashSet
    //HashSetCreate { t: TypeInfo, len: usize, capacity: usize, ptr: *const u8 },
    //HashSetLenChange { t: TypeInfo, len: usize, capacity: usize, ptr: *const u8 },
    //HashSetResize { t: TypeInfo, len: usize, capacity: usize, old_ptr: *const u8, new_ptr: *const u8 },
    //HashSetDrop { t: TypeInfo, ptr: *const u8 },

    // RefCell
    //RefCellBorrow { t: TypeInfo, ptr: *const u8 },
    //RefCellRefClone { t: TypeInfo, ptr: *const u8 },
    //RefCellUnborrow { t: TypeInfo, ptr: *const u8 },
    //RefCellBorrowMut { t: TypeInfo, ptr: *const u8 },
    //RefCellRefCloneMut { t: TypeInfo, ptr: *const u8 },
    //RefCellUnborrowMut { t: TypeInfo, ptr: *const u8 },
}

// TODO:
// * clone
// * iterators
// * transmute
