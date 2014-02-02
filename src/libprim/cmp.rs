// Copyright 2012-2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

/*!

The `Ord` and `Eq` comparison traits

This module contains the definition of both `Ord` and `Eq` which define
the common interfaces for doing comparison. Both are language items
that the compiler uses to implement the comparison operators. Rust code
may implement `Ord` to overload the `<`, `<=`, `>`, and `>=` operators,
and `Eq` to overload the `==` and `!=` operators.

*/

#[allow(missing_doc)];

use cast;
use intrinsics::TypeId;
use ptr::RawPtr;
use tuple::{ImmutableTuple1, ImmutableTuple2, ImmutableTuple3, ImmutableTuple4};
use tuple::{ImmutableTuple5, ImmutableTuple6, ImmutableTuple7, ImmutableTuple8};
use tuple::{ImmutableTuple9, ImmutableTuple10, ImmutableTuple11, ImmutableTuple12};
use vec::Vector;

/**
* Trait for values that can be compared for equality and inequality.
*
* This trait allows partial equality, where types can be unordered instead of strictly equal or
* unequal. For example, with the built-in floating-point types `a == b` and `a != b` will both
* evaluate to false if either `a` or `b` is NaN (cf. IEEE 754-2008 section 5.11).
*
* Eq only requires the `eq` method to be implemented; `ne` is its negation by default.
*
* Eventually, this will be implemented by default for types that implement `TotalEq`.
*/
#[lang="eq"]
pub trait Eq {
    fn eq(&self, other: &Self) -> bool;

    #[inline]
    fn ne(&self, other: &Self) -> bool { !self.eq(other) }
}

impl Eq for TypeId {
    fn eq(&self, other: &TypeId) -> bool {
        self.t == other.t
    }
}

impl<T> Eq for *T {
    #[inline]
    fn eq(&self, other: &*T) -> bool {
        *self == *other
    }
    #[inline]
    fn ne(&self, other: &*T) -> bool { !self.eq(other) }
}

impl<T> Eq for *mut T {
    #[inline]
    fn eq(&self, other: &*mut T) -> bool {
        *self == *other
    }
    #[inline]
    fn ne(&self, other: &*mut T) -> bool { !self.eq(other) }
}

impl<_R> Eq for extern "C" fn() -> _R {
    #[inline]
    fn eq(&self, other: &extern "C" fn() -> _R) -> bool {
        let self_: *() = unsafe { cast::transmute(*self) };
        let other_: *() = unsafe { cast::transmute(*other) };
        self_ == other_
    }
    #[inline]
    fn ne(&self, other: &extern "C" fn() -> _R) -> bool {
        !self.eq(other)
    }
}

macro_rules! fnptreq(
    ($($p:ident),*) => {
        impl<_R,$($p),*> Eq for extern "C" fn($($p),*) -> _R {
            #[inline]
            fn eq(&self, other: &extern "C" fn($($p),*) -> _R) -> bool {
                let self_: *() = unsafe { cast::transmute(*self) };
                let other_: *() = unsafe { cast::transmute(*other) };
                self_ == other_
            }
            #[inline]
            fn ne(&self, other: &extern "C" fn($($p),*) -> _R) -> bool {
                !self.eq(other)
            }
        }
    }
)

fnptreq!(A)
fnptreq!(A,B)
fnptreq!(A,B,C)
fnptreq!(A,B,C,D)
fnptreq!(A,B,C,D,E)

impl<'a, T: Eq> Eq for &'a T {
    #[inline]
    fn eq(&self, other: & &'a T) -> bool {
        *(*self) == *(*other)
    }
    #[inline]
    fn ne(&self, other: & &'a T) -> bool {
        *(*self) != *(*other)
    }
}

impl<T:Eq> Eq for @T {
    #[inline]
    fn eq(&self, other: &@T) -> bool { *(*self) == *(*other) }
    #[inline]
    fn ne(&self, other: &@T) -> bool { *(*self) != *(*other) }
}

impl<T:Eq> Eq for ~T {
    #[inline]
    fn eq(&self, other: &~T) -> bool { *(*self) == *(*other) }
    #[inline]
    fn ne(&self, other: &~T) -> bool { *(*self) != *(*other) }
}


/// Trait for equality comparisons where `a == b` and `a != b` are strict inverses.
pub trait TotalEq {
    fn equals(&self, other: &Self) -> bool;
}

macro_rules! totaleq_impl(
    ($t:ty) => {
        impl TotalEq for $t {
            #[inline]
            fn equals(&self, other: &$t) -> bool { *self == *other }
        }
    }
)

totaleq_impl!(bool)

totaleq_impl!(u8)
totaleq_impl!(u16)
totaleq_impl!(u32)
totaleq_impl!(u64)

totaleq_impl!(i8)
totaleq_impl!(i16)
totaleq_impl!(i32)
totaleq_impl!(i64)

totaleq_impl!(int)
totaleq_impl!(uint)

totaleq_impl!(char)

impl<'a, T: TotalEq> TotalEq for &'a T {
    #[inline]
    fn equals(&self, other: & &'a T) -> bool { (**self).equals(*other) }
}

impl<T: TotalEq> TotalEq for @T {
    #[inline]
    fn equals(&self, other: &@T) -> bool { (**self).equals(*other) }
}

impl<T: TotalEq> TotalEq for ~T {
    #[inline]
    fn equals(&self, other: &~T) -> bool { (**self).equals(*other) }
}

pub enum Ordering { Less = -1, Equal = 0, Greater = 1 }

impl Eq for Ordering {
    fn eq(&self, other: &Ordering) -> bool {
        *self == *other
    }
}

/// Trait for types that form a total order
pub trait TotalOrd: TotalEq {
    fn cmp(&self, other: &Self) -> Ordering;
}

impl TotalEq for Ordering {
    #[inline]
    fn equals(&self, other: &Ordering) -> bool {
        *self == *other
    }
}
impl TotalOrd for Ordering {
    #[inline]
    fn cmp(&self, other: &Ordering) -> Ordering {
        (*self as int).cmp(&(*other as int))
    }
}

impl Ord for Ordering {
    #[inline]
    fn lt(&self, other: &Ordering) -> bool { (*self as int) < (*other as int) }
}

macro_rules! totalord_impl(
    ($t:ty) => {
        impl TotalOrd for $t {
            #[inline]
            fn cmp(&self, other: &$t) -> Ordering {
                if *self < *other { Less }
                else if *self > *other { Greater }
                else { Equal }
            }
        }
    }
)

totalord_impl!(u8)
totalord_impl!(u16)
totalord_impl!(u32)
totalord_impl!(u64)

totalord_impl!(i8)
totalord_impl!(i16)
totalord_impl!(i32)
totalord_impl!(i64)

totalord_impl!(int)
totalord_impl!(uint)

totalord_impl!(char)

impl<'a, T: TotalOrd> TotalOrd for &'a T {
    #[inline]
    fn cmp(&self, other: & &'a T) -> Ordering { (**self).cmp(*other) }
}

impl<T: TotalOrd> TotalOrd for @T {
    #[inline]
    fn cmp(&self, other: &@T) -> Ordering { (**self).cmp(*other) }
}

impl<T: TotalOrd> TotalOrd for ~T {
    #[inline]
    fn cmp(&self, other: &~T) -> Ordering { (**self).cmp(*other) }
}


/// Compares (a1, b1) against (a2, b2), where the a values are more significant.
pub fn cmp2<A:TotalOrd,B:TotalOrd>(
    a1: &A, b1: &B,
    a2: &A, b2: &B) -> Ordering
{
    match a1.cmp(a2) {
        Less => Less,
        Greater => Greater,
        Equal => b1.cmp(b2)
    }
}

/**
Return `o1` if it is not `Equal`, otherwise `o2`. Simulates the
lexical ordering on a type `(int, int)`.
*/
#[inline]
pub fn lexical_ordering(o1: Ordering, o2: Ordering) -> Ordering {
    match o1 {
        Equal => o2,
        _ => o1
    }
}

/**
* Trait for values that can be compared for a sort-order.
*
* Ord only requires implementation of the `lt` method,
* with the others generated from default implementations.
*
* However it remains possible to implement the others separately,
* for compatibility with floating-point NaN semantics
* (cf. IEEE 754-2008 section 5.11).
*/
#[lang="ord"]
pub trait Ord {
    fn lt(&self, other: &Self) -> bool;
    #[inline]
    fn le(&self, other: &Self) -> bool { !other.lt(self) }
    #[inline]
    fn gt(&self, other: &Self) -> bool {  other.lt(self) }
    #[inline]
    fn ge(&self, other: &Self) -> bool { !self.lt(other) }
}

impl<T> Ord for *T {
    #[inline]
    fn lt(&self, other: &*T) -> bool {
        *self < *other
    }
    #[inline]
    fn le(&self, other: &*T) -> bool {
        *self <= *other
    }
    #[inline]
    fn ge(&self, other: &*T) -> bool {
        *self >= *other
    }
    #[inline]
    fn gt(&self, other: &*T) -> bool {
        *self > *other
    }
}

impl<T> Ord for *mut T {
    #[inline]
    fn lt(&self, other: &*mut T) -> bool {
        *self < *other
    }
    #[inline]
    fn le(&self, other: &*mut T) -> bool {
        *self <= *other
    }
    #[inline]
    fn ge(&self, other: &*mut T) -> bool {
        *self >= *other
    }
    #[inline]
    fn gt(&self, other: &*mut T) -> bool {
        *self > *other
    }
}

impl<'a, T: Ord> Ord for &'a T {
    #[inline]
    fn lt(&self, other: & &'a T) -> bool {
        *(*self) < *(*other)
    }
    #[inline]
    fn le(&self, other: & &'a T) -> bool {
        *(*self) <= *(*other)
    }
    #[inline]
    fn ge(&self, other: & &'a T) -> bool {
        *(*self) >= *(*other)
    }
    #[inline]
    fn gt(&self, other: & &'a T) -> bool {
        *(*self) > *(*other)
    }
}

impl<T:Ord> Ord for @T {
    #[inline]
    fn lt(&self, other: &@T) -> bool { *(*self) < *(*other) }
    #[inline]
    fn le(&self, other: &@T) -> bool { *(*self) <= *(*other) }
    #[inline]
    fn ge(&self, other: &@T) -> bool { *(*self) >= *(*other) }
    #[inline]
    fn gt(&self, other: &@T) -> bool { *(*self) > *(*other) }
}

impl<T:Ord> Ord for ~T {
    #[inline]
    fn lt(&self, other: &~T) -> bool { *(*self) < *(*other) }
    #[inline]
    fn le(&self, other: &~T) -> bool { *(*self) <= *(*other) }
    #[inline]
    fn ge(&self, other: &~T) -> bool { *(*self) >= *(*other) }
    #[inline]
    fn gt(&self, other: &~T) -> bool { *(*self) > *(*other) }
}


/// The equivalence relation. Two values may be equivalent even if they are
/// of different types. The most common use case for this relation is
/// container types; e.g. it is often desirable to be able to use `&str`
/// values to look up entries in a container with `~str` keys.
pub trait Equiv<T> {
    fn equiv(&self, other: &T) -> bool;
}

impl<T> Equiv<*mut T> for *T {
    fn equiv(&self, other: &*mut T) -> bool {
        self.to_uint() == other.to_uint()
    }
}

impl<T> Equiv<*T> for *mut T {
    fn equiv(&self, other: &*T) -> bool {
        self.to_uint() == other.to_uint()
    }
}

#[inline]
pub fn min<T:Ord>(v1: T, v2: T) -> T {
    if v1 < v2 { v1 } else { v2 }
}

#[inline]
pub fn max<T:Ord>(v1: T, v2: T) -> T {
    if v1 > v2 { v1 } else { v2 }
}

macro_rules! int_cmp (($T:ty) => (

impl Ord for $T {
    #[inline]
    fn lt(&self, other: &$T) -> bool { return (*self) < (*other); }
}

impl Eq for $T {
    #[inline]
    fn eq(&self, other: &$T) -> bool { return (*self) == (*other); }
}

))

int_cmp!(int)
int_cmp!(i8)
int_cmp!(i16)
int_cmp!(i32)
int_cmp!(i64)


macro_rules! uint_cmp (($T:ty) => (

impl Ord for $T {
    #[inline]
    fn lt(&self, other: &$T) -> bool { (*self) < (*other) }
}

impl Eq for $T {
    #[inline]
    fn eq(&self, other: &$T) -> bool { return (*self) == (*other); }
}

))

uint_cmp!(uint)
uint_cmp!(u8)
uint_cmp!(u16)
uint_cmp!(u32)
uint_cmp!(u64)

impl Eq for f32 {
    #[inline]
    fn eq(&self, other: &f32) -> bool { (*self) == (*other) }
}

impl Ord for f32 {
    #[inline]
    fn lt(&self, other: &f32) -> bool { (*self) < (*other) }
    #[inline]
    fn le(&self, other: &f32) -> bool { (*self) <= (*other) }
    #[inline]
    fn ge(&self, other: &f32) -> bool { (*self) >= (*other) }
    #[inline]
    fn gt(&self, other: &f32) -> bool { (*self) > (*other) }
}

impl Eq for f64 {
    #[inline]
    fn eq(&self, other: &f64) -> bool { (*self) == (*other) }
}

impl Ord for f64 {
    #[inline]
    fn lt(&self, other: &f64) -> bool { (*self) < (*other) }
    #[inline]
    fn le(&self, other: &f64) -> bool { (*self) <= (*other) }
    #[inline]
    fn ge(&self, other: &f64) -> bool { (*self) >= (*other) }
    #[inline]
    fn gt(&self, other: &f64) -> bool { (*self) > (*other) }
}

impl Eq for () {
    #[inline]
    fn eq(&self, _other: &()) -> bool { true }
    #[inline]
    fn ne(&self, _other: &()) -> bool { false }
}

impl Ord for () {
    #[inline]
    fn lt(&self, _other: &()) -> bool { false }
}

impl TotalOrd for () {
    #[inline]
    fn cmp(&self, _other: &()) -> Ordering { Equal }
}

impl TotalEq for () {
    #[inline]
    fn equals(&self, _other: &()) -> bool { true }
}


impl Ord for bool {
    #[inline]
    fn lt(&self, other: &bool) -> bool {
        *self == false && *other != false
    }
}

impl TotalOrd for bool {
    #[inline]
    fn cmp(&self, other: &bool) -> Ordering {
        if *self == false && *other != false {
            Less
        } else if *self != false && *other == false {
            Greater
        } else {
            Equal
        }
    }
}

impl Eq for bool {
    #[inline]
    fn eq(&self, other: &bool) -> bool { (*self) == (*other) }
}

macro_rules! tuple_impls {
    ($( 
       {
            $(($get_ref_fn:ident) -> $T:ident;
            )+
        }
    )+) => {
        $(

            impl<$($T:Eq),+> Eq for ($($T,)+) {
                #[inline]
                fn eq(&self, other: &($($T,)+)) -> bool {
                    $(*self.$get_ref_fn() == *other.$get_ref_fn())&&+
                }
                #[inline]
                fn ne(&self, other: &($($T,)+)) -> bool {
                    $(*self.$get_ref_fn() != *other.$get_ref_fn())||+
                }
            }

            impl<$($T:TotalEq),+> TotalEq for ($($T,)+) {
                #[inline]
                fn equals(&self, other: &($($T,)+)) -> bool {
                    $(self.$get_ref_fn().equals(other.$get_ref_fn()))&&+
                }
            }

            impl<$($T:Ord + Eq),+> Ord for ($($T,)+) {
                #[inline]
                fn lt(&self, other: &($($T,)+)) -> bool {
                    lexical_ord!(lt, $(self.$get_ref_fn(), other.$get_ref_fn()),+)
                }
                #[inline]
                fn le(&self, other: &($($T,)+)) -> bool {
                    lexical_ord!(le, $(self.$get_ref_fn(), other.$get_ref_fn()),+)
                }
                #[inline]
                fn ge(&self, other: &($($T,)+)) -> bool {
                    lexical_ord!(ge, $(self.$get_ref_fn(), other.$get_ref_fn()),+)
                }
                #[inline]
                fn gt(&self, other: &($($T,)+)) -> bool {
                    lexical_ord!(gt, $(self.$get_ref_fn(), other.$get_ref_fn()),+)
                }
            }

            impl<$($T:TotalOrd),+> TotalOrd for ($($T,)+) {
                #[inline]
                fn cmp(&self, other: &($($T,)+)) -> Ordering {
                    lexical_cmp!($(self.$get_ref_fn(), other.$get_ref_fn()),+)
                }
            }
        )+
    }
}

// Constructs an expression that performs a lexical ordering using method $rel.
// The values are interleaved, so the macro invocation for
// `(a1, a2, a3) < (b1, b2, b3)` would be `lexical_ord!(lt, a1, b1, a2, b2,
// a3, b3)` (and similarly for `lexical_cmp`)
macro_rules! lexical_ord {
    ($rel: ident, $a:expr, $b:expr, $($rest_a:expr, $rest_b:expr),+) => {
        if *$a != *$b { lexical_ord!($rel, $a, $b) }
        else { lexical_ord!($rel, $($rest_a, $rest_b),+) }
    };
    ($rel: ident, $a:expr, $b:expr) => { (*$a) . $rel ($b) };
}

macro_rules! lexical_cmp {
    ($a:expr, $b:expr, $($rest_a:expr, $rest_b:expr),+) => {
        match ($a).cmp($b) {
            Equal => lexical_cmp!($($rest_a, $rest_b),+),
            ordering   => ordering
        }
    };
    ($a:expr, $b:expr) => { ($a).cmp($b) };
}

tuple_impls! {
    {
        (n0_ref) -> A;
    }

    {
        (n0_ref) -> A;
        (n1_ref) -> B;
    }

    {
        (n0_ref) -> A;
        (n1_ref) -> B;
        (n2_ref) -> C;
    }

    {
        (n0_ref) -> A;
        (n1_ref) -> B;
        (n2_ref) -> C;
        (n3_ref) -> D;
    }

    {
        (n0_ref) -> A;
        (n1_ref) -> B;
        (n2_ref) -> C;
        (n3_ref) -> D;
        (n4_ref) -> E;
    }

    {
        (n0_ref) -> A;
        (n1_ref) -> B;
        (n2_ref) -> C;
        (n3_ref) -> D;
        (n4_ref) -> E;
        (n5_ref) -> F;
    }

    {
        (n0_ref) -> A;
        (n1_ref) -> B;
        (n2_ref) -> C;
        (n3_ref) -> D;
        (n4_ref) -> E;
        (n5_ref) -> F;
        (n6_ref) -> G;
    }

    {
        (n0_ref) -> A;
        (n1_ref) -> B;
        (n2_ref) -> C;
        (n3_ref) -> D;
        (n4_ref) -> E;
        (n5_ref) -> F;
        (n6_ref) -> G;
        (n7_ref) -> H;
    }

    {
        (n0_ref) -> A;
        (n1_ref) -> B;
        (n2_ref) -> C;
        (n3_ref) -> D;
        (n4_ref) -> E;
        (n5_ref) -> F;
        (n6_ref) -> G;
        (n7_ref) -> H;
        (n8_ref) -> I;
    }

    {
        (n0_ref) -> A;
        (n1_ref) -> B;
        (n2_ref) -> C;
        (n3_ref) -> D;
        (n4_ref) -> E;
        (n5_ref) -> F;
        (n6_ref) -> G;
        (n7_ref) -> H;
        (n8_ref) -> I;
        (n9_ref) -> J;
    }

    {
        (n0_ref)  -> A;
        (n1_ref)  -> B;
        (n2_ref)  -> C;
        (n3_ref)  -> D;
        (n4_ref)  -> E;
        (n5_ref)  -> F;
        (n6_ref)  -> G;
        (n7_ref)  -> H;
        (n8_ref)  -> I;
        (n9_ref)  -> J;
        (n10_ref) -> K;
    }

    {
        (n0_ref)  -> A;
        (n1_ref)  -> B;
        (n2_ref)  -> C;
        (n3_ref)  -> D;
        (n4_ref)  -> E;
        (n5_ref)  -> F;
        (n6_ref)  -> G;
        (n7_ref)  -> H;
        (n8_ref)  -> I;
        (n9_ref)  -> J;
        (n10_ref) -> K;
        (n11_ref) -> L;
    }
}


impl Eq for char {
    #[inline]
    fn eq(&self, other: &char) -> bool { (*self) == (*other) }
}

impl Ord for char {
    #[inline]
    fn lt(&self, other: &char) -> bool { *self < *other }
}

impl<'a,T:Eq> Eq for &'a [T] {
    fn eq(&self, other: & &'a [T]) -> bool {
        self.len() == other.len() &&
            order::eq(*self, *other)
    }
}

impl<T:Eq> Eq for ~[T] {
    #[inline]
    fn eq(&self, other: &~[T]) -> bool { self.as_slice() == *other }
    #[inline]
    fn ne(&self, other: &~[T]) -> bool { !self.eq(other) }
}

impl<T:Eq> Eq for @[T] {
    #[inline]
    fn eq(&self, other: &@[T]) -> bool { self.as_slice() == *other }
    #[inline]
    fn ne(&self, other: &@[T]) -> bool { !self.eq(other) }
}

impl<'a,T:TotalEq> TotalEq for &'a [T] {
    fn equals(&self, other: & &'a [T]) -> bool {
        self.len() == other.len() &&
            order::equals(*self, *other)
    }
}

impl<T:TotalEq> TotalEq for ~[T] {
    #[inline]
    fn equals(&self, other: &~[T]) -> bool { self.as_slice().equals(&other.as_slice()) }
}

impl<T:TotalEq> TotalEq for @[T] {
    #[inline]
    fn equals(&self, other: &@[T]) -> bool { self.as_slice().equals(&other.as_slice()) }
}

impl<'a,T:Eq, V: Vector<T>> Equiv<V> for &'a [T] {
    #[inline]
    fn equiv(&self, other: &V) -> bool { self.as_slice() == other.as_slice() }
}

impl<'a,T:Eq, V: Vector<T>> Equiv<V> for ~[T] {
    #[inline]
    fn equiv(&self, other: &V) -> bool { self.as_slice() == other.as_slice() }
}

impl<'a,T:Eq, V: Vector<T>> Equiv<V> for @[T] {
    #[inline]
    fn equiv(&self, other: &V) -> bool { self.as_slice() == other.as_slice() }
}

impl<'a,T:TotalOrd> TotalOrd for &'a [T] {
    fn cmp(&self, other: & &'a [T]) -> Ordering {
        order::cmp(*self, *other)
    }
}

impl<T: TotalOrd> TotalOrd for ~[T] {
    #[inline]
    fn cmp(&self, other: &~[T]) -> Ordering { self.as_slice().cmp(&other.as_slice()) }
}

impl<T: TotalOrd> TotalOrd for @[T] {
    #[inline]
    fn cmp(&self, other: &@[T]) -> Ordering { self.as_slice().cmp(&other.as_slice()) }
}

impl<'a, T: Eq + Ord> Ord for &'a [T] {
    #[inline]
    fn lt(&self, other: & &'a [T]) -> bool {
        order::lt(*self, *other)
    }
}

impl<T: Eq + Ord> Ord for ~[T] {
    #[inline]
    fn lt(&self, other: &~[T]) -> bool { self.as_slice() < other.as_slice() }
    #[inline]
    fn le(&self, other: &~[T]) -> bool { self.as_slice() <= other.as_slice() }
    #[inline]
    fn ge(&self, other: &~[T]) -> bool { self.as_slice() >= other.as_slice() }
    #[inline]
    fn gt(&self, other: &~[T]) -> bool { self.as_slice() > other.as_slice() }
}

impl<T: Eq + Ord> Ord for @[T] {
    #[inline]
    fn lt(&self, other: &@[T]) -> bool { self.as_slice() < other.as_slice() }
    #[inline]
    fn le(&self, other: &@[T]) -> bool { self.as_slice() <= other.as_slice() }
    #[inline]
    fn ge(&self, other: &@[T]) -> bool { self.as_slice() >= other.as_slice() }
    #[inline]
    fn gt(&self, other: &@[T]) -> bool { self.as_slice() > other.as_slice() }
}

mod order {

    use super::{Eq, TotalEq, Ord, TotalOrd};
    use super::{Ordering, Less, Equal, Greater};

    fn eq<T: Eq>(this: &[T], other: &[T]) -> bool {
    }

    fn equals<T: TotalEq>(this: &[T], other: &[T]) -> bool {
    }

    fn lt<T: Ord>(this: &[T], other: &[T]) -> bool {
    }

    fn cmp<T: TotalOrd>(this: &[T], other: &[T]) -> Ordering {
    }
}

#[cfg(test)]
mod test {
    use super::lexical_ordering;

    #[test]
    fn test_int_totalord() {
        assert_eq!(5.cmp(&10), Less);
        assert_eq!(10.cmp(&5), Greater);
        assert_eq!(5.cmp(&5), Equal);
        assert_eq!((-5).cmp(&12), Less);
        assert_eq!(12.cmp(-5), Greater);
    }

    #[test]
    fn test_cmp2() {
        assert_eq!(cmp2(1, 2, 3, 4), Less);
        assert_eq!(cmp2(3, 2, 3, 4), Less);
        assert_eq!(cmp2(5, 2, 3, 4), Greater);
        assert_eq!(cmp2(5, 5, 5, 4), Greater);
    }

    #[test]
    fn test_int_totaleq() {
        assert!(5.equals(&5));
        assert!(!2.equals(&17));
    }

    #[test]
    fn test_ordering_order() {
        assert!(Less < Equal);
        assert_eq!(Greater.cmp(&Less), Greater);
    }

    #[test]
    fn test_lexical_ordering() {
        fn t(o1: Ordering, o2: Ordering, e: Ordering) {
            assert_eq!(lexical_ordering(o1, o2), e);
        }

        let xs = [Less, Equal, Greater];
        for &o in xs.iter() {
            t(Less, o, Less);
            t(Equal, o, o);
            t(Greater, o, Greater);
         }
    }
}
