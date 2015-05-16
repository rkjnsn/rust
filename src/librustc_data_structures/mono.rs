// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::hash::{Hash, Hasher};

// A *globally* unique identifier of a monomorphized functions,
// a hash of the svh, node_id, and type substitutions.
#[derive(PartialEq, Eq, Debug, RustcDecodable, RustcEncodable, Copy, Clone)]
pub struct UniversalMonoId {
    hash: u64,
}

impl UniversalMonoId {
    pub fn new(crypto_hash: u64) -> UniversalMonoId {
        UniversalMonoId { hash: crypto_hash }
    }

    #[inline]
    pub fn to_u64(&self) -> u64 { self.hash }
}

impl Hash for UniversalMonoId {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        // FIXME: There's no need to rehash this since it's already a hash.
        // Could be wins from using a 'null' Hasher.
        self.hash.hash(state);
    }
}
