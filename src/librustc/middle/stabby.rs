// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Stability level indexing
//!
//! This pass has two roles: first, to discover the stability index
//! of all applicable local AST nodes, for insertion into metadata;
//! second, to provide an interface for querying for the stability
//! level required of code in the local crate.

use middle::ty;
use std::hashmap::HashMap;
use syntax::ast;
use syntax::ast::{NodeId, Attribute, Crate, Item, ForeignItem};
use syntax::attr;
use syntax::attr::{Stability, AttributeMethods};
use syntax::visit;
use syntax::visit::Visitor;

pub struct StabilityIndex {
    /// The map of node ids to stability index, for inserting into.
    /// A `None` value indicates that there was explicit nor inherited
    /// stability level. Callers are free to interpret the absense of
    /// a stability level however they want.
    stab_map: HashMap<NodeId, Option<Stability>>
}

pub fn index_stability(tcx: ty::ctxt, krate: &Crate) -> StabilityIndex {

    struct Ctx {
        tcx: ty::ctxt,
        idx: StabilityIndex,
    }
    let idx = StabilityIndex { stab_map: HashMap::new() };
    let mut ctx = Ctx { tcx: tcx, idx: idx };

    let init_lvl = None;
    let lvl = record_level(&mut ctx, ast::CRATE_NODE_ID, init_lvl, krate.attrs);
    visit::walk_crate(&mut ctx, krate, lvl);

    return ctx.idx;

    impl Visitor<Option<Stability>> for Ctx {
        fn visit_item(&mut self, i: &Item, lvl: Option<Stability>) {
            let lvl = record_level(self, i.id, lvl, i.attrs);
            visit::walk_item(self, i, lvl);
        }

        fn visit_foreign_item(&mut self, i: &ForeignItem, lvl: Option<Stability>) {
            let lvl = record_level(self, i.id, lvl, i.attrs);
            visit::walk_foreign_item(self, i, lvl);
        }
    }

    fn record_level(ctx: &mut Ctx, id: NodeId, lvl: Option<Stability>,
                    attrs: &[Attribute]) -> Option<Stability> {
        let lvl = calc_level(lvl, attrs);
        ctx.idx.stab_map.insert(id, lvl.clone());
        lvl
    }

    fn calc_level(lvl: Option<Stability>,
                  attrs: &[Attribute]) -> Option<Stability> {
        // First check for an explicit attribute
        let s = attr::find_stability(attrs.iter().map(|a| a.meta()));
        if s.is_some() { return s }

        // Inherit the stability level.
        match lvl {
            None => None,
            Some(curr) => {
                // Note: not inheriting the reason text
                Some(Stability {
                    level: curr.level,
                    text: None
                })
            }
        }
    }
}

impl StabilityIndex {
}
