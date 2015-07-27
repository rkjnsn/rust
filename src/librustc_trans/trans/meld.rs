// Copyright 2012-2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Monomorphized function melding
//!
//! This module attempts to reuse previous monomorphizations of a
//! function (with one set of type substitutions) to implement later
//! monomorphizations of that function (with a *different* set of type
//! substitutions).
//!
//! It is mainly targeting the many small abstractions in the core and
//! standard libraries that are reinstantiated thousands of times in
//! any given code base.
//!
//! Externally, the melder has a minimal interface made up of the
//! `try_meld` and `register` functions. These plug into the
//! monomorphizer as a cache, first looking up whether the function
//! already exists with `try_meld`, then registering new functions in
//! the meld cache with `register`.
//!
//! It operates in two phases:
//!
//! 1. Traverse the generic function looking for patterns to reject
//! and extracting information to be used in the next phase.
//! 2. Hash various properties of the type substitutions. Two
//! instances with the the same hash can be melded. The main hashed
//! properties are the various sizes and alignments, but also LLVM
//! type class, calling conventions, fat-ptrness and others.
//!
//! (The existing implementation does not have this strict division).
//!
//! Both phases are recursive, with the melder needing to examine all
//! generic callees within the body of a meldable function.
//!
//! When a function is melded the caller simply casts the function
//! pointer to the correct signature before calling it. It's unknown
//! how LLVM's optimizers treat this pattern.
//!
//! Known problems:
//!
//! * This doesn't account for enum representation optimizations,
//!   so e.g. if a function uses Option<T> in the function body
//!   two different Option<T>'s might have different representations
//!   even if T has the same.
//! * Melding depends a lot on type params not being dropped, and
//!   right now that can't be determined automatically but has to
//!   be hinted with #[meld_hint(no_drop)].

use llvm::ValueRef;
use llvm;
use metadata::csearch;
use middle::def;
use middle::subst::{self, Subst};
use middle::ty::{self, Ty};
use middle::traits;
use rustc::ast_map;
use std::hash::{Hasher, Hash, SipHasher};
use std::result::Result as StdResult;
use syntax::parse::token::{self, special_idents};
use syntax::{ast, abi};
use syntax::attr::AttrMetaMethods;
use trans::context::CrateContext;
use trans::{common, type_of, inline, machine, monomorphize, meth};
use trans::common::{ExprId, MethodCallKey};
use trans::monomorphize::MonoId;
use util::nodemap::{FnvHashMap, FnvHashSet};

pub enum Result {
    Melded(ValueRef),
    Unmelded(MeldHash)
}

pub type MeldHash = StdResult<u64, Error>;

pub type Error = &'static str;

/// Attempt to reuse a previously-translated instance of a generic
/// function for a second set of type substitutions. If the meld
/// attempt results in a cache hit it returns a `Result` of `Melded`,
/// containing the LLVM function to call. This function must be
/// bitcast to the correct signature before calling. If the function
/// cannot be implemented by a previously-translated instance then it
/// returns `Unmelded` as the `Result`, which must later be passed
/// back to the `register` function to register the monomporphized
/// function with the function melder.
pub fn try_meld<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>,
                          fn_id: ast::DefId,
                          params: &'tcx subst::Substs<'tcx>) -> Result {
    let debug_string: &Fn() -> String = &|| {
        format!("{} % {:?}", ccx.tcx().item_path_str(fn_id), params.types)
    };

    if !ccx.meld_state().borrow_mut().enabled {
        return Result::Unmelded(Err("melding disabled"));
    }

    // Calculate the meld hash then look in up in the cache
    match calc_meld_hash(ccx, fn_id, params) {
        Ok(hash) => {
            info!("good meld hash: {} % {}", debug_string(), hash);
            if let Some(val) = lookup(ccx, hash) {
                info!("cache hit: {}", debug_string());
                Result::Melded(val)
            } else {
                info!("cache miss: {} % {}", debug_string(), hash);
                Result::Unmelded(Ok(hash))
            }
        }
        Err(e) => {
            info!("bad meld hash: {} % {}", debug_string(), e);
            Result::Unmelded(Err(e))
        }
    }
}

fn calc_meld_hash<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>,
                            fn_id: ast::DefId,
                            params: &'tcx subst::Substs<'tcx>) -> MeldHash {

    debug_assert!(fn_id.krate == ast::LOCAL_CRATE); // Melding only works on monomorphizations

    let debug_string: &Fn() -> String = &|| {
        format!("{} % {:?}", ccx.tcx().item_path_str(fn_id), params.types)
    };

    info!("running meld for {}", debug_string());

    if !should_try_meld(ccx, fn_id) {
        info!("not melding fn. not meld crate");
        return Err("non-meld fn");
    } else {
        info!("trying to meld fn");
    }

    if !ccx.meld_state().borrow_mut().wip.insert(fn_id.node) {
        return Err("recursive meld");
    }

    let result = calc_meld_hash2(ccx, fn_id, params);

    cache_meld(ccx, fn_id, params, result);

    let r = ccx.meld_state().borrow_mut().wip.remove(&fn_id.node);
    debug_assert!(r);

    return result;
}

fn should_try_meld<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>,
                             fn_id: ast::DefId) -> bool {
    // Only items from crates with the meld_functions feature get melded
    let meld_crate = match ccx.external_srcs().borrow().get(&fn_id.node) {
        Some(id) => {
            debug_assert!(id.krate != ast::LOCAL_CRATE);

            csearch::is_meld_func_crate(&ccx.sess().cstore, id.krate)
        }
        None => {
            ccx.sess().features.borrow().meld_functions
        }
    };

    let try_meld = meld_crate && {
        let attrs = ccx.tcx().map.attrs(fn_id.node);

        let mut meld_fn = false;
        for attr in attrs {
            if attr.check_name("meld_hint") {
                meld_fn = true;
                break;
            }
        }

        meld_fn
    };
    
    return try_meld || ccx.meld_state().borrow().always_meld;
}

fn calc_meld_hash2<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>,
                             fn_id: ast::DefId,
                             params: &subst::Substs<'tcx>) -> MeldHash {

    // Figure out if the structure of the function is suitable
    let (res, facts) = check_structure(ccx, fn_id, params);
    
    // Sanity checks
    try!(check_expectations(ccx, fn_id, &res, facts));

    let hash_state = try!(res);

    // Figure out if the type substitutions are suitable
    try!(check_types(ccx, params, facts));

    // Hash the function id and type substs to get a uniqe meld id for
    // this instance
    let hash = hash_types(ccx, hash_state, fn_id, params);

    Ok(hash)
}

fn check_structure<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>,
                             fn_id: ast::DefId,
                             params: &subst::Substs<'tcx>)
                             -> (StdResult<SipHasher, Error>, Facts) {
    let mut examiner = Examiner {
        ccx: ccx,
        hash_state: SipHasher::new(),
        fn_id: fn_id,
        params: params,
        facts: Facts::new()
    };

    let res = examiner.examine_structure();

    let Examiner { hash_state, facts, .. } = examiner;

    if let Err(e) = res {
        return (Err(e), facts);
    }

    (Ok(hash_state), facts)
}

fn check_expectations<'a, 'tcx>(_ccx: &CrateContext<'a, 'tcx>,
                                _fn_id: ast::DefId,
                                res: &StdResult<SipHasher, Error>,
                                facts: Facts) -> StdResult<(), Error> {
    if facts.expect_meld && !res.is_ok() {
        //ccx.sess().warn(&format!("expected meld failed for {} % {:?} % {:?}", common_name, params, res.err()));
    }
    if !facts.expect_meld && res.is_ok() {
        return Err("unexpected meld");
    }

    Ok(())
}

fn check_types<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>,
                         params: &subst::Substs<'tcx>,
                         facts: Facts) -> StdResult<(), Error> {
    let tycheck = TyCheck {
        ccx: ccx,
        params: params,
        facts: facts
    };

    try!(tycheck.check_types());

    Ok(())
}

fn recursive_calc_meld_hash<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>,
                                      fn_id: ast::DefId,
                                      params: &'tcx subst::Substs<'tcx>) -> MeldHash {
    let debug_string: &Fn() -> String = &|| {
        format!("{} % {:?}", ccx.tcx().item_path_str(fn_id), params.types)
    };
    info!("recursing {}", debug_string());
    if params.types.is_empty() {
        info!("no type substs");
        // This is not a generic function. It is meldable.
        return Ok(0);
    }

    let fn_id = inline::maybe_instantiate_inline(ccx, fn_id);
    
    if fn_id.krate != ast::LOCAL_CRATE {
        return Err("uninlinable recursive meld");
    }

    // Don't do work if it's already been done. This is only done on
    // recursive melds because monomorphize is responsible for
    // shortcircuiting the main meld function.
    if let Some(h) = get_cached_meld(ccx, fn_id, params) {
        info!("recursive cache-hit: {}", debug_string());
        return h;
    } else {
        info!("recursive cache-miss: {}", debug_string());
    }
    
    let r = calc_meld_hash(ccx, fn_id, params);

    if r.is_ok() {
        let hash = r.as_ref().map(|&h| h).unwrap();
        info!("recursive meldhash is ok {}", hash);
    } else {
        info!("recursive meldhash is bad");
    }

    r
}

pub fn register<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>,
                          hash: MeldHash,
                          llfn: ValueRef) {
    match hash {
        Ok(hash) => {
            insert(ccx, hash, llfn);
        }
        Err(_) => ()
    }
}

fn lookup<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>, hash: u64) -> Option<ValueRef> {
    ccx.meld_state().borrow().melds.get(&hash).map(|&llfn| llfn)
}

fn insert<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>, hash: u64, llfn: ValueRef) {
    assert!(ccx.meld_state().borrow_mut().melds.insert(hash, llfn).is_none());
}

fn get_cached_meld<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>,
                             fn_id: ast::DefId,
                             params: &subst::Substs<'tcx>) -> Option<MeldHash> {
    let id = MonoId {
        def: fn_id,
        params: &params.types
    };

    ccx.meld_state().borrow().cache.get(&id).map(|&h| h)
}

fn cache_meld<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>,
                        fn_id: ast::DefId,
                        params: &'tcx subst::Substs<'tcx>,
                        hash: MeldHash) {
    let id = MonoId {
        def: fn_id,
        params: &params.types
    };

    let _r = ccx.meld_state().borrow_mut().cache.insert(id, hash);

    // FIXM: This isn't true. May be a bug in monomorphize/MonoId.
    //assert!(r.is_none());
}

struct Examiner<'a, 'tcx: 'a> {
    ccx: &'a CrateContext<'a, 'tcx>,
    hash_state: SipHasher,
    fn_id: ast::DefId,
    params: &'a subst::Substs<'tcx>,
    facts: Facts
}

#[derive(Copy, Clone)]
struct Facts {
    uses_type_params: bool,
    expect_meld: bool,
    no_drop: bool,
}

impl<'a, 'tcx> Examiner<'a, 'tcx> {
    fn examine_structure(&mut self) -> StdResult<(), Error> {
        info!("walking function");
        try!(self.examine_fn_id());
        info!("checking predicates");
        try!(self.examine_predicates());
        Ok(())
    }

    fn examine_fn_id(&mut self) -> StdResult<(), Error> {
        let map_node = self.ccx.tcx().map.find(self.fn_id.node).unwrap();

        try!(self.examine_fn_attrs(&map_node));

        try!(self.bail_if_not_melding());

        match map_node {
            ast_map::NodeItem(i) => match *i {
                ast::Item {
                    node: ast::ItemFn(ref decl, _, _, abi, _, ref body), ..
                } =>  {
                    try!(self.examine_fn(decl, abi, body));
                },
                _ => return Err("unknown item type")
            },
            ast_map::NodeTraitItem(i) => match i.node {
                ast::MethodTraitItem(ref sig, Some(ref block)) => {
                    try!(self.examine_fn(&sig.decl, sig.abi, block));
                    try!(self.look_for_simple_method_wrapper(&sig.decl, block));
                }
                _ => {
                    return Err("unsupported trait item")
                }
            },
            ast_map::NodeImplItem(i) => match i.node {
                ast::MethodImplItem(ref sig, ref block) => {
                    try!(self.examine_fn(&sig.decl, sig.abi, block));
                    try!(self.look_for_simple_method_wrapper(&sig.decl, block));
                }
                _ => {
                    return Err("unsupported impl item")
                }
            },
            _ => return Err("unknown node type")
        };

        Ok(())
    }

    fn examine_fn_attrs(&mut self, node: &ast_map::Node) -> StdResult<(), Error> {
        let attrs = match *node {
            ast_map::NodeItem(ref i) => &i.attrs,
            ast_map::NodeTraitItem(ref i) => &i.attrs,
            ast_map::NodeImplItem(ref i) => &i.attrs,
            _ => return Err("unknown node type")
        };

        // Look for the 'no_drop' hint because we're not smart enough
        // to see that typarams are not being dropped.
        for attr in attrs {
            if attr.check_name("meld_hint") {
                self.facts.expect_meld = true;
                for item in attr.meta_item_list().unwrap_or(&[]) {
                    if item.check_name("no_drop") {
                        self.facts.no_drop = true;
                    }
                }
            }
        }

        Ok(())
    }

    fn bail_if_not_melding(&mut self) -> StdResult<(), Error> {
        if !self.facts.expect_meld && !self.ccx.meld_state().borrow().always_meld {
            return Err("non-meld-hint fn");
        }

        Ok(())
    }
    
    fn examine_fn(&mut self,
                  decl: &'a ast::FnDecl,
                  abi: abi::Abi,
                  body: &ast::Block) -> StdResult<(), Error> {

        try!(self.examine_block(body));

        let ast::FnDecl { inputs: _, output: _, variadic } = *decl;

        if variadic { return Err("variadic") }
        if abi != abi::Abi::Rust { return Err("non-rust abi") }

        Ok(())
    }

    fn examine_block(&mut self, block: &ast::Block) -> StdResult<(), Error> {
        let ast::Block { ref stmts, ref expr, id: _, rules: _, span: _ } = *block;
        for stmt in stmts {
            match stmt.node {
                ast::StmtDecl(ref decl, _) => {
                    match decl.node {
                        ast::DeclLocal(ref local) => {
                            if let Some(ref init_expr) = local.init {
                                try!(self.examine_expr(init_expr));
                            } else {
                                return Err("let without init")
                            }
                        }
                        _ => return Err("item decl")
                    }
                }
                ast::StmtSemi(ref expr, _) => {
                    try!(self.examine_expr(expr));
                }
                _ => return Err("statement kind")
            }
        }
        match *expr {
            None => return Err("no tail expr"),
            Some(ref expr) => {
                try!(self.examine_expr(expr));
            }
        }

        Ok(())
    }
    
    fn examine_expr(&mut self, expr: &ast::Expr) -> StdResult<(), Error> {
        info!("{:?}", expr);
        let tcx = self.ccx.tcx();
        match expr.node {
            ast::ExprCall(ref callee, ref args) => {
                try!(self.examine_callee(callee));
                for arg in args {
                    try!(self.examine_expr(&**arg));
                }
            }
            ast::ExprBlock(ref b) => {
                try!(self.examine_block(b));
            }
            ast::ExprBinary(ref b, ref expr_a, ref expr_b) if b.node == ast::BiEq => {
                try!(self.examine_expr(&**expr_a));
                try!(self.examine_expr(&**expr_b));
            }
            ast::ExprCast(ref expr, _) |
            ast::ExprField(ref expr, _) |
            ast::ExprTupField(ref expr, _) => {
                try!(self.examine_expr(&**expr));
            }
            ast::ExprMethodCall(_, _, ref exprs) => {
                info!("expr method call");
                // This is duplicated from meth.rs
                let method_call = ty::MethodCall::expr(expr.id);
                let method = tcx.tables.borrow().method_map[&method_call];
                match tcx.impl_or_trait_item(method.def_id).container() {
                    ty::ImplContainer(_) => {
                        return Err("impl container")
                    }
                    ty::TraitContainer(trait_def_id) => {
                        let trait_substs = method.substs.clone().method_to_trait();
                        let trait_substs = tcx.mk_substs(trait_substs);
                        let ref trait_ref = ty::TraitRef::new(trait_def_id, trait_substs);
                        let trait_ref = ty::Binder(monomorphize::apply_param_substs(tcx,
                                                                                    self.params,
                                                                                    trait_ref));
                        let span = tcx.map.span(method_call.expr_id);
                        let origin = common::fulfill_obligation(self.ccx,
                                                                span,
                                                                trait_ref.clone());
                        match origin {
                            traits::VtableImpl(vtable_impl) => {
                                let impl_id = vtable_impl.impl_def_id;
                                let mname = match tcx.impl_or_trait_item(method.def_id) {
                                    ty::MethodTraitItem(method) => method.name,
                                    _ => {
                                        tcx.sess.bug("non-method trait item");
                                    }
                                };
                                let mth_id = meth::method_with_name(self.ccx, impl_id, mname);
                                let callee_substs =
                                    meth::combine_impl_and_methods_tps(
                                        self.ccx, MethodCallKey(method_call), self.params, vtable_impl.substs);

                                // Deal with default methods. Copied from trans_fn_ref_with_substs
                                let (id, substs) = match tcx.provided_source(mth_id) {
                                    None => {
                                        let callee_substs = tcx.mk_substs(callee_substs);
                                        (mth_id, callee_substs)
                                    }
                                    Some(source_id) => {
                                        match tcx.impl_or_trait_item(source_id) {
                                            ty::MethodTraitItem(method) => {
                                                let trait_ref = tcx.impl_trait_ref(impl_id).unwrap();
                                                // Compute the first substitution
                                                let first_subst =
                                                    tcx.make_substs_for_receiver_types(&trait_ref, &*method).erase_regions();
                                                // And compose them
                                                let new_substs = tcx.mk_substs(first_subst.subst(tcx, &callee_substs));
                                                (source_id, new_substs)
                                            }
                                            _ => unreachable!()
                                        }
                                    }
                                };
                                
                                let callee_hash = try!(recursive_calc_meld_hash(self.ccx, id, substs));
                                callee_hash.hash(&mut self.hash_state);
                            }
                            _ => return Err("vtable")
                        }
                    }                        
                }
                
                for expr in exprs {
                    try!(self.examine_expr(&**expr));
                }
            }
            ast::ExprAddrOf(_, ref expr) => {
                try!(self.examine_expr(&**expr));
            }
            ast::ExprStruct(_, ref fields, ref fru_expr) => {
                for field in fields {
                    try!(self.examine_expr(&*field.expr));
                }

                if let Some(ref e) = *fru_expr {
                    try!(self.examine_expr(&**e));
                }
            }
            ast::ExprPath(None, _) | ast::ExprLit(_) => { }
            _ => return Err("unknown expression")
        }

        Ok(())
    }

    // Identifies methods that look like `fn f(self) { self }`,
    // like `iter::I.IntoIterator::into_iter`
    fn look_for_simple_method_wrapper(&mut self,
                                      decl: &ast::FnDecl,
                                      body: &ast::Block) -> StdResult<(), Error> {
        let mut simple_wrapper = true;

        info!("{}, {}, {}", decl.inputs.len(), body.stmts.len(), tail_returns_self(body));
        simple_wrapper &= decl.inputs.len() == 1;
        simple_wrapper &= body.stmts.len() == 0;
        simple_wrapper &= tail_returns_self(body);

        if simple_wrapper {
            info!("simple wrapper does not use type params");
            self.facts.uses_type_params = false;
        } else {
            info!("not a simple wrapper");
        }

        return Ok(());

        fn tail_returns_self(body: &ast::Block) -> bool {
            match body.expr {
                None => false,
                Some(ref expr) => {
                    match expr.node {
                        ast::ExprPath(None, ref p) => {
                            if p.segments.len() != 1 { return false }
                            let ast::PathSegment { ref identifier, ref parameters } = p.segments[0];
                            match *parameters {
                                ast::AngleBracketedParameters(ref d) => {
                                    if !d.lifetimes.is_empty() || !d.types.is_empty() || !d.bindings.is_empty() {
                                        return false
                                    }
                                }
                                ast::ParenthesizedParameters(ref d) => {
                                    if !d.inputs.is_empty() || d.output.is_some() {
                                        return false
                                    }
                                }
                            }
                            if identifier.name != special_idents::self_.name { return false }

                            true
                        }
                        _ => false
                    }
                }
            }
        }
    }

    fn examine_callee(&mut self, callee: &ast::Expr) -> StdResult<(), Error> {
        let tcx = self.ccx.tcx();

        match callee.node {
            ast::ExprPath(..) => (),
            _ => return Err("expr callee kind")
        }

        let def = tcx.def_map.borrow().get(&callee.id).unwrap().full_def();
        let callee_ty = tcx.node_id_to_type(callee.id);
        let callee_ty = monomorphize::apply_param_substs(tcx, self.params, &callee_ty);
        info!("def {:?}", def);
        match def {
            def::DefFn(did, _) => {
                match callee_ty.sty {
                    ty::TyBareFn(_, ref f) if f.abi == abi::RustIntrinsic => {
                        let local_did = inline::maybe_instantiate_inline(self.ccx, did);
                        let item = tcx.map.expect_foreign_item(local_did.node);
                        let name = token::get_ident(item.ident);
                        if name != "uninit"
                            && name != "copy_nonoverlapping"
                            && name != "size_of"
                            && name != "forget"
                            && name != "align_of"
                            && name != "min_align_of"
                            && name != "offset"
                            && name != "move_val_init"
                            && name != "size_of_val"
                            && name != "align_of_val"
                            && name != "transmute" {
                            return Err("unknown intrinsic");
                        }
                    }
                    ty::TyBareFn(_, _) => {
                        let fn_id = did;
                        let params = common::node_id_substs(self.ccx, ExprId(callee.id), self.params);
                        let ref params = tcx.mk_substs(params);
                        let callee_hash = try!(recursive_calc_meld_hash(self.ccx, fn_id, params));
                        callee_hash.hash(&mut self.hash_state);
                    }
                    _ => {
                        return Err("callee kind");
                    }
                }
            }
            _ => {
                return Err("non-fn callee");
            }
        }

        Ok(())
    }

    fn examine_predicates(&mut self) -> StdResult<(), Error> {
        if !self.facts.uses_type_params {
            info!("fn does not use type params. ignoring predicates");
            return Ok(())
        }

        let preds = try!(self.lookup_predicates());
        for pred in &preds.predicates {
            match *pred {
                ty::Predicate::Trait(ty::Binder(ref p)) => {
                    info!("trait predicate: {:?}", p);
                    let good = self.is_self_predicate(p) || self.is_sized_trait(p);
                    if !good {
                        return Err("predicate analysis")
                    }
                }
                _ => {
                    return Err("predicate kind")
                }
            }
        }

        Ok(())
    }

    fn is_self_predicate(&mut self, p: &ty::TraitPredicate<'tcx>) -> bool {
        // FIXME: Is it ok to let all Self predicates through?
        if let Some(ref t) = p.trait_ref.substs.self_ty() {
            t.is_self()
        } else {
            false
        }
    }

    fn is_sized_trait(&mut self, p: &ty::TraitPredicate<'tcx>) -> bool {
        let trait_def = p.def_id();
        match self.ccx.tcx().lang_items.sized_trait() {
            Some(d) => trait_def == d,
            None => false
        }
    }

    fn lookup_predicates(&mut self) -> StdResult<ty::GenericPredicates<'tcx>, Error> {
        let external_id = self.ccx.external_srcs().borrow().get(&self.fn_id.node).map(|i| *i);

        let real_id = if let Some(external_id) = external_id {
            info!("looking up external id for method");
            external_id
        } else {
            info!("using local id for method");
            self.fn_id
        };

        let tcx = self.ccx.tcx();
        match tcx.map.find(self.fn_id.node) {
            Some(ast_map::NodeTraitItem(ref trait_item)) => {
                match trait_item.node {
                    ast::MethodTraitItem(_, _) => {
                        match tcx.impl_or_trait_item(real_id) {
                            ty::MethodTraitItem(ref method_ty) => {
                                Ok(method_ty.predicates.clone())
                            }
                            _ => tcx.sess.bug("non-method in method-lookup")
                        }
                    }
                    _ => return Err("predicate lookup")
                }
            }
            Some(ast_map::NodeImplItem(ref impl_item)) => {
                match impl_item.node {
                    ast::MethodImplItem(_, _) => {
                        match tcx.impl_or_trait_item(real_id) {
                            ty::MethodTraitItem(ref method_ty) => {
                                Ok(method_ty.predicates.clone())
                            }
                            _ => tcx.sess.bug("non-method in method-lookup")
                        }
                    }
                    _ => return Err("predicate lookup")
                }
            }
            Some(ast_map::NodeItem(_)) => {
                Ok(tcx.lookup_predicates(real_id))
            }
            _ => return Err("predicate lookup")
        }
    }

}

struct TyCheck<'a, 'tcx: 'a> {
    ccx: &'a CrateContext<'a, 'tcx>,
    params: &'a subst::Substs<'tcx>,
    facts: Facts
}

impl<'a, 'tcx> TyCheck<'a, 'tcx> {
    fn check_types(&self) -> StdResult<(), Error> {
        for param in &self.params.types {
            try!(self.check_type(*param));
        }

        try!(self.check_types_recursive());

        Ok(())
    }

    fn check_type(&self, param: Ty<'tcx>) -> StdResult<(), Error> {
        if !self.facts.no_drop {
            if common::type_needs_drop(self.ccx.tcx(), param) {
                return Err("needs drop");
            }
        }

        let llty = type_of::type_of(self.ccx, param);

        let kind = llty.kind();
        match kind {
            llvm::Pointer |
            llvm::Void | llvm::Float | llvm::Double |
            llvm::Integer | llvm::Struct => {
                /* ok */
            }
            _ => {
                /* Short-circuit uncommon kinds we don't want to think about yet */
                return Err("unusual llvm type kind");
            }
        }

        Ok(())
    }

    fn check_types_recursive(&self) -> StdResult<(), Error> {
        Ok(())
    }
}

fn hash_types<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>,
                        ref mut hash_state: SipHasher,
                        fn_id: ast::DefId,
                        params: &subst::Substs<'tcx>) -> u64 {
    fn_id.node.hash(hash_state);

    for param in &params.types {
        hash_type(ccx, hash_state, *param);
    }

    hash_state.finish()
}

fn hash_type<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>,
                       hash_state: &mut SipHasher,
                       param: Ty<'tcx>) {
    let llty = type_of::type_of(ccx, param);

    let kind = llty.kind();
    kind.hash(hash_state);
    
    let size_store = machine::llsize_of_store(ccx, llty);
    let size_alloc = machine::llsize_of_alloc(ccx, llty);
    let size_real = machine::llsize_of_real(ccx, llty);

    // Try to hash two bools instead of of the three size ints:
    // The *vast* majority of types have the same value for all these,
    // and also have size that fits into a byte. If that's the case
    // just hash the size cast to u8 and the flag indicating this
    // branch was taken.
    let hash_opt_flag = if size_store == size_alloc && size_store == size_real && size_store < 256 {
        info!("yes doing size hash opt");
        (size_store as u8).hash(hash_state);
        true
    } else {
        info!("not doing size hash opt");
        size_store.hash(hash_state);
        size_alloc.hash(hash_state);
        size_real.hash(hash_state);
        false
    };
    
    let align_min = machine::llalign_of_min(ccx, llty);
    let align_pref = machine::llalign_of_pref(ccx, llty);

    let align: u16 = (align_min as u16) << 8 | align_pref as u16;

    align.hash(hash_state);

    let aggregate = llty.is_aggregate();
    let ret_outptr = type_of::return_uses_outptr(ccx, param);
    let arg_is_indirect = type_of::arg_is_indirect(ccx, param);
    let fat = common::type_is_fat_ptr(ccx.tcx(), param);

    let flags: u8 =
        (aggregate as u8) << 0
        | (ret_outptr as u8) << 1
        | (arg_is_indirect as u8) << 2
        | (fat as u8) << 3
        | (hash_opt_flag as u8) << 4;

    flags.hash(hash_state);
}

impl Facts {
    fn new() -> Facts {
        Facts {
            uses_type_params: true,
            expect_meld: false,
            no_drop: false
        }
    }
}

pub struct MeldState<'tcx> {
    melds: FnvHashMap<u64, ValueRef>,
    wip: FnvHashSet<ast::NodeId>,
    enabled: bool,
    always_meld: bool,
    cache: FnvHashMap<MonoId<'tcx>, MeldHash>
}

impl<'tcx> MeldState<'tcx> {
    pub fn new<'a>() -> MeldState<'a> {
        let no_meld = ::std::env::var("RUSTC_NO_MELD").is_ok();
        let always_meld = ::std::env::var("RUSTC_ALWAYS_MELD").is_ok();

        MeldState {
            melds: FnvHashMap(),
            wip: FnvHashSet(),
            enabled: !no_meld,
            always_meld: always_meld,
            cache: FnvHashMap()
        }
    }
}

/* next:

slice::[T].SliceExt::iter
slice::Iter<'a, T>.Iterator::next
mem::replace
ptr::Unique<T>::new
cell::UnsafeCell<T>::get
ptr::P<T>.Deref::deref
cell::RefCell<T>::new
cell::RefCell<T>::borrow_mut
mem::align_of_val
mem::swap
option::Option<T>::is_some
option::Option<T>::unwrap
vec::Vec<T>.ops::Deref::deref
nonzero::NonZero<T>.Deref::deref
vec::dealloc
vec::Vec<T>::from_raw_parts
vec::Vec<T>::with_capacity
vec::Vec<T>::set_len
vec::Vec<T>::grow_capacity
vec::Vec<T>::reserve
slice::[T]::get_unchecked
slice::[T]::get_unchecked_mut
collections::hash::table::Bucket<K, V, M>::next
*/

// FIXME
// is    "option::Option<T>.Default::default", really ok
// because of enum optimizations?
// FIXME hash::Hasher::write_u64 looks wrong
