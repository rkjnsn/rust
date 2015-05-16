// Copyright 2012-2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use back::svh::Svh;
use back::link::exported_name;
use session;
use llvm::ValueRef;
use llvm;
use middle::infer;
use middle::subst;
use middle::subst::{Subst, Substs};
use middle::traits;
use middle::ty_fold::{TypeFolder, TypeFoldable};
use trans::attributes;
use trans::base::{trans_enum_variant, push_ctxt, get_item_val};
use trans::base::trans_fn;
use trans::base;
use trans::common::*;
use trans::declare;
use trans::foreign;
use middle::ty::{self, HasProjectionTypes, Ty};
use util::ppaux::Repr;

use syntax::abi;
use syntax::ast;
use syntax::ast_map;
use syntax::ast_util::local_def;
use syntax::attr;
use syntax::codemap::DUMMY_SP;
use std::hash::{Hasher, Hash, SipHasher};
use std::cell::Cell;

pub fn monomorphic_fn<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>,
                                fn_id: ast::DefId,
                                original_fn_id: ast::DefId,
                                psubsts: &'tcx subst::Substs<'tcx>,
                                ref_id: Option<ast::NodeId>)
    -> (ValueRef, Ty<'tcx>, bool) {
    debug!("monomorphic_fn(\
            fn_id={}, \
            real_substs={}, \
            ref_id={:?})",
           fn_id.repr(ccx.tcx()),
           psubsts.repr(ccx.tcx()),
           ref_id);

    assert!(psubsts.types.all(|t| {
        !ty::type_needs_infer(*t) && !ty::type_has_params(*t)
    }));

    let _icx = push_ctxt("monomorphic_fn");

    let hash_id = MonoId::new(ccx, original_fn_id, &psubsts.types);

    let item_ty = ty::lookup_item_type(ccx.tcx(), fn_id).ty;
    let mono_ty = item_ty.subst(ccx.tcx(), psubsts);

    match ccx.monomorphized().borrow().get(&hash_id) {
        Some(&val) => {
            debug!("leaving monomorphic fn {}",
            ty::item_path_str(ccx.tcx(), fn_id));
            return (val, mono_ty, false);
        }
        None => ()
    }

        info!("monomorphizing: {} ({})",
              ty::item_path_str(ccx.tcx(), fn_id),
              psubsts.repr(ccx.tcx()));

    debug!("monomorphic_fn(\
            fn_id={}, \
            psubsts={}, \
            hash_id={:?})",
           fn_id.repr(ccx.tcx()),
           psubsts.repr(ccx.tcx()),
           hash_id);


    let map_node = session::expect(
        ccx.sess(),
        ccx.tcx().map.find(fn_id.node),
        || {
            format!("while monomorphizing {:?}, couldn't find it in \
                     the item map (may have attempted to monomorphize \
                     an item defined in a different crate?)",
                    fn_id)
        });

    if let ast_map::NodeForeignItem(_) = map_node {
        if ccx.tcx().map.get_foreign_abi(fn_id.node) != abi::RustIntrinsic {
            // Foreign externs don't have to be monomorphized.
            return (get_item_val(ccx, fn_id.node), mono_ty, true);
        }
    }

    debug!("monomorphic_fn about to subst into {}", item_ty.repr(ccx.tcx()));

    debug!("mono_ty = {} (post-substitution)", mono_ty.repr(ccx.tcx()));

    let mono_ty = normalize_associated_type(ccx.tcx(), &mono_ty);
    debug!("mono_ty = {} (post-normalization)", mono_ty.repr(ccx.tcx()));

    ccx.stats().n_monos.set(ccx.stats().n_monos.get() + 1);

    let depth;
    {
        let mut monomorphizing = ccx.monomorphizing().borrow_mut();
        depth = match monomorphizing.get(&fn_id) {
            Some(&d) => d, None => 0
        };

        // Random cut-off -- code that needs to instantiate the same function
        // recursively more than thirty times can probably safely be assumed
        // to be causing an infinite expansion.
        if depth > ccx.sess().recursion_limit.get() {
            ccx.sess().span_fatal(ccx.tcx().map.span(fn_id.node),
                "reached the recursion limit during monomorphization");
        }

        monomorphizing.insert(fn_id, depth + 1);
    }

    let hash_u64;
    let hash;
    let symbol = {
        let ref mut hasher = SipHasher::new();
        hash_id.hash(hasher);
        hash_u64 = hasher.finish();
        hash = format!("h{}", hash_u64);
        ccx.tcx().map.with_path(fn_id.node, |path| {
            exported_name(path, &hash[..])
        })
    };

    debug!("monomorphize_fn mangled to {}", symbol);

    info!("monomorphic hash_id: {:?}, symbol: {}", hash_id, symbol);
    
    // This shouldn't need to option dance.
    let mut hash_id = Some(hash_id);
    let is_first = !ccx.have_monomorphization(hash_u64);
    let secondary_translation = Cell::new(false);
    let do_linkonce_opts = Cell::new(false);
    let mut mk_lldecl = |abi: abi::Abi| {
        let lldecl = if abi != abi::Rust {
            foreign::decl_rust_fn_with_foreign_abi(ccx, mono_ty, &symbol[..])
        } else {
            // FIXME(nagisa): perhaps needs a more fine grained selection? See setup_lldecl below.
            declare::define_internal_rust_fn(ccx, &symbol[..], mono_ty).unwrap_or_else(||{
                ccx.sess().bug(&format!("symbol `{}` already defined", symbol));
            })
        };

        ccx.monomorphized().borrow_mut().insert(hash_id.take().unwrap(), lldecl);
        lldecl
    };
    let setup_lldecl = |lldecl, attrs: &[ast::Attribute]| {
        base::update_linkage(ccx, lldecl, None, base::OriginalTranslation);
        attributes::from_fn_attrs(ccx, attrs, lldecl);

        let trans_everywhere = attr::requests_inline(attrs);

        secondary_translation.set(trans_everywhere && !is_first);
        do_linkonce_opts.set(true);

        // FIXME: If this is from the local crate and unreachable
        // then we shouldn't make it public. No downstream users.
        llvm::SetLinkage(lldecl, llvm::ExternalLinkage);

        // If `true`, then `lldecl` should be given a function body.
        // Otherwise, it should be left as a declaration of an external
        // function, with no definition in the current compilation unit.
        trans_everywhere || is_first
    };

    let lldecl = match map_node {
        ast_map::NodeItem(i) => {
            match *i {
              ast::Item {
                  node: ast::ItemFn(ref decl, _, _, abi, _, ref body),
                  ..
              } => {
                  let d = mk_lldecl(abi);
                  let needs_body = setup_lldecl(d, &i.attrs);
                  if needs_body {
                      if abi != abi::Rust {
                          foreign::trans_rust_fn_with_foreign_abi(
                              ccx, &**decl, &**body, &[], d, psubsts, fn_id.node,
                              Some(&hash[..]));
                      } else {
                          trans_fn(ccx, &**decl, &**body, d, psubsts, fn_id.node, &[]);
                      }
                  }

                  d
              }
              _ => {
                ccx.sess().bug("Can't monomorphize this kind of item")
              }
            }
        }
        ast_map::NodeVariant(v) => {
            let parent = ccx.tcx().map.get_parent(fn_id.node);
            let tvs = ty::enum_variants(ccx.tcx(), local_def(parent));
            let this_tv = tvs.iter().find(|tv| { tv.id.node == fn_id.node}).unwrap();
            let d = mk_lldecl(abi::Rust);
            attributes::inline(d, attributes::InlineAttr::Hint);
            match v.node.kind {
                ast::TupleVariantKind(ref args) => {
                    trans_enum_variant(ccx,
                                       parent,
                                       &*v,
                                       &args[..],
                                       this_tv.disr_val,
                                       psubsts,
                                       d);
                }
                ast::StructVariantKind(_) =>
                    ccx.sess().bug("can't monomorphize struct variants"),
            }
            d
        }
        ast_map::NodeImplItem(impl_item) => {
            match impl_item.node {
                ast::MethodImplItem(ref sig, ref body) => {
                    let d = mk_lldecl(abi::Rust);
                    let needs_body = setup_lldecl(d, &impl_item.attrs);
                    if needs_body {
                        trans_fn(ccx,
                                 &sig.decl,
                                 body,
                                 d,
                                 psubsts,
                                 impl_item.id,
                                 &[]);
                    }
                    d
                }
                _ => {
                    ccx.sess().bug(&format!("can't monomorphize a {:?}",
                                           map_node))
                }
            }
        }
        ast_map::NodeTraitItem(trait_item) => {
            match trait_item.node {
                ast::MethodTraitItem(ref sig, Some(ref body)) => {
                    let d = mk_lldecl(abi::Rust);
                    let needs_body = setup_lldecl(d, &trait_item.attrs);
                    if needs_body {
                        trans_fn(ccx, &sig.decl, body, d,
                                 psubsts, trait_item.id, &[]);
                    }
                    d
                }
                _ => {
                    ccx.sess().bug(&format!("can't monomorphize a {:?}",
                                           map_node))
                }
            }
        }
        ast_map::NodeStructCtor(struct_def) => {
            let d = mk_lldecl(abi::Rust);
            attributes::inline(d, attributes::InlineAttr::Hint);
            base::trans_tuple_struct(ccx,
                                     &struct_def.fields,
                                     struct_def.ctor_id.expect("ast-mapped tuple struct \
                                                                didn't have a ctor id"),
                                     psubsts,
                                     d);
            d
        }

        // Ugh -- but this ensures any new variants won't be forgotten
        ast_map::NodeForeignItem(..) |
        ast_map::NodeLifetime(..) |
        ast_map::NodeExpr(..) |
        ast_map::NodeStmt(..) |
        ast_map::NodeArg(..) |
        ast_map::NodeBlock(..) |
        ast_map::NodePat(..) |
        ast_map::NodeLocal(..) => {
            ccx.sess().bug(&format!("can't monomorphize a {:?}",
                                   map_node))
        }
    };

    ccx.monomorphizing().borrow_mut().insert(fn_id, depth);

    if do_linkonce_opts.get() {
        if is_first {
            // The first time a crate dag instantiates a function with
            // a given set of substitutions it is declared
            // linkonce_odr. By being linkonce, it can be exported
            // publicly by multiple crates in a diamond, and the
            // linker will throw one out. ODR allows *this*
            // translation unit to assume that any downstream
            // implementations of this function that may override it
            // have *the same implementation* as this one - which lets
            // LLVM inline this one.
            //
            // Subsequent monomorphizations in downstream crates will
            // have available_externally linkage. This is a
            // compile-time optimization that lets the downstream
            // compilation units reuse the upstream translations.
            llvm::SetLinkage(lldecl, llvm::WeakODRLinkage);
            ccx.available_monomorphizations().borrow_mut().insert(hash_u64);
            info!("first mono: {}", symbol);
        } else if secondary_translation.get() {
            llvm::SetLinkage(lldecl, llvm::AvailableExternallyLinkage);
            info!("second mono: {}", symbol);
        } else {
            info!("unmonomorphized: {}", symbol);
            llvm::SetLinkage(lldecl, llvm::ExternalLinkage);
        }
    }

    debug!("leaving monomorphic fn {}", ty::item_path_str(ccx.tcx(), fn_id));
    (lldecl, mono_ty, true)
}

#[derive(PartialEq, Eq, Debug)]
pub struct MonoId {
    svh: Svh,
    node_id: ast::NodeId,
    crate_independent_substs_hash: u64,
}

impl MonoId {
    pub fn new<'a, 'tcx>(ccx: &CrateContext<'a, 'tcx>, def_id: ast::DefId,
                         params: &'tcx subst::VecPerParamSpace<Ty<'tcx>>) -> MonoId {
        // FIXME: expensive
        let svh = if def_id.krate == ast::LOCAL_CRATE {
            ccx.link_meta().crate_hash.clone()
        } else {
            ccx.tcx().sess.cstore.get_crate_hash(def_id.krate)
        };

        let ref mut state = SipHasher::new();

        for ty in params {
            // FIXME: This clones the svh at least once more
            let hash = ty::hash_crate_independent(ccx.tcx(), *ty, &svh);
            hash.hash(state);
        }
        let subst_hash = state.finish();

        MonoId {
            svh: svh,
            node_id: def_id.node,
            crate_independent_substs_hash: subst_hash
        }
    }
}

impl Hash for MonoId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.svh.hash(state);
        self.node_id.hash(state);
        self.crate_independent_substs_hash.hash(state);
    }
}

/// Monomorphizes a type from the AST by first applying the in-scope
/// substitutions and then normalizing any associated types.
pub fn apply_param_substs<'tcx,T>(tcx: &ty::ctxt<'tcx>,
                                  param_substs: &Substs<'tcx>,
                                  value: &T)
                                  -> T
    where T : TypeFoldable<'tcx> + Repr<'tcx> + HasProjectionTypes + Clone
{
    let substituted = value.subst(tcx, param_substs);
    normalize_associated_type(tcx, &substituted)
}

/// Removes associated types, if any. Since this during
/// monomorphization, we know that only concrete types are involved,
/// and hence we can be sure that all associated types will be
/// completely normalized away.
pub fn normalize_associated_type<'tcx,T>(tcx: &ty::ctxt<'tcx>, value: &T) -> T
    where T : TypeFoldable<'tcx> + Repr<'tcx> + HasProjectionTypes + Clone
{
    debug!("normalize_associated_type(t={})", value.repr(tcx));

    let value = erase_regions(tcx, value);

    if !value.has_projection_types() {
        return value;
    }

    // FIXME(#20304) -- cache

    let infcx = infer::new_infer_ctxt(tcx);
    let typer = NormalizingClosureTyper::new(tcx);
    let mut selcx = traits::SelectionContext::new(&infcx, &typer);
    let cause = traits::ObligationCause::dummy();
    let traits::Normalized { value: result, obligations } =
        traits::normalize(&mut selcx, cause, &value);

    debug!("normalize_associated_type: result={} obligations={}",
           result.repr(tcx),
           obligations.repr(tcx));

    let mut fulfill_cx = traits::FulfillmentContext::new();
    for obligation in obligations {
        fulfill_cx.register_predicate_obligation(&infcx, obligation);
    }
    let result = drain_fulfillment_cx_or_panic(DUMMY_SP, &infcx, &mut fulfill_cx, &result);

    result
}
