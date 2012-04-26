import common::*;
import lib::llvm::{TypeRef};
import syntax::ast;
import lib::llvm::llvm;
import driver::session::session;
import std::map::hashmap;

import ty::*;

export type_of;
export type_of_explicit_args;
export type_of_fn_from_ty;
export type_of_fn;

fn type_of_explicit_args(cx: @crate_ctxt, inputs: [ty::arg]) -> [TypeRef] {
    vec::map(inputs) {|arg|
        let arg_ty = arg.ty;
        let llty = type_of(cx, arg_ty);
        alt ty::resolved_mode(cx.tcx, arg.mode) {
          ast::by_val { llty }
          _ { T_ptr(llty) }
        }
    }
}

fn type_of_fn(cx: @crate_ctxt, inputs: [ty::arg], output: ty::t) -> TypeRef {
    let mut atys: [TypeRef] = [];

    // Arg 0: Output pointer.
    atys += [T_ptr(type_of(cx, output))];

    // Arg 1: Environment
    atys += [T_opaque_box_ptr(cx)];

    // ... then explicit args.
    atys += type_of_explicit_args(cx, inputs);
    ret T_fn(atys, llvm::LLVMVoidType());
}

// Given a function type and a count of ty params, construct an llvm type
fn type_of_fn_from_ty(cx: @crate_ctxt, fty: ty::t) -> TypeRef {
    type_of_fn(cx, ty::ty_fn_args(fty), ty::ty_fn_ret(fty))
}

fn type_of(cx: @crate_ctxt, t: ty::t) -> TypeRef {
    assert !ty::type_has_vars(t);

    #debug("type_of %?: %?", t, ty::get(t));

    // Check the cache.
    if cx.lltypes.contains_key(t) { ret cx.lltypes.get(t); }

    // Replace any typedef'd types with their equivalent non-typedef
    // type. This ensures that all LLVM nominal types that contain
    // Rust types are defined as the same LLVM types.  If we don't do
    // this then, e.g. `option<{myfield: bool}>` would be a different
    // type than `option<myrec>`.
    let t_norm = ty::normalize_ty(cx.tcx, t);
    let llty = if t != t_norm {
        type_of(cx, t_norm)
    } else {
        alt ty::get(t).struct {
          ty::ty_nil | ty::ty_bot { T_nil() }
          ty::ty_bool { T_bool() }
          ty::ty_int(t) { T_int_ty(cx, t) }
          ty::ty_uint(t) { T_uint_ty(cx, t) }
          ty::ty_float(t) { T_float_ty(cx, t) }
          ty::ty_estr(ty::vstore_uniq) |
          ty::ty_str { T_ptr(T_vec(cx, T_i8())) }
          ty::ty_enum(did, _) { type_of_enum(cx, did, t) }
          ty::ty_estr(ty::vstore_box) { T_ptr(T_box(cx, T_i8())) }
          ty::ty_evec(mt, ty::vstore_box) |
          ty::ty_box(mt) { T_ptr(T_box(cx, type_of(cx, mt.ty))) }
          ty::ty_opaque_box { T_ptr(T_box(cx, T_i8())) }
          ty::ty_uniq(mt) { T_ptr(type_of(cx, mt.ty)) }
          ty::ty_evec(mt, ty::vstore_uniq) |
          ty::ty_vec(mt) { T_ptr(T_vec(cx, type_of(cx, mt.ty))) }
          ty::ty_ptr(mt) { T_ptr(type_of(cx, mt.ty)) }
          ty::ty_rptr(_, mt) { T_ptr(type_of(cx, mt.ty)) }

          ty::ty_evec(mt, ty::vstore_slice(_)) {
            T_struct([T_ptr(type_of(cx, mt.ty)),
                      T_uint_ty(cx, ast::ty_u)])
          }

          ty::ty_estr(ty::vstore_slice(_)) {
            T_struct([T_ptr(T_i8()),
                      T_uint_ty(cx, ast::ty_u)])
          }

          ty::ty_estr(ty::vstore_fixed(n)) {
            T_array(T_i8(), n + 1u /* +1 for trailing null */)
          }

          ty::ty_evec(mt, ty::vstore_fixed(n)) {
            T_array(type_of(cx, mt.ty), n)
          }

          ty::ty_rec(fields) {
            let mut tys: [TypeRef] = [];
            for vec::each(fields) {|f|
                let mt_ty = f.mt.ty;
                tys += [type_of(cx, mt_ty)];
            }
            T_struct(tys)
          }
          ty::ty_fn(_) { T_fn_pair(cx, type_of_fn_from_ty(cx, t)) }
          ty::ty_iface(_, _) { T_opaque_iface(cx) }
          ty::ty_res(_, sub, substs) {
            let sub1 = ty::subst(cx.tcx, substs, sub);
            ret T_struct([T_i8(), type_of(cx, sub1)]);
          }
          ty::ty_param(_, _) { T_typaram(cx.tn) }
          ty::ty_type { T_ptr(cx.tydesc_type) }
          ty::ty_tup(elts) {
            let mut tys = [];
            for vec::each(elts) {|elt|
                tys += [type_of(cx, elt)];
            }
            T_struct(tys)
          }
          ty::ty_opaque_closure_ptr(_) { T_opaque_box_ptr(cx) }
          ty::ty_constr(subt,_) { type_of(cx, subt) }
          ty::ty_class(did, ts) {
            // only instance vars are record fields at runtime
            let fields = lookup_class_fields(cx.tcx, did);
            let tys = vec::map(fields) {|f|
                let t = ty::lookup_field_type(cx.tcx, did, f.id, ts);
                type_of(cx, t)
            };
            T_struct(tys)
          }
          ty::ty_self(_) { cx.tcx.sess.unimpl("type_of: ty_self \
                                               not implemented"); }
          ty::ty_var(_) { cx.tcx.sess.bug("type_of shouldn't see a ty_var"); }
        }
    };
    cx.lltypes.insert(t, llty);
    ret llty;
}

// This should only be called from type_of, above, because it
// creates new llvm named struct types lazily that are then
// cached by type_of
fn type_of_enum(cx: @crate_ctxt, did: ast::def_id, t: ty::t)
    -> TypeRef {

    #debug("type_of_enum %?: %?", t, ty::get(t));

    // Every enum type has a unique name. When we find our roots
    // for GC and unwinding we will use this name to rediscover
    // the Rust type
    let name = llvm_type_name(cx, t);

    let named_llty = common::T_named_struct(name);

    let lltys = {
        let degen = (*ty::enum_variants(cx.tcx, did)).len() == 1u;
        let size = shape::static_size_of_enum(cx, t);
        if !degen {
            [T_enum_discrim(cx), type_of_supervariant(cx, t)]
        }
        else if size == 0u {
            [T_enum_discrim(cx)]
        }
        else {
            [type_of_supervariant(cx, t)]
        }
    };

    common::set_struct_body(named_llty, lltys);
    ret named_llty;
}

fn llvm_type_name(cx: @crate_ctxt, t: ty::t) -> str {
    let (name, did, tps) = alt check ty::get(t).struct {
      ty::ty_enum(did, substs) {
        ("enum", did, substs.tps)
      }
    };
    ret #fmt(
        "%s %s[#%d]",
        name,
        util::ppaux::parameterized(
            cx.tcx,
            ty::item_path_str(cx.tcx, did),
            none,
            tps),
        did.crate
    );
}

// The superviariant is a struct type that has both the maximum alignment
// and maximum size of all variants for a particular enum, discounting
// the discriminator
fn type_of_supervariant(cx: @crate_ctxt, t: ty::t) -> TypeRef {

    #debug("finding supervariant for %?",
           util::ppaux::ty_to_str(cx.tcx, t));

    let size = shape::static_size_of_enum(cx, t);
    let align = shape::static_align_of_enum(cx, t);

    #debug("size: %u, align: %u", size, align);

    if size == 0u {
        ret T_struct([]);
    }

    // The type used to fill out the structure to the maximum
    // multiple of the alignment. It has the same alignment
    // as the struct alignment.
    let fill_ty = alt align {
      1u { T_i8() }
      2u { T_i16() }
      4u { T_i32() }
      8u { T_i64() }
      unknown_alignment {
        cx.sess.bug(
            #fmt("unknown alignment %u in type_of_supervariant",
                 unknown_alignment));
      }
    };

    // The type used to pad the structure up to the structure size
    let pad_ty = T_i8();

    // The number of each of the above types to stuff into our structure
    let fill_times = size / align;
    let pad_times = size % align;

    #debug("supervariant fill: %u, pad %u", fill_times, pad_times);

    let fill_vec = vec::from_elem(fill_times, fill_ty);
    let pad_vec = vec::from_elem(pad_times, pad_ty);

    let llty = T_struct(fill_vec + pad_vec);

    let supersize = shape::llsize_of_real(cx, llty);
    let superalign = shape::llalign_of_real(cx, llty);
    #debug("supersize: %u, superalign: %u", supersize, superalign);

    assert size == supersize;
    assert align == superalign;

    ret llty;
}
