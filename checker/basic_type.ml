open Type.Smt_emitter
open Z3
open Z3_sexp
open Sexplib.Std

module DepType = struct
  exception DepTypeError of string

  let dep_type_error msg = raise (DepTypeError ("[Dep Type Error] " ^ msg))

  type exp_t =
    | Exp of Z3Expr.expr
    | Top
  [@@deriving sexp]

  type t = exp_t * int
  [@@deriving sexp]

  let get (smt_ctx: SmtEmitter.t) (low: int) (high: int) (e: t) : t =
    let ctx, _ = smt_ctx in
    let new_size = high - low + 1 in
    let exp, size = e in
    match exp with
    | Exp e -> 
      if size > high then
        Exp (Z3.BitVector.mk_extract ctx low high e), new_size (* TODO: Check mk_extract parameter order *)
      else
        dep_type_error (Printf.sprintf "get with invalid argument low %d high %d size %d" low high size)
    | Top -> Top, new_size

end

module TaintType = struct
  exception TaintTypeError of string

  let taint_type_error msg = raise (TaintTypeError ("[Taint Type Error] " ^ msg))

  type t = Z3Expr.expr (* Note: t must be a Boolean *)
  [@@deriving sexp]

end


module BasicType = struct
  exception BasicTypeError of string

  let basic_type_error msg = raise (BasicTypeError ("[Basic Type Error] " ^ msg))

  type t = DepType.t * TaintType.t
  [@@deriving sexp]

  type update_t =
  | Reg

  let get (smt_ctx: SmtEmitter.t) (low: int) (high: int) (e: t) : t =
    let dep, taint = e in
    DepType.get smt_ctx low high dep, taint

end
