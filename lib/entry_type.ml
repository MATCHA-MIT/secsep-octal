open Isa_basic
open Single_exp
open Taint_exp
open Constraint
open Smt_emitter
open Sexplib

module type EntryType = sig
  type t
  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t

  type ext_t = 
    | SignExt
    | ZeroExt
    | OldExt of t (* Used for memory slot partial update *)

  type local_var_map_t
  val local_var_map_t_of_sexp : Sexp.t -> local_var_map_t
  val sexp_of_local_var_map_t : local_var_map_t -> Sexp.t

  val get_empty_var_map: local_var_map_t

  (* TODO: Remove these two after finished updating memory type!!! *)
  val partial_read_val: t -> t
  val partial_write_val: t -> t -> t

  val next_var : t -> t
  val to_string : t -> string
  val to_ocaml_string: t -> string
  val empty_var_map_to_ocaml_string: string
  val cmp: t -> t -> int
  val read_val: int64 -> int64 -> t -> t (* off -> sz -> type: read tppe of [off, off+sz] *)
  val mem_partial_read_val: t -> t (* a lazy implementation to handle partially reading a mem entry *)
  val mem_partial_write_val: t -> t -> t (* a lazy implementation to handle partially writing a mem entry *)
  val ext_val: ext_t -> int64 -> int64 -> t -> t (* off -> sz -> type *)
  val get_eq_taint_constraint: t -> t -> Constraint.t list
  val get_sub_taint_constraint: t -> t -> Constraint.t list
  val get_untaint_constraint: t -> Constraint.t list
  val update_ld_taint_constraint: t -> TaintExp.t option -> Constraint.t list
  val update_st_taint_constraint: t -> TaintExp.t option -> t * Constraint.t list

  val exe_bop_inst: IsaBasic.bop -> t -> t -> t
  val exe_uop_inst: IsaBasic.uop -> t -> t

  val get_single_exp: t -> SingleExp.t (* Used for get address, must be 8-byte dep type *)
  val get_single_taint_exp: t -> (SingleExp.t * TaintExp.t)
  val get_single_local_var_map: local_var_map_t -> SingleExp.local_var_map_t
  val get_const_type: IsaBasic.immediate -> t
  val get_top_type: unit -> t
  val get_top_untaint_type: unit -> t
  val get_mem_op_type: IsaBasic.immediate option -> t option -> t option -> int64 -> t
  val handle_mem_rw: t -> t -> Constraint.t list

  (* vmap->exp->pc->(new_vmap,new_exp) use local var if exp is a bexp or uexp, pc determines local var id*)
  val update_local_var: local_var_map_t -> t -> int -> (local_var_map_t * t)
  (* map, a, b: a is a var, add a->b to map *)
  val add_local_var: local_var_map_t -> t -> t -> local_var_map_t
  val add_local_global_var: local_var_map_t -> SingleExp.SingleVarSet.t -> local_var_map_t
  val pp_local_var: int -> local_var_map_t -> unit

  (* This is used to build var map and var constraints when connecting two context *)
  val add_context_map: bool (* is_mem *) -> local_var_map_t -> t -> t -> local_var_map_t

  (* eliminate all local variables*)
  val repl_local_var: local_var_map_t -> t -> t
  val repl_context_var: local_var_map_t -> t -> t
  val is_val2 : local_var_map_t -> t -> bool

  val to_smt_expr: SmtEmitter.t -> t -> SmtEmitter.exp_t
end


module DepType = struct
  exception DepTypeError of string
  let dep_type_error msg = raise (DepTypeError ("[Dep Type Error] " ^ msg))

  type bop =
    | Add
    | Sub
    | Mul (* Signed multiply *)
    | Sal
    | Sar
    | Xor
    | And
    | Or
  
  type uop =
    | Not

  (* type t =
    | Const of int * int
    | Var of IsaBasic.immediate * int
    | BExp of bop * t * t  *)

  type dep_size =
    | Quad
    | Long
    | Word
    | High
    | Low
    | Other

end
