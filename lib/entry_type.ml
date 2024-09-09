open Isa
open Single_exp
open Constraint
open Smt_emitter

module type EntryType = sig
  type t
  type ext_t = 
    | SignExt
    | ZeroExt
    | OldExt of t (* Used for memory slot partial update *)

  type local_var_map_t

  val get_empty_var_map: local_var_map_t

  (* TODO: Remove these two after finished updating memory type!!! *)
  val partial_read_val: t -> t
  val partial_write_val: t -> t -> t

  val next_var : t -> t
  val to_string : t -> string
  val cmp: t -> t -> int
  val read_val: int64 -> int64 -> t -> t (* off -> sz -> type: read tppe of [off, off+sz] *)
  val mem_partial_read_val: t -> t (* a lazy implementation to handle partially reading a mem entry *)
  val mem_partial_write_val: t -> t -> t (* a lazy implementation to handle partially writing a mem entry *)
  val ext_val: ext_t -> int64 -> int64 -> t -> t (* off -> sz -> type *)
  val get_write_constraint: t -> t -> Constraint.t list

  val exe_bop_inst: Isa.bop -> t -> t -> t
  val exe_uop_inst: Isa.uop -> t -> t

  val get_single_exp: t -> SingleExp.t (* Used for get address, must be 8-byte dep type *)
  val get_single_local_var_map: local_var_map_t -> SingleExp.local_var_map_t
  val get_const_type: Isa.immediate -> t
  val get_top_type: t
  val get_mem_op_type: Isa.immediate option -> t option -> t option -> int64 -> t

  (* vmap->exp->pc->(new_vmap,new_exp) use local var if exp is a bexp or uexp, pc determines local var id*)
  val update_local_var: local_var_map_t -> t -> int -> (local_var_map_t * t)
  (* map, a, b: a is a var, add a->b to map *)
  val add_local_var: local_var_map_t -> t -> t -> local_var_map_t
  val add_local_global_var: local_var_map_t -> SingleExp.SingleVarSet.t -> local_var_map_t
  val pp_local_var: int -> local_var_map_t -> unit

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
    | Var of Isa.imm_var_id * int
    | BExp of bop * t * t  *)

  type dep_size =
    | Quad
    | Long
    | Word
    | High
    | Low
    | Other

end
