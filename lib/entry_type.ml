open Isa
open Single_exp

module type EntryType = sig
  type t
  type ext_t = 
    | SignExt
    | ZeroExt
    | OldExt of t (* Used for memory slot partial update *)

  type local_var_map_t

  (* TODO: Remove these two after finished updating memory type!!! *)
  val partial_read_val: t -> t
  val partial_write_val: t -> t -> t

  val next_var : t -> t
  val to_string : t -> string
  val read_val: int64 -> int64 -> t -> t (* off -> sz -> type: read tppe of [off, off+sz] *)
  val ext_val: ext_t -> int64 -> int64 -> t -> t (* off -> sz -> type *)
  val get_single_exp: t -> SingleExp.t (* Used for get address, must be 8-byte dep type *)
  val get_top_type: t
  val get_imm_type: Isa.immediate -> t
  val get_mem_op_type: Isa.immediate option -> t option -> t option -> int64 -> t

  (* vmap->exp->pc->(new_vmap,new_exp) use local var if exp is a bexp or uexp, pc determines local var id*)
  val update_local_var: local_var_map_t -> t -> int -> (local_var_map_t * t)
  (* eliminate all local variables*)
  val repl_local_var: local_var_map_t -> t -> t
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
