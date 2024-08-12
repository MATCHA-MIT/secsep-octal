open Isa

module type EntryType = sig
  type t
  val partial_read_val: t -> t
  val partial_write_val: t -> t -> t
  val next_var : t -> t
  val to_string : t -> string
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
