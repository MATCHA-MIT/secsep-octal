open Isa_basic
open Mem_offset
open Pretty_print

module Ir = struct
  exception IrError of string
  let ir_error msg = raise (IrError ("[Ir Error] " ^ msg))

  type operand =
    | RegOp of int
    | MemOp of IsaBasic.imm_var_id * MemOffset.t * bool (* Addr range & access full slot with single inst or not *)
    | UnknownOp

  type label = string

  type t =
    | Mov of operand * (operand list)
    | Jump of label
    | Skip

  let ready_for_prop (e: t) : bool =
    match e with
    | Mov (dest, src_list) ->
      begin match List.find_opt (fun x -> x = UnknownOp) (dest :: src_list) with
      | None -> true
      | _ -> false
      end
    | _ -> true

  let cmp_operand (op1: operand) (op2: operand) : bool =
    match op1, op2 with
    | RegOp r1, RegOp r2 -> r1 = r2
    | MemOp (base1, off1, b1), MemOp (base2, off2, b2) ->
      base1 = base2 && (MemOffset.cmp off1 off2 = 0) && b1 = b2
    | UnknownOp, UnknownOp -> true
    | _ -> false

  let string_of_operand (op: operand) : string =
    match op with
    | RegOp r -> IsaBasic.string_of_reg_idx r
    | MemOp (ptr, offset, is_single_full_slot) -> 
      "Mem (" ^ (string_of_int ptr) ^ ", " ^ (MemOffset.to_string offset) ^ ", " ^ (string_of_bool is_single_full_slot) ^ ")"
    | UnknownOp -> "Unknown"

  let to_string (e: t) : string =
    match e with
    | Mov (dest, src_list) ->
      let src_str_list = List.map string_of_operand src_list in
      "Mov {" ^ (String.concat "; " src_str_list) ^ "} -> " ^ (string_of_operand dest)
    | Jump target -> "Jump " ^ target
    | Skip -> "Skip"

  let pp_ir (lvl: int) (e: t) =
    PP.print_lvl lvl "%s\n" (to_string e)

end

module IrProgram = struct
  type block = {
    label: Ir.label;
    insts: Ir.t list;
  }

  type t = block list

  let pp_ir_program (lvl: int) (p: t) =
    PP.print_lvl lvl "Ir Prog\n";
    List.iteri (
      fun i block -> 
        PP.print_lvl (lvl + 1) "<Block %d %s>\n" i block.label;
        List.iteri (
          fun i x -> 
            PP.print_lvl (lvl + 2) "%d" i;
            Ir.pp_ir (lvl + 2) x
        ) block.insts
    ) p

end
