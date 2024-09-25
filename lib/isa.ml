(* ISA interface *)
open Isa_basic
open Pretty_print
open Mem_anno_type

module Isa (MemAnno: MemAnnoType) = struct

  include IsaBasic

  type operand =
    | ImmOp of immediate
    | RegOp of register
    | MemOp of immediate option * register option * register option * scale option (* disp, base, index, scale *)
    | LdOp of immediate option * register option * register option * scale option * int64 * MemAnno.t
    | StOp of immediate option * register option * register option * scale option * int64 * MemAnno.t
    | LabelOp of label

  let rec string_of_operand (op: operand): string =
    match op with
    | ImmOp imm -> string_of_immediate imm
    | RegOp r -> string_of_reg r
    | MemOp (disp, base, index, scale) ->
      let disp_str = string_of_option string_of_immediate disp in
      let base_str = string_of_option string_of_reg base in
      let index_str = string_of_option string_of_reg index in
      let scale_str = string_of_option scale_to_string (scale) in
      Printf.sprintf "%s(%s,%s,%s)" disp_str base_str index_str scale_str
    | LdOp (disp, base, index, scale, size, mem_anno) ->
      let addr_str = string_of_operand (MemOp (disp, base, index, scale)) in
      Printf.sprintf "Ld(%s,%s,anno={%s})" addr_str (Int64.to_string size) (MemAnno.to_string mem_anno)
    | StOp (disp, base, index, scale, size, mem_anno) ->
      let addr_str = string_of_operand (MemOp (disp, base, index, scale)) in
      Printf.sprintf "St(%s,%s,anno={%s})" addr_str (Int64.to_string size) (MemAnno.to_string mem_anno)
    | LabelOp label -> label

  let get_op_size (op: operand) : int64 =
    match op with
    | RegOp r -> get_reg_size r
    | LdOp (_, _, _, _, size, _)
    | StOp (_, _, _, _, size, _) -> size
    | _ -> isa_error "cannot get size for the given op"

  (* TODO: Remove this function *)
  let cmp_operand (op1: operand) (op2: operand) : bool = (* true for equal *)
    match op1, op2 with
    | ImmOp i1, ImmOp i2 -> i1 = i2
    | RegOp r1, RegOp r2 -> r1 = r2
    | MemOp (d1, b1, i1, s1), MemOp (d2, b2, i2, s2) ->
      d1 = d2 && b1 = b2 && i1 = i2 && s1 = s2
    | LdOp (d1, b1, i1, s1, size1, _), LdOp (d2, b2, i2, s2, size2, _)
    | StOp (d1, b1, i1, s1, size1, _), StOp (d2, b2, i2, s2, size2, _) ->
        d1 = d2 && b1 = b2 && i1 = i2 && s1 = s2 && size1 = size2
    | LabelOp l1, LabelOp l2 -> l1 = l2
    | _ -> false

  let get_reg_op_size (op_list: operand list) : int64 option =
    let helper (acc: int64 option) (op: operand) : int64 option =
      match op, acc with
      | RegOp _, Some size -> Some size (* Note: this result should not be used when operand size does not match!!! *)
        (* if get_reg_size r = size then acc else isa_error "reg size does not match" *)
      | RegOp r, None -> Some (get_reg_size r)
      | _, _ -> acc
    in
    List.fold_left helper None op_list

  type instruction =
    (* | Mov of operand * operand
    | MovS of operand * operand
    | MovZ of operand * operand
    | Lea of operand * operand *)
    | BInst of bop * operand * operand * operand
    | UInst of uop * operand * operand
    | Xchg of operand * operand * operand * operand
    | Cmp of operand * operand
    | Test of operand * operand
    | Push of operand * MemAnno.t
    | Pop of operand * MemAnno.t
    | RepStosq
    | RepMovsq
    | Jmp of label
    | Jcond of branch_cond * label
    | Call of label
    | Nop
    | Syscall
    | Hlt

  type basic_block = {
    label: label;
    insts: instruction list;
  }

  type func = {
    name: label;
    body: basic_block list;
  }

  type prog = {
    funcs: func list;
    imm_var_map: imm_var_map;
  }

  type program = {
    bbs: basic_block list;
    imm_var_map: imm_var_map;
  }

  let get_func (p: prog) (func_name: label) : func =
    List.find (fun (x: func) -> x.name = func_name) p.funcs

  let inst_is_uncond_jump (inst: instruction) : bool =
    match inst with
    | Jmp _ -> true
    | _ -> false

  let mnemonic_of_instruction (inst: instruction) : string =
    match inst with
    (* | Mov _ -> "mov"
    | MovS _ -> "movs"
    | MovZ _ -> "movz"
    | Lea _ -> "lea" *)
    | BInst (bop, _, _, _) ->
      begin match opcode_of_binst bop with
      | Some s -> s
      | None -> isa_error "cannot find opcode for a bop"
      end
    | UInst (uop, _, _) ->
      begin match opcode_of_uinst uop with
      | Some s -> s
      | None -> isa_error "cannot find opcode for a uop"
      end
    | Cmp _ -> "cmp"
    | Test _ -> "test"
    | Push _ -> "push"
    | Pop _ -> "pop"
    | Xchg _ -> "xchg"
    | RepStosq -> "rep stosq"
    | RepMovsq -> "rep movsq"
    | Jmp _ -> "jmp"
    | Jcond (cond, _) ->
      begin match opcode_of_cond_jump cond with
      | Some s -> s
      | None -> isa_error "cannot find opcode for a cond jump"
      end
    | Call _ -> "call"
    | Nop -> "nop"
    | Syscall -> "syscall"
    | Hlt -> "hlt"

  let string_of_instruction (inst: instruction) : string =
    let get_tab (opcode: string): string =
      if String.length opcode <= 3 then "\t\t" else "\t"
    in
    match inst with
    | BInst (bop, dst, src2, src1) ->
      begin match opcode_of_binst bop with
      | Some opcode -> 
        Printf.sprintf 
          "%s%s%s\t<-\t%s,\t%s" 
          opcode (get_tab opcode) (string_of_operand dst) (string_of_operand src2) (string_of_operand src1)
      | None -> isa_error "cannot find opcode for a bop"
      end
    | UInst (uop, dst, src) ->
      begin match opcode_of_uinst uop with
      | Some opcode ->
        Printf.sprintf 
            "%s%s%s\t<-\t%s" 
            opcode (get_tab opcode) (string_of_operand dst) (string_of_operand src)
      | None -> isa_error "cannot find opcode for a uop"
      end
    | Cmp (src2, src1) ->
      Printf.sprintf "cmp\t\t%s,\t%s" (string_of_operand src2) (string_of_operand src1)
    | Test (src2, src1) ->
      Printf.sprintf "test\t%s,\t%s" (string_of_operand src2) (string_of_operand src1)
    | Push (src, mem_anno) -> Printf.sprintf "push\t%s # %s" (string_of_operand src) (MemAnno.to_string mem_anno)
    | Pop (src, mem_anno) -> Printf.sprintf "pop\t\t%s # %s" (string_of_operand src) (MemAnno.to_string mem_anno)
    | Xchg (dst2, dst1, _, _) -> 
      Printf.sprintf "xchg\t%s,\t%s" (string_of_operand dst2) (string_of_operand dst1)
    | RepStosq -> "rep stosq"
    | RepMovsq -> "rep movsq"
    | Jmp label -> Printf.sprintf "jmp\t\t%s" label
    | Jcond (cond, label) ->
      begin match opcode_of_cond_jump cond with
      | Some opcode -> Printf.sprintf "%s\t\t%s" opcode label
      | None -> isa_error "cannot find opcode for a cond jump"
      end
    | Call label -> Printf.sprintf "call\t\t%s" label
    | Nop -> "nop"
    | Syscall -> "syscall"
    | Hlt -> "hlt"

  (* let get_op_list (inst: instruction) : operand list =
    match inst with
    | Mov (op0, op1) | MovS (op0, op1) | MovZ (op0, op1) 
    | Lea (op0, op1) | Not (op0, op1) | Cmp (op0, op1) | Test (op0, op1) -> [op0; op1]
    | Add (op0, op1, op2) | Sub (op0, op1, op2) | Sal (op0, op1, op2)
    | Sar (op0, op1, op2) | Shr (op0, op1, op2) | Xor (op0, op1, op2)
    | And (op0, op1, op2) | Or (op0, op1, op2) -> [op0; op1; op2]
    | Push op | Pop op -> [op]
    | _ -> [] *)

  let pp_block (lvl: int) (bb: basic_block) =
    PP.print_lvl lvl "%s\n" bb.label;
    List.iter (
      fun inst -> PP.print_lvl (lvl + 1) "%s\n" (string_of_instruction inst)
    ) bb.insts

  let pp_block_list (lvl: int) (block_list: basic_block list) =
    List.iter (pp_block lvl) block_list

  let pp_ocaml_block_list (lvl: int) (buf: Buffer.t) (block_list: basic_block list) =
    PP.bprint_lvl lvl buf "[\n";
    List.iter (
      fun bb -> 
        PP.bprint_lvl (lvl + 1) buf "label = %s\n" bb.label;
        PP.bprint_lvl (lvl + 1) buf "insts = [\n";
        List.iter (
          fun inst -> PP.bprint_lvl (lvl + 2) buf "%s;\n" (string_of_instruction inst)
        ) bb.insts;
        PP.bprint_lvl (lvl + 1) buf "];\n";
    ) block_list;
    PP.bprint_lvl lvl buf "]\n"

  let pp_prog (lvl: int) (p: prog) =
    PP.print_lvl lvl "Prog\n";
    List.iteri (
      fun i func -> 
        PP.print_lvl lvl "<Func %d %s>\n" i func.name;
        pp_block_list (lvl + 1) func.body
    ) p.funcs


  let update_op_taint (update_func: MemAnno.t -> MemAnno.t) (op: operand) : operand =
    match op with
    | LdOp (disp, base, index, scale, size, old_mem_anno) ->
      LdOp (disp, base, index, scale, size, update_func old_mem_anno)
    | StOp (disp, base, index, scale, size, old_mem_anno) ->
      StOp (disp, base, index, scale, size, update_func old_mem_anno)
    | _ -> op
  
  let update_inst_taint (update_func: MemAnno.t -> MemAnno.t) (inst: instruction) : instruction =
    let update_helper = update_op_taint update_func in
    match inst with
    | BInst (bop, op1, op2, op3) ->
      BInst (bop, update_helper op1, update_helper op2, update_helper op3)
    | UInst (uop, op1, op2) ->
      UInst (uop, update_helper op1, update_helper op2)
    | Xchg (op1, op2, op3, op4) ->
      Xchg (update_helper op1, update_helper op2, update_helper op3, update_helper op4)
    | Cmp (op1, op2) ->
      Cmp (update_helper op1, update_helper op2)
    | Test (op1, op2) ->
      Test (update_helper op1, update_helper op2)
    | _ -> inst

  let update_block_taint (update_func: MemAnno.t -> MemAnno.t) (block: basic_block) : basic_block =
    { block with insts = List.map (update_inst_taint update_func) block.insts }

  let update_block_list_taint 
      (update_func: MemAnno.t -> MemAnno.t) (block_list: basic_block list) : basic_block list =
    List.map (update_block_taint update_func) block_list

end
