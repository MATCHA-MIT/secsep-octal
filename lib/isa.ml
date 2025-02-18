(* ISA interface *)
open Isa_basic
open Pretty_print
open Mem_anno_type
open Branch_anno_type
open Call_anno_type
open Constraint
open Sexplib.Std

module Isa (MemAnno: MemAnnoType) = struct

  include IsaBasic

  (* disp, base, index, scale *)
  type mem_op = immediate option * register option * register option * scale option * data_size option
  [@@deriving sexp]

  type ldst_op = immediate option * register option * register option * scale option * data_size * MemAnno.t
  [@@deriving sexp]

  type operand =
    | ImmOp of immediate
    | RegOp of register
    | RegMultOp of register list
    | MemOp of mem_op
    | LdOp of ldst_op
    | StOp of ldst_op
    | LabelOp of label
  [@@deriving sexp]

  (* operand utilities *)

  let get_reg_mult_op_size (reg_mult_op: register list) : int64 =
    List.fold_left (
      fun (acc: int64) (r: register) ->
        Int64.add acc (get_reg_size r)
    ) 0L reg_mult_op

  let get_mem_op_size (mem_op: mem_op) : data_size option =
    let _, _, _, _, size = mem_op in size

  let set_mem_op_size (mem_op: mem_op) (size: data_size option) : mem_op =
    let d, b, i, s, _ = mem_op in d, b, i, s, size

  let get_op_size (op: operand) : data_size =
    match op with
    | ImmOp imm -> IsaBasic.get_imm_size imm |> Option.get
    | RegOp r -> get_reg_size r
    | LdOp (_, _, _, _, size, _)
    | StOp (_, _, _, _, size, _) -> size
    | _ -> isa_error "cannot get size for the given op"

  let memop_to_mem_operand (sz_opt: data_size option) (op: mem_op) = 
    let d, b, i, s, _ = op in
    MemOp (d, b, i, s, sz_opt)

  let extract_memop_of_ldst (op: operand) =
    match op with
    | LdOp (d, b, i, s, sz, _) -> MemOp (d, b, i, s, Some sz)
    | StOp (d, b, i, s, sz, _) -> MemOp (d, b, i, s, Some sz)
    | _ -> op

  (* TODO: Remove this function *)
  let cmp_operand (op1: operand) (op2: operand) : bool = (* true for equal *)
    match op1, op2 with
    | ImmOp i1, ImmOp i2 -> i1 = i2
    | RegOp r1, RegOp r2 -> r1 = r2
    | MemOp (d1, b1, i1, s1, sz1), MemOp (d2, b2, i2, s2, sz2) ->
      d1 = d2 && b1 = b2 && i1 = i2 && s1 = s2 && sz1 = sz2
    | LdOp (d1, b1, i1, s1, size1, _), LdOp (d2, b2, i2, s2, size2, _)
    | StOp (d1, b1, i1, s1, size1, _), StOp (d2, b2, i2, s2, size2, _) ->
        d1 = d2 && b1 = b2 && i1 = i2 && s1 = s2 && size1 = size2
    | LabelOp l1, LabelOp l2 -> l1 = l2
    | _ -> false

  let get_ld_st_related_taint_constraint
      (op1: operand) (op2: operand) : Constraint.t list =
    match op1, op2 with
    | LdOp (d1, b1, i1, s1, size1, anno1), StOp (d2, b2, i2, s2, size2, anno2)
    | StOp (d2, b2, i2, s2, size2, anno2), LdOp (d1, b1, i1, s1, size1, anno1) ->
      if d1 = d2 && b1 = b2 && i1 = i2 && s1 = s2 && size1 = size2 then
        begin match MemAnno.get_taint anno1, MemAnno.get_taint anno2 with
        | Some t1, Some t2 -> [ TaintSub (t1, t2); TaintSub (t2, t1) ]
        | _ -> []
        end
      else []
    | _ -> []

  let is_opr_callee_saved_reg (opr: operand) : register option =
    match opr with
    | RegOp r ->
      if is_reg_callee_saved r && get_reg_size r = 8L then
        Some r
      else
        None
    | _ -> None

  (* stringify *)

  let rec string_of_operand (op: operand): string =
    match op with
    | ImmOp imm -> string_of_immediate imm
    | RegOp r -> string_of_reg r
    | RegMultOp r_list -> Printf.sprintf "(%s)" (String.concat " " (List.map string_of_reg r_list))
    | MemOp (disp, base, index, scale, size) ->
      let disp_str = string_of_option string_of_immediate disp in
      let base_str = string_of_option string_of_reg base in
      let index_str = string_of_option string_of_reg index in
      let scale_str = string_of_option scale_to_string (scale) in
      let size_str = string_of_data_size size in
      Printf.sprintf "%s(%s,%s,%s)%s" disp_str base_str index_str scale_str size_str
    | LdOp (disp, base, index, scale, size, mem_anno) ->
      let addr_str = string_of_operand (MemOp (disp, base, index, scale, Some size)) in
      Printf.sprintf "Ld(%s,anno={%s})" addr_str (MemAnno.to_string mem_anno)
    | StOp (disp, base, index, scale, size, mem_anno) ->
      let addr_str = string_of_operand (MemOp (disp, base, index, scale, Some size)) in
      Printf.sprintf "St(%s,anno={%s})" addr_str (MemAnno.to_string mem_anno)
    | LabelOp label -> label

  let ocaml_string_of_operand (op: operand): string =
    match op with
    | ImmOp imm -> ocaml_string_of_immediate_op imm
    | RegOp r -> ocaml_string_of_reg_op r
    | RegMultOp r_list -> Printf.sprintf "RegMultOp (%s)" (String.concat " " (List.map ocaml_string_of_reg_op r_list))
    | MemOp (disp, base, index, scale, size) ->
      let disp_str = ocaml_string_of_option ocaml_string_of_immediate disp in
      let base_str = ocaml_string_of_option ocaml_string_of_reg base in
      let index_str = ocaml_string_of_option ocaml_string_of_reg index in
      let scale_str = ocaml_string_of_option ocaml_scale_to_string (scale) in
      let size_str = string_of_data_size size in
      Printf.sprintf "MemOp ((%s, %s, %s, %s), %s)" disp_str base_str index_str scale_str size_str
    | LdOp (disp, base, index, scale, size, mem_anno)
    | StOp (disp, base, index, scale, size, mem_anno) ->
      let disp_str = ocaml_string_of_option ocaml_string_of_immediate disp in
      let base_str = ocaml_string_of_option ocaml_string_of_reg base in
      let index_str = ocaml_string_of_option ocaml_string_of_reg index in
      let scale_str = ocaml_string_of_option ocaml_scale_to_string (scale) in
      Printf.sprintf "%s(%s, %s, %s, %s, %sL, %s)"
        (match op with LdOp _ -> "LdOp" | StOp _ -> "StOp" | _ -> isa_error "ocaml_string_of_operand")
        disp_str base_str index_str scale_str
        (Int64.to_string size) (MemAnno.to_ocaml_string mem_anno)
    | LabelOp label -> Printf.sprintf "LabelOp %s" label

  type instruction =
    | BInst of bop * operand * operand * operand
    | UInst of uop * operand * operand
    | TInst of top * operand * (operand list)
    | Xchg of operand * operand * operand * operand
    | Cmp of operand * operand
    | Test of operand * operand
    | Push of operand * MemAnno.t
    | Pop of operand * MemAnno.t
    | RepMovs of (data_size * MemAnno.t * MemAnno.t)
    | RepLods of (data_size * MemAnno.t)
    | RepStos of (data_size * MemAnno.t)
    | Jmp of label * BranchAnno.t
    | Jcond of branch_cond * label * BranchAnno.t
    | Call of label * CallAnno.t
    | Nop
    | Syscall
    | Hlt
    | Directive of string
  [@@deriving sexp]

  type basic_block = {
    label: label;
    insts: instruction list;
    mnemonics: string list; (* same length as insts *)
    orig_asm: string option list; (* same length as insts, None if the instruction is added for auxilary purpose, like jmp at the end of BB *)
  }
  [@@deriving sexp]

  type func = {
    name: label;
    body: basic_block list;
    related_gsyms: label list;
    subfunctions: label list;
  }
  [@@deriving sexp]

  type prog = {
    funcs: func list;
    imm_var_map: imm_var_map;
  }
  [@@deriving sexp]

  let get_func_of_prog (p: prog) (func_name: label) : func =
    List.find (fun (x: func) -> x.name = func_name) p.funcs

  let inst_is_uncond_jump (inst: instruction) : bool =
    match inst with
    | Jmp _ -> true
    | _ -> false

  (* simple instruction builders *)

  let make_inst_add_i_r (imm: int64) (reg: register) : instruction =
    BInst (Add, RegOp reg, RegOp reg, ImmOp (ImmNum (imm, Some (get_reg_size reg))))

  let make_inst_add_i_m64 (imm: int64) (mem_op: mem_op) : instruction =
    let size = get_gpr_full_size () in
    let d, b, i, s, _ = mem_op in
    let mem_anno = MemAnno.make_empty () in
    BInst (Add, StOp (d, b, i, s, size, mem_anno), LdOp (d, b, i, s, size, mem_anno), ImmOp (ImmNum (imm, Some size)))

  let make_inst_st_r64_m64 (reg: register) (mem_op: mem_op) : instruction =
    if get_reg_offset_size reg <> (0L, 8L) then
      isa_error "make_inst_st_r64_m64: reg size does not match";
    let size = get_gpr_full_size () in
    let d, b, i, s, _ = mem_op in
    let mem_anno = MemAnno.make_empty () in
    UInst (Mov, StOp (d, b, i, s, size, mem_anno), RegOp reg)

  let make_inst_ld_r64_m64 (reg: register) (mem_op: mem_op) : instruction =
    if get_reg_offset_size reg <> (0L, 8L) then
      isa_error "make_inst_st_r64_m64: reg size does not match";
    let size = get_gpr_full_size () in
    let d, b, i, s, _ = mem_op in
    let mem_anno = MemAnno.make_empty () in
    UInst (Mov, RegOp reg, LdOp (d, b, i, s, size, mem_anno))

  (* taint update *)

  let update_op_taint (update_func: MemAnno.t -> MemAnno.t) (op: operand) : operand =
    match op with
    | LdOp (disp, base, index, scale, size, old_mem_anno) ->
      LdOp (disp, base, index, scale, size, update_func old_mem_anno)
    | StOp (disp, base, index, scale, size, old_mem_anno) ->
      StOp (disp, base, index, scale, size, update_func old_mem_anno)
    | _ -> op
  
  let update_inst_taint 
      (update_func: MemAnno.t -> MemAnno.t)
      (update_call_func: CallAnno.t -> CallAnno.t)
      (inst: instruction) : instruction =
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
    | Push (op, mem_anno) ->
      Push (update_helper op, update_func mem_anno)
    | Pop (op, mem_anno) ->
      Pop (update_helper op, update_func mem_anno)
    | Call (op, call_anno) ->
      Call (op, update_call_func call_anno)
    | _ -> inst

  let update_block_taint 
      (update_func: MemAnno.t -> MemAnno.t) 
      (update_call_func: CallAnno.t -> CallAnno.t)
      (block: basic_block) : basic_block =
    { block with insts = List.map (update_inst_taint update_func update_call_func) block.insts }

  let update_block_list_taint 
      (update_func: MemAnno.t -> MemAnno.t)
      (update_call_func: CallAnno.t -> CallAnno.t)
      (block_list: basic_block list) : basic_block list =
    List.map (update_block_taint update_func update_call_func) block_list

  (* stringify *)

  let prog_to_file (filename: string) (p: prog) =
    let open Sexplib in
    let channel = open_out filename in
    Sexp.output_hum channel (sexp_of_prog p)

  let mnemonic_of_instruction (inst: instruction) : string =
    match inst with
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
    | TInst (top, _, _) ->
      begin match opcode_of_tinst top with
      | Some s -> s
      | None -> isa_error "cannot find opcode for a top"
      end
    | Cmp _ -> "cmp"
    | Test _ -> "test"
    | Push _ -> "push"
    | Pop _ -> "pop"
    | Xchg _ -> "xchg"
    | RepMovs _ -> "rep movs"
    | RepLods _ -> "rep lods"
    | RepStos _ -> "rep stos"
    | Jmp _ -> "jmp"
    | Jcond (cond, _, _) ->
      begin match opcode_of_cond_jump cond with
      | Some s -> s
      | None -> isa_error "cannot find opcode for a cond jump"
      end
    | Call _ -> "call"
    | Nop -> "nop"
    | Syscall -> "syscall"
    | Hlt -> "hlt"
    | Directive content -> content

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
    | TInst (top, dst, src_list) ->
      begin match opcode_of_tinst top with
      | Some opcode ->
        Printf.sprintf 
            "%s%s%s\t<-\t%s" 
            opcode (get_tab opcode) (string_of_operand dst) (String.concat ",\t" (List.map string_of_operand src_list))
      | None -> isa_error "cannot find opcode for a top"
      end
    | Cmp (src2, src1) ->
      Printf.sprintf "cmp\t\t%s,\t%s" (string_of_operand src2) (string_of_operand src1)
    | Test (src2, src1) ->
      Printf.sprintf "test\t%s,\t%s" (string_of_operand src2) (string_of_operand src1)
    | Push (src, mem_anno) -> Printf.sprintf "push\t%s # %s" (string_of_operand src) (MemAnno.to_string mem_anno)
    | Pop (src, mem_anno) -> Printf.sprintf "pop\t\t%s # %s" (string_of_operand src) (MemAnno.to_string mem_anno)
    | Xchg (dst2, dst1, _, _) -> 
      Printf.sprintf "xchg\t%s,\t%s" (string_of_operand dst2) (string_of_operand dst1)
    | RepMovs (size, anno1, anno2) ->
      Printf.sprintf "rep movs(%Ld)\t # %s # %s" size (MemAnno.to_string anno1) (MemAnno.to_string anno2)
    | RepLods (size, anno) ->
      Printf.sprintf "rep lods(%Ld)\t # %s" size (MemAnno.to_string anno)
    | RepStos (size, anno) ->
      Printf.sprintf "rep stos(%Ld)\t # %s" size (MemAnno.to_string anno)
    | Jmp (label, _) -> Printf.sprintf "jmp\t\t%s" label
    | Jcond (cond, label, _) ->
      begin match opcode_of_cond_jump cond with
      | Some opcode -> Printf.sprintf "%s\t\t%s" opcode label
      | None -> isa_error "cannot find opcode for a cond jump"
      end
    | Call (label, call_anno) -> Printf.sprintf "call\t\t%s, %s" label (CallAnno.to_string call_anno)
    | Nop -> "nop"
    | Syscall -> "syscall"
    | Hlt -> "hlt"
    | Directive content -> content

  let ocaml_string_of_instruction (inst: instruction) : string =
    match inst with
    | BInst (bop, dst, src2, src1) ->
      begin match ocaml_opcode_of_binst bop with
      | Some opcode ->
        Printf.sprintf "BInst (%s, %s, %s, %s)"
          opcode
          (ocaml_string_of_operand dst)
          (ocaml_string_of_operand src2)
          (ocaml_string_of_operand src1)
      | None -> isa_error "cannot find opcode for a bop"
      end
    | UInst (uop, dst, src) ->
      begin match ocaml_opcode_of_uinst uop with
      | Some opcode ->
        Printf.sprintf "UInst (%s, %s, %s)"
          opcode
          (ocaml_string_of_operand dst)
          (ocaml_string_of_operand src)
      | None -> isa_error "cannot find opcode for a uop"
      end
    | TInst (top, dst, src_list) ->
      begin match ocaml_opcode_of_tinst top with
      | Some opcode ->
        Printf.sprintf "UInst (%s, %s, [%s])"
          opcode
          (ocaml_string_of_operand dst)
          (String.concat "; " (List.map ocaml_string_of_operand src_list))
      | None -> isa_error "cannot find opcode for a uop"
      end
    | Cmp (src2, src1) ->
      Printf.sprintf "Cmp (%s, %s)" (ocaml_string_of_operand src2) (ocaml_string_of_operand src1)
    | Test (src2, src1) ->
      Printf.sprintf "Test (%s, %s)" (ocaml_string_of_operand src2) (ocaml_string_of_operand src1)
    | Push (src, mem_anno) ->
      Printf.sprintf "Push (%s, %s)" (ocaml_string_of_operand src) (MemAnno.to_ocaml_string mem_anno)
    | Pop (src, mem_anno) ->
      Printf.sprintf "Pop (%s, %s)" (ocaml_string_of_operand src) (MemAnno.to_ocaml_string mem_anno)
    | Xchg (dst2, dst1, o3, o4) ->
      Printf.sprintf "Xchg (%s, %s, %s, %s)"
        (ocaml_string_of_operand dst2)
        (ocaml_string_of_operand dst1)
        (ocaml_string_of_operand o3)
        (ocaml_string_of_operand o4)
    | RepMovs (size, anno1, anno2) ->
      Printf.sprintf "RepMovs (%Ld, %s, %s)" size (MemAnno.to_ocaml_string anno1) (MemAnno.to_ocaml_string anno2)
    | RepLods (size, anno) ->
      Printf.sprintf "RepLods (%Ld, %s)" size (MemAnno.to_ocaml_string anno)
    | RepStos (size, anno) ->
      Printf.sprintf "RepStos (%Ld, %s)" size (MemAnno.to_ocaml_string anno)
    | Jmp (label, _) -> Printf.sprintf "Jmp \"%s\"" label
    | Jcond (cond, label, _) ->
      begin match ocaml_opcode_of_cond_jump cond with
      | Some opcode -> Printf.sprintf "Jcond (%s, \"%s\")" opcode label
      | None -> isa_error "cannot find opcode for a cond jump"
      end
    | Call (label, call_anno) -> Printf.sprintf "Call (\"%s\", %s)" label (CallAnno.to_ocaml_string call_anno)
    | Nop -> "Nop"
    | Syscall -> "Syscall"
    | Hlt -> "Hlt"
    | Directive content -> Printf.sprintf "Directive \"%s\"" content
      
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
        PP.bprint_lvl (lvl + 1) buf "{\n";
        PP.bprint_lvl (lvl + 2) buf "label = \"%s\";\n" bb.label;
        PP.bprint_lvl (lvl + 2) buf "insts = [\n";
        List.iter (
          fun inst -> PP.bprint_lvl (lvl + 3) buf "%s;\n" (ocaml_string_of_instruction inst)
        ) bb.insts;
        PP.bprint_lvl (lvl + 2) buf "]\n";
        PP.bprint_lvl (lvl + 1) buf "};\n";
    ) block_list;
    PP.bprint_lvl lvl buf "]\n"

  let pp_prog (lvl: int) (p: prog) =
    PP.print_lvl lvl "Prog\n";
    List.iteri (
      fun i func -> 
        PP.print_lvl lvl "<Func %d %s>\n" i func.name;
        pp_block_list (lvl + 1) func.body
    ) p.funcs

end
