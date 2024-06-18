open Isa
open Ir
open Single_exp
open Type_exp
open Type_full_exp
open Cond_type
open Reg_type
open Mem_offset
open Mem_type
open Pretty_print

module StateType = struct
  exception StateTypeError of string
  let state_type_error msg = raise (StateTypeError ("[State Type Error] " ^ msg))

  let stack_base_id : Isa.imm_var_id = 1

  module CondVarSet = Set.Make(Int)
  
  type t = {
    reg_type: RegRangeType.t;
    mem_type: MemRangeType.t;
    cond_type: TypeExp.t * TypeExp.t;
    cond_hist: CondVarSet.t
  }

  let init_state_type 
      (start_var: TypeExp.t) : 
      TypeExp.t * t =
    let idx0, r_type = RegRangeType.init_reg_type start_var in
    let m_type = MemRangeType.init_mem_type in
    (idx0, 
    { 
      reg_type = r_type; 
      mem_type = m_type; 
      cond_type = (TypeSingle (SingleConst 0L), 
      TypeSingle (SingleConst 0L)); cond_hist = CondVarSet.empty 
    })
    
  let update_state_type
      (update_list: (Isa.imm_var_id * (MemOffset.t * bool) list) list)
      (start_var_idx: (TypeExp.type_var_id, Isa.imm_var_id) Either.t)
      (new_var_set: TypeExp.TypeVarSet.t) (drop_var_set: TypeExp.TypeVarSet.t)
      (old_state: t) :
      ((TypeExp.type_var_id, Isa.imm_var_id) Either.t) * TypeExp.TypeVarSet.t * TypeExp.TypeVarSet.t * t =
    let new_mem_type, var_idx, n_var_set, d_var_set = 
          MemRangeType.update_mem_type old_state.mem_type start_var_idx new_var_set drop_var_set update_list in
    (var_idx, n_var_set, d_var_set, { old_state with mem_type = new_mem_type })

  let rec get_imm_single_exp (i: Isa.immediate) : SingleExp.t =
    match i with
    | ImmNum v -> SingleConst v
    | ImmLabel v -> SingleVar v
    | ImmBExp (i1, i2) -> SingleBExp (SingleAdd, get_imm_single_exp i1, get_imm_single_exp i2)

  let get_imm_type (i: Isa.immediate) : TypeExp.t =
    TypeSingle (get_imm_single_exp i)

  let get_reg_type (curr_state: t) (r: Isa.register) : TypeExp.t =
    RegRangeType.get_reg_type curr_state.reg_type r

  let set_reg_type (curr_state: t) (r: Isa.register) (new_type: TypeExp.t) : t =
     {curr_state with reg_type = RegRangeType.set_reg_type curr_state.reg_type r new_type}

  let get_mem_op_ir_op (base: Isa.register option) (index: Isa.register option) : Ir.operand list =
    match base, index with
    | Some r1, Some r2 -> [RegOp (Isa.get_reg_idx r1); RegOp (Isa.get_reg_idx r2)]
    | Some r, None
    | None, Some r -> [RegOp (Isa.get_reg_idx r)]
    | None, None -> []

  let get_mem_op_type (curr_state: t)
      (disp: Isa.immediate option) (base: Isa.register option)
      (index: Isa.register option) (scale: Isa.scale option) : TypeExp.t =
    let disp_type = match disp with
    | Some d -> get_imm_type d
    | None -> get_imm_type (ImmNum 0L)
    in
    let base_type = match base with
    | Some b -> get_reg_type curr_state b
    | None -> get_imm_type (ImmNum 0L)
    in
    let index_type = match index with
    | Some i -> get_reg_type curr_state i
    | None -> get_imm_type (ImmNum 0L)
    in
    let scale_val = match scale with
    | Some s -> Isa.scale_val s
    | None -> 1L
    in
    let new_type = TypeExp.eval (
      TypeBExp (TypeAdd,
        TypeBExp (TypeAdd, base_type, disp_type),
        TypeBExp (TypeMul, index_type, TypeSingle (SingleConst scale_val))
      )
    ) in
    new_type

  let get_ld_op_type 
      (sol: (TypeExp.type_var_id * TypeFullExp.type_sol) list)
      (curr_state: t)
      (disp: Isa.immediate option) (base: Isa.register option)
      (index: Isa.register option) (scale: Isa.scale option)
      (size: int64) : 
      ((Isa.imm_var_id * MemOffset.t * bool) * TypeExp.t * MemOffset.ConstraintSet.t, TypeExp.t * int64) Either.t =
    let addr_type = get_mem_op_type curr_state disp base index scale in
    let addr_type, _ = TypeFullExp.repl_all_sol sol (addr_type, curr_state.cond_hist) in
    MemRangeType.get_mem_entry_with_addr curr_state.mem_type (addr_type, size)

  let set_st_op_type 
      (sol: (TypeExp.type_var_id * TypeFullExp.type_sol) list)
      (curr_state: t)
      (disp: Isa.immediate option) (base: Isa.register option)
      (index: Isa.register option) (scale: Isa.scale option)
      (size: int64)
      (new_type: TypeExp.t) : 
      ((Isa.imm_var_id * MemOffset.t * bool) * t * MemOffset.ConstraintSet.t, TypeExp.t * int64) Either.t =
    let addr_type = get_mem_op_type curr_state disp base index scale in
    let addr_type, _ = TypeFullExp.repl_all_sol sol (addr_type, curr_state.cond_hist) in
    match MemRangeType.set_mem_entry_with_addr curr_state.mem_type (addr_type, size) new_type with
    | Left (m_key, m_type, st_constraint) -> Left (m_key, { curr_state with mem_type = m_type }, st_constraint)
    | Right other -> Right other

  let get_src_op_type 
      (sol: (TypeExp.type_var_id * TypeFullExp.type_sol) list)
      (curr_state: t)
      (src: Isa.operand) : (Ir.operand list) * TypeExp.t * ((TypeExp.t * int64) option) * MemOffset.ConstraintSet.t =
    match src with
    | ImmOp imm -> ([], get_imm_type imm, None, MemOffset.ConstraintSet.empty)
    | RegOp r -> ([RegOp (Isa.get_reg_idx r)], get_reg_type curr_state r, None, MemOffset.ConstraintSet.empty)
    | MemOp (disp, base, index, scale) -> 
      (get_mem_op_ir_op base index,get_mem_op_type curr_state disp base index scale, None, MemOffset.ConstraintSet.empty)
    | LdOp (disp, base, index, scale, size) ->
      begin match get_ld_op_type sol curr_state disp base index scale size with
      | Left ((ptr, offset, is_single_full_slot), exp, ld_constraint) -> ([MemOp (ptr, offset, is_single_full_slot)], exp, None, ld_constraint)
      | Right (addr, size) -> ([UnknownOp], TypeExp.TypePtr (addr, size), Some (addr, size), MemOffset.ConstraintSet.empty)
      end
    | StOp _ -> state_type_error ("get_src_op_type: cannot get src op type of a st op")
    | LabelOp _ -> state_type_error ("get_src_op_type: cannot get src op type of a label op")

  let set_dest_op_type
      (sol: (TypeExp.type_var_id * TypeFullExp.type_sol) list)
      (curr_state: t) (dest: Isa.operand) (new_type: TypeExp.t) : Ir.operand * t * ((TypeExp.t * int64) option) * MemOffset.ConstraintSet.t =
    match dest with
    | RegOp r -> (RegOp (Isa.get_reg_idx r), set_reg_type curr_state r new_type, None, MemOffset.ConstraintSet.empty)
    | StOp (disp, base, index, scale, size) ->
      begin match set_st_op_type sol curr_state disp base index scale size new_type with
      | Left ((ptr, offset, is_single_full_slot), exp, st_constraint) -> (MemOp (ptr, offset, is_single_full_slot), exp, None, st_constraint)
      | Right addr_exp -> (UnknownOp, curr_state, Some addr_exp, MemOffset.ConstraintSet.empty)
      end
    | _ -> state_type_error ("set_dest_op_type: dest is not reg or st op")

  let add_state_type_cond (curr_state: t) (cond_idx: int) : t =
    { curr_state with cond_hist = CondVarSet.add cond_idx curr_state.cond_hist }

  (* TODO: Generate IR here!!! *)
  let type_prop_inst
      (sol: (TypeExp.type_var_id * TypeFullExp.type_sol) list)
      (curr_state: t) 
      (cond_list: CondType.t list) 
      (unknown_addr_list: (TypeFullExp.t * int64) list)
      (constraint_set: MemOffset.ConstraintSet.t)
      (inst: Isa.instruction) : t * (CondType.t list) * ((TypeFullExp.t * int64) list) * MemOffset.ConstraintSet.t * Ir.t =
    let get_unknown_list
      (new_unknown_list: ((TypeExp.t * int64) option) list) : (TypeFullExp.t * int64) list =
      List.filter_map (
        fun x ->
          match x with
          | None -> None
          | Some (addr, size) -> Some ((addr, curr_state.cond_hist), size)
      ) new_unknown_list
    in
    match inst with
    | Mov (dest, src)
    | MovS (dest, src)
    | MovZ (dest, src)
    | Lea (dest, src) ->
      let src_key, src_type, ua1, src_constraint = get_src_op_type sol curr_state src in
      let dest_key, new_state, ua2, dest_constraint = set_dest_op_type sol curr_state dest src_type in
      (new_state, cond_list, 
      (get_unknown_list [ua2; ua1]) @ unknown_addr_list,
      MemOffset.constraint_union [constraint_set; src_constraint; dest_constraint],
      Mov (dest_key, src_key))
    | Add (dest, src1, src2) ->
      let src1_key, src1_type, ua1, src1_constraint = get_src_op_type sol curr_state src1 in
      let src2_key, src2_type, ua2, src2_constraint = get_src_op_type sol curr_state src2 in
      let dest_type = TypeExp.eval (TypeBExp (TypeAdd, src1_type, src2_type)) in
      let dest_key, new_state, ua3, dest_constraint = set_dest_op_type sol curr_state dest dest_type in
      (new_state, cond_list, 
      (get_unknown_list [ua3; ua2; ua1]) @ unknown_addr_list,
      MemOffset.constraint_union [constraint_set; src1_constraint; src2_constraint; dest_constraint],
      Mov (dest_key, src1_key @ src2_key))
    | Sub (dest, src1, src2) ->
      let src1_key, src1_type, ua1, src1_constraint = get_src_op_type sol curr_state src1 in
      let src2_key, src2_type, ua2, src2_constraint = get_src_op_type sol curr_state src2 in
      let dest_type = TypeExp.eval (TypeBExp (TypeSub, src1_type, src2_type)) in
      let dest_key, new_state, ua3, dest_constraint = set_dest_op_type sol curr_state dest dest_type in
      (new_state, cond_list, 
      (get_unknown_list [ua3; ua2; ua1]) @ unknown_addr_list,
      MemOffset.constraint_union [constraint_set; src1_constraint; src2_constraint; dest_constraint],
      Mov (dest_key, src1_key @ src2_key))
    | Sal (dest, src1, src2) ->
      let src1_key, src1_type, ua1, src1_constraint = get_src_op_type sol curr_state src1 in
      let src2_key, src2_type, ua2, src2_constraint = get_src_op_type sol curr_state src2 in
      let dest_type = TypeExp.eval (TypeBExp (TypeSal, src1_type, src2_type)) in
      let dest_key, new_state, ua3, dest_constraint = set_dest_op_type sol curr_state dest dest_type in
      (new_state, cond_list, 
      (get_unknown_list [ua3; ua2; ua1]) @ unknown_addr_list,
      MemOffset.constraint_union [constraint_set; src1_constraint; src2_constraint; dest_constraint],
      Mov (dest_key, src1_key @ src2_key))
    | Shr (dest, src1, src2) ->
      let src1_key, src1_type, ua1, src1_constraint = get_src_op_type sol curr_state src1 in
      let src2_key, src2_type, ua2, src2_constraint = get_src_op_type sol curr_state src2 in
      let dest_type = TypeExp.eval (TypeBExp (TypeShr, src1_type, src2_type)) in
      let dest_key, new_state, ua3, dest_constraint = set_dest_op_type sol curr_state dest dest_type in
      (new_state, cond_list, 
      (get_unknown_list [ua3; ua2; ua1]) @ unknown_addr_list,
      MemOffset.constraint_union [constraint_set; src1_constraint; src2_constraint; dest_constraint],
      Mov (dest_key, src1_key @ src2_key))
    | Sar (dest, src1, src2) ->
      let src1_key, src1_type, ua1, src1_constraint = get_src_op_type sol curr_state src1 in
      let src2_key, src2_type, ua2, src2_constraint = get_src_op_type sol curr_state src2 in
      let dest_type = TypeExp.eval (TypeBExp (TypeSar, src1_type, src2_type)) in
      let dest_key, new_state, ua3, dest_constraint = set_dest_op_type sol curr_state dest dest_type in
      (new_state, cond_list, 
      (get_unknown_list [ua3; ua2; ua1]) @ unknown_addr_list,
      MemOffset.constraint_union [constraint_set; src1_constraint; src2_constraint; dest_constraint],
      Mov (dest_key, src1_key @ src2_key))
    | Xor (dest, src1, src2) ->
      (* TODO: Add special case for xor %rax %rax *)
      begin match src1, src2 with
      | RegOp r1, RegOp r2 ->
        if r1 = r2 then 
          let dest_key, new_state, ua, dest_constraint = set_dest_op_type sol curr_state dest (TypeSingle (SingleConst 0L)) in
          (new_state, cond_list, 
          (get_unknown_list [ua]) @ unknown_addr_list,
          MemOffset.ConstraintSet.union constraint_set dest_constraint,
          Mov (dest_key, []))
        else begin
          let src1_key, src1_type, ua1, src1_constraint = get_src_op_type sol curr_state src1 in
          let src2_key, src2_type, ua2, src2_constraint = get_src_op_type sol curr_state src2 in
          let dest_type = TypeExp.eval (TypeBExp (TypeXor, src1_type, src2_type)) in
          let dest_key, new_state, ua3, dest_constraint = set_dest_op_type sol curr_state dest dest_type in
          (new_state, cond_list, 
          (get_unknown_list [ua3; ua2; ua1]) @ unknown_addr_list,
          MemOffset.constraint_union [constraint_set; src1_constraint; src2_constraint; dest_constraint],
          Mov (dest_key, src1_key @ src2_key))
        end
      | _ ->
        let src1_key, src1_type, ua1, src1_constraint = get_src_op_type sol curr_state src1 in
        let src2_key, src2_type, ua2, src2_constraint = get_src_op_type sol curr_state src2 in
        let dest_type = TypeExp.eval (TypeBExp (TypeXor, src1_type, src2_type)) in
        let dest_key, new_state, ua3, dest_constraint = set_dest_op_type sol curr_state dest dest_type in
        (new_state, cond_list, 
        (get_unknown_list [ua3; ua2; ua1]) @ unknown_addr_list,
        MemOffset.constraint_union [constraint_set; src1_constraint; src2_constraint; dest_constraint],
        Mov (dest_key, src1_key @ src2_key))
      end
    | Not (dest, src) ->
      let src_key, src_type, ua1, src_constraint = get_src_op_type sol curr_state src in
      let dest_type = TypeExp.eval (TypeUExp (TypeNot, src_type)) in
      let dest_key, new_state, ua2, dest_constraint = set_dest_op_type sol curr_state dest dest_type in
      (new_state, cond_list, 
      (get_unknown_list [ua2; ua1]) @ unknown_addr_list,
      MemOffset.constraint_union [constraint_set; src_constraint; dest_constraint],
      Mov (dest_key, src_key))
    | And (dest, src1, src2) ->
      let src1_key, src1_type, ua1, src1_constraint = get_src_op_type sol curr_state src1 in
      let src2_key, src2_type, ua2, src2_constraint = get_src_op_type sol curr_state src2 in
      let dest_type = TypeExp.eval (TypeBExp (TypeAnd, src1_type, src2_type)) in
      let dest_key, new_state, ua3, dest_constraint = set_dest_op_type sol curr_state dest dest_type in
      (new_state, cond_list, 
      (get_unknown_list [ua3; ua2; ua1]) @ unknown_addr_list,
      MemOffset.constraint_union [constraint_set; src1_constraint; src2_constraint; dest_constraint],
      Mov (dest_key, src1_key @ src2_key))
    | Or (dest, src1, src2) ->
      let src1_key, src1_type, ua1, src1_constraint = get_src_op_type sol curr_state src1 in
      let src2_key, src2_type, ua2, src2_constraint = get_src_op_type sol curr_state src2 in
      let dest_type = TypeExp.eval (TypeBExp (TypeOr, src1_type, src2_type)) in
      let dest_key, new_state, ua3, dest_constraint = set_dest_op_type sol curr_state dest dest_type in
      (new_state, cond_list, 
      (get_unknown_list [ua3; ua2; ua1]) @ unknown_addr_list,
      MemOffset.constraint_union [constraint_set; src1_constraint; src2_constraint; dest_constraint],
      Mov (dest_key, src1_key @ src2_key))
    | Cmp (src1, src2) ->
      let _, src1_type, ua1, src1_constraint = get_src_op_type sol curr_state src1 in
      let _, src2_type, ua2, src2_constraint = get_src_op_type sol curr_state src2 in
      ({curr_state with cond_type = (src1_type, src2_type)}, cond_list, 
      (get_unknown_list [ua2; ua1]) @ unknown_addr_list,
      MemOffset.constraint_union [constraint_set; src1_constraint; src2_constraint],
      Skip)
      (* TODO: Handle the case where some other instructions set flags and some other instructions use flags!!! *)
    | Test (src1, src2) ->
      let _, src1_type, ua1, src1_constraint = get_src_op_type sol curr_state src1 in
      let _, src2_type, ua2, src2_constraint = get_src_op_type sol curr_state src2 in
      let src_and_type = 
        if Isa.cmp_operand src1 src2 then src1_type else TypeExp.TypeBExp (TypeAnd, src1_type, src2_type) in
      ({curr_state with cond_type = (src_and_type, TypeExp.TypeSingle (SingleConst 0L))}, cond_list, 
      (get_unknown_list [ua2; ua1]) @ unknown_addr_list,
      MemOffset.constraint_union [constraint_set; src1_constraint; src2_constraint],
      Skip)
    | Jcond (target, branch_cond) ->
      let cond_left, cond_right = curr_state.cond_type in
      let new_cond = 
        begin match branch_cond with
        | JNe -> CondType.CondNe ((cond_left, cond_left), (cond_right, cond_right), curr_state.cond_hist)
        | JE -> CondType.CondEq ((cond_left, cond_left), (cond_right, cond_right), curr_state.cond_hist)
        | JL -> CondType.CondLq ((cond_left, cond_left), (cond_right, cond_right), curr_state.cond_hist)
        | JLe -> CondType.CondLe ((cond_left, cond_left), (cond_right, cond_right), curr_state.cond_hist)
        | JG -> CondType.CondLq ((cond_right, cond_right), (cond_left, cond_left), curr_state.cond_hist)
        | JGe -> CondType.CondLe ((cond_right, cond_right), (cond_left, cond_left), curr_state.cond_hist)
        end in
      let new_cond_list, new_cond_idx = CondType.add_cond_type cond_list new_cond false in
      (add_state_type_cond curr_state new_cond_idx, new_cond_list, unknown_addr_list, constraint_set,
      Jump target)
    | Jmp target -> (curr_state, cond_list, unknown_addr_list, constraint_set, Jump target)
    | Push _ -> (* Note: We do not track mem access in push/pop*)
      let _, src1_type, ua1, src1_constraint = get_src_op_type sol curr_state (Isa.RegOp Isa.RSP) in
      let src2_type = TypeExp.TypeSingle (SingleConst (-8L)) in
      (* let src2_type, src2_cond = get_src_op_type curr_state src2 in *)
      let dest_type = TypeExp.eval (TypeBExp (TypeAdd, src1_type, src2_type)) in
      let _, new_state, ua2, dest_constraint = set_dest_op_type sol curr_state (Isa.RegOp Isa.RSP) dest_type in
      (new_state, cond_list, 
      (get_unknown_list [ua2; ua1]) @ unknown_addr_list,
      MemOffset.constraint_union [constraint_set; src1_constraint; dest_constraint],
      Skip)
    | Pop _ ->
      let _, src1_type, ua1, src1_constraint = get_src_op_type sol curr_state (Isa.RegOp Isa.RSP) in
      let src2_type = TypeExp.TypeSingle (SingleConst 8L) in
      (* let src2_type, src2_cond = get_src_op_type curr_state src2 in *)
      let dest_type = TypeExp.eval (TypeBExp (TypeAdd, src1_type, src2_type)) in
      let _, new_state, ua2, dest_constraint = set_dest_op_type sol curr_state (Isa.RegOp Isa.RSP) dest_type in
      (new_state, cond_list, 
      (get_unknown_list [ua2; ua1]) @ unknown_addr_list,
      MemOffset.constraint_union [constraint_set; src1_constraint; dest_constraint],
      Skip)
    | _ -> begin
        print_endline ("[Warning] type_prop_inst: instruction not handled: " ^ (Isa.mnemonic_of_instruction inst));
        (curr_state, cond_list, unknown_addr_list, constraint_set, Skip) (* TODO *)
    end

  let pp_state_type (lvl: int) (s: t) =
    PP.print_lvl lvl "Reg:\n";
    List.iteri (fun i x ->
      PP.print_lvl (lvl + 1) "<%s> " (Isa.string_of_reg_idx i);
      TypeExp.pp_type_exp (lvl + 2) x;
      Printf.printf "\n"
    ) s.reg_type;
    PP.print_lvl lvl "Mem:\n";
    MemRangeType.pp_ptr_set (lvl + 1) s.mem_type.ptr_list;
    MemRangeType.pp_mem_type (lvl + 1) s.mem_type.mem_type;
    PP.print_lvl lvl "Cond:\n";
    TypeExp.pp_type_exp (lvl + 1) (fst s.cond_type);
    Printf.printf "\n";
    TypeExp.pp_type_exp (lvl + 1) (snd s.cond_type)

end

