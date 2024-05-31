open Isa
open Single_exp
open Type_exp
open Type_full_exp
open Cond_type
open Reg_type
open Mem_type
(* open Pretty_print *)

module StateType = struct
  exception StateTypeError of string
  let state_type_error msg = raise (StateTypeError ("[State Type Error] " ^ msg))

  let stack_base_id : Isa.imm_var_id = 1

  module CondVarSet = Set.Make(Int)
  
  type t = {
    reg_type: RegType.t;
    mem_type: MemType.t;
    cond_type: TypeExp.t * TypeExp.t;
    cond_hist: CondVarSet.t
  }

  let rec get_imm_single_exp (i: Isa.immediate) : SingleExp.t =
    match i with
    | ImmNum v -> SingleConst v
    | ImmLabel v -> SingleVar v
    | ImmBExp (i1, i2) -> SingleBExp (SingleAdd, get_imm_single_exp i1, get_imm_single_exp i2)

  let get_imm_type (i: Isa.immediate) : TypeExp.t =
    TypeSingle (get_imm_single_exp i)

  let get_reg_type (curr_state: t) (r: Isa.register) : TypeExp.t =
    RegType.get_reg_type curr_state.reg_type r

  let set_reg_type (curr_state: t) (r: Isa.register) (new_type: TypeExp.t) : t =
     {curr_state with reg_type = RegType.set_reg_type curr_state.reg_type r new_type}

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

  let get_ld_op_type (curr_state: t)
      (disp: Isa.immediate option) (base: Isa.register option)
      (index: Isa.register option) (scale: Isa.scale option)
      (size: int64) : (TypeExp.t, TypeExp.t * int64) Either.t =
    let addr_type = get_mem_op_type curr_state disp base index scale in
    MemType.get_mem_type curr_state.mem_type (addr_type, size)

  let set_st_op_type (curr_state: t)
      (disp: Isa.immediate option) (base: Isa.register option)
      (index: Isa.register option) (scale: Isa.scale option)
      (size: int64)
      (new_type: TypeExp.t) : (t, TypeExp.t * int64) Either.t =
    let addr_type = get_mem_op_type curr_state disp base index scale in
    match MemType.set_mem_type curr_state.mem_type (addr_type, size) new_type with
    | Left m_type -> Left { curr_state with mem_type = m_type }
    | Right other -> Right other

  let get_src_op_type 
    (curr_state: t)
    (src: Isa.operand) : TypeExp.t * ((TypeExp.t * int64) option) =
    match src with
    | ImmOp imm -> (get_imm_type imm, None)
    | RegOp r -> (get_reg_type curr_state r, None)
    | MemOp (disp, base, index, scale) -> (get_mem_op_type curr_state disp base index scale, None)
    | LdOp (disp, base, index, scale, size) ->
      begin match get_ld_op_type curr_state disp base index scale size with
      | Left exp -> (exp, None)
      | Right (addr, size) -> (TypeExp.TypePtr (addr, size), Some (addr, size))
      end
    | StOp _ -> state_type_error ("get_src_op_type: cannot get src op type of a st op")
    | LabelOp _ -> state_type_error ("get_src_op_type: cannot get src op type of a label op")

  let set_dest_op_type (curr_state: t) (dest: Isa.operand) (new_type: TypeExp.t) : t * ((TypeExp.t * int64) option) =
    match dest with
    | RegOp r -> (set_reg_type curr_state r new_type, None)
    | StOp (disp, base, index, scale, size) ->
      begin match set_st_op_type curr_state disp base index scale size new_type with
      | Left exp -> (exp, None)
      | Right addr_exp -> (curr_state, Some addr_exp)
      end
    | _ -> state_type_error ("set_dest_op_type: dest is not reg or st op")

  let add_state_type_cond (curr_state: t) (cond_idx: int) : t =
    { curr_state with cond_hist = CondVarSet.add cond_idx curr_state.cond_hist }

  let type_prop_inst 
      (curr_state: t) 
      (cond_list: CondType.t list) 
      (unknown_addr_list: (TypeFullExp.t * int64) list) 
      (inst: Isa.instruction) : t * (CondType.t list) * ((TypeFullExp.t * int64) list) =
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
      let src_type, ua1 = get_src_op_type curr_state src in
      let new_state, ua2 = set_dest_op_type curr_state dest src_type in
      (new_state, cond_list, (get_unknown_list [ua2; ua1]) @ unknown_addr_list)
    | Add (dest, src1, src2) ->
      let src1_type, ua1 = get_src_op_type curr_state src1 in
      let src2_type, ua2 = get_src_op_type curr_state src2 in
      let dest_type = TypeExp.eval (TypeBExp (TypeAdd, src1_type, src2_type)) in
      let new_state, ua3 = set_dest_op_type curr_state dest dest_type in
      (new_state, cond_list, (get_unknown_list [ua3; ua2; ua1]) @ unknown_addr_list)
    | Sub (dest, src1, src2) ->
      let src1_type, ua1 = get_src_op_type curr_state src1 in
      let src2_type, ua2 = get_src_op_type curr_state src2 in
      let dest_type = TypeExp.eval (TypeBExp (TypeSub, src1_type, src2_type)) in
      let new_state, ua3 = set_dest_op_type curr_state dest dest_type in
      (new_state, cond_list, (get_unknown_list [ua3; ua2; ua1]) @ unknown_addr_list)
    | Sal (dest, src1, src2) ->
      let src1_type, ua1 = get_src_op_type curr_state src1 in
      let src2_type, ua2 = get_src_op_type curr_state src2 in
      let dest_type = TypeExp.eval (TypeBExp (TypeSal, src1_type, src2_type)) in
      let new_state, ua3 = set_dest_op_type curr_state dest dest_type in
      (new_state, cond_list, (get_unknown_list [ua3; ua2; ua1]) @ unknown_addr_list)
    | Shr (dest, src1, src2) ->
      let src1_type, ua1 = get_src_op_type curr_state src1 in
      let src2_type, ua2 = get_src_op_type curr_state src2 in
      let dest_type = TypeExp.eval (TypeBExp (TypeShr, src1_type, src2_type)) in
      let new_state, ua3 = set_dest_op_type curr_state dest dest_type in
      (new_state, cond_list, (get_unknown_list [ua3; ua2; ua1]) @ unknown_addr_list)
    | Sar (dest, src1, src2) ->
      let src1_type, ua1 = get_src_op_type curr_state src1 in
      let src2_type, ua2 = get_src_op_type curr_state src2 in
      let dest_type = TypeExp.eval (TypeBExp (TypeSar, src1_type, src2_type)) in
      let new_state, ua3 = set_dest_op_type curr_state dest dest_type in
      (new_state, cond_list, (get_unknown_list [ua3; ua2; ua1]) @ unknown_addr_list)
    | Xor (dest, src1, src2) ->
      (* TODO: Add special case for xor %rax %rax *)
      begin match src1, src2 with
      | RegOp r1, RegOp r2 ->
        if r1 = r2 then 
          let new_state, ua = set_dest_op_type curr_state dest (TypeSingle (SingleConst 0L)) in
          (new_state, cond_list, (get_unknown_list [ua]) @ unknown_addr_list)
        else begin
          let src1_type, ua1 = get_src_op_type curr_state src1 in
          let src2_type, ua2 = get_src_op_type curr_state src2 in
          let dest_type = TypeExp.eval (TypeBExp (TypeXor, src1_type, src2_type)) in
          let new_state, ua3 = set_dest_op_type curr_state dest dest_type in
          (new_state, cond_list, (get_unknown_list [ua3; ua2; ua1]) @ unknown_addr_list)
        end
      | _ ->
        let src1_type, ua1 = get_src_op_type curr_state src1 in
        let src2_type, ua2 = get_src_op_type curr_state src2 in
        let dest_type = TypeExp.eval (TypeBExp (TypeXor, src1_type, src2_type)) in
        let new_state, ua3 = set_dest_op_type curr_state dest dest_type in
        (new_state, cond_list, (get_unknown_list [ua3; ua2; ua1]) @ unknown_addr_list)
      end
    | Not (dest, src) ->
      let src_type, ua1 = get_src_op_type curr_state src in
      let dest_type = TypeExp.eval (TypeUExp (TypeNot, src_type)) in
      let new_state, ua2 = set_dest_op_type curr_state dest dest_type in
      (new_state, cond_list, (get_unknown_list [ua2; ua1]) @ unknown_addr_list)
    | And (dest, src1, src2) ->
      let src1_type, ua1 = get_src_op_type curr_state src1 in
      let src2_type, ua2 = get_src_op_type curr_state src2 in
      let dest_type = TypeExp.eval (TypeBExp (TypeAnd, src1_type, src2_type)) in
      let new_state, ua3 = set_dest_op_type curr_state dest dest_type in
      (new_state, cond_list, (get_unknown_list [ua3; ua2; ua1]) @ unknown_addr_list)
    | Or (dest, src1, src2) ->
      let src1_type, ua1 = get_src_op_type curr_state src1 in
      let src2_type, ua2 = get_src_op_type curr_state src2 in
      let dest_type = TypeExp.eval (TypeBExp (TypeOr, src1_type, src2_type)) in
      let new_state, ua3 = set_dest_op_type curr_state dest dest_type in
      (new_state, cond_list, (get_unknown_list [ua3; ua2; ua1]) @ unknown_addr_list)
    | Cmp (src1, src2) ->
      let src1_type, ua1 = get_src_op_type curr_state src1 in
      let src2_type, ua2 = get_src_op_type curr_state src2 in
      ({curr_state with cond_type = (src1_type, src2_type)}, cond_list, (get_unknown_list [ua2; ua1]) @ unknown_addr_list)
      (* TODO: Handle the case where some other instructions set flags and some other instructions use flags!!! *)
    | Jcond (_, branch_cond) ->
      let cond_left, cond_right = curr_state.cond_type in
      let new_cond = 
        begin match branch_cond with
        | JNe -> CondType.CondNe (cond_left, cond_right)
        | JE -> CondType.CondEq (cond_left, cond_right)
        | JL -> CondType.CondLq (cond_left, cond_right)
        | JLe -> CondType.CondLe (cond_left, cond_right)
        | JG -> CondType.CondLq (cond_right, cond_left)
        | JGe -> CondType.CondLe (cond_right, cond_left)
        end in
      let new_cond_list, new_cond_idx = CondType.add_cond_type cond_list new_cond false in
      (add_state_type_cond curr_state new_cond_idx, new_cond_list, unknown_addr_list)
    | Jmp _ -> (curr_state, cond_list, unknown_addr_list)
    | Push _ ->
      let src1_type, ua1 = get_src_op_type curr_state (Isa.RegOp Isa.RSP) in
      let src2_type = TypeExp.TypeSingle (SingleConst (-8L)) in
      (* let src2_type, src2_cond = get_src_op_type curr_state src2 in *)
      let dest_type = TypeExp.eval (TypeBExp (TypeAdd, src1_type, src2_type)) in
      let new_state, ua2 = set_dest_op_type curr_state (Isa.RegOp Isa.RSP) dest_type in
      (new_state, cond_list, (get_unknown_list [ua2; ua1]) @ unknown_addr_list)
    | Pop _ ->
      let src1_type, ua1 = get_src_op_type curr_state (Isa.RegOp Isa.RSP) in
      let src2_type = TypeExp.TypeSingle (SingleConst 8L) in
      (* let src2_type, src2_cond = get_src_op_type curr_state src2 in *)
      let dest_type = TypeExp.eval (TypeBExp (TypeAdd, src1_type, src2_type)) in
      let new_state, ua2 = set_dest_op_type curr_state (Isa.RegOp Isa.RSP) dest_type in
      (new_state, cond_list, (get_unknown_list [ua2; ua1]) @ unknown_addr_list)
    | _ -> begin
        print_endline ("[Warning] type_prop_inst: instruction not handled: " ^ (Isa.mnemonic_of_instruction inst));
        (curr_state, cond_list, unknown_addr_list) (* TODO *)
    end

end

