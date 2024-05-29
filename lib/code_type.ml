(* Type and type derivation *)
open Isa
open Single_exp
open Type_exp
open Type_full_exp
open Cond_type_old
open Pretty_print

module CodeType = struct
  exception CodeTypeError of string
  let code_type_error msg = raise (CodeTypeError ("[Code Type Error] " ^ msg))

  let stack_base_id : Isa.imm_var_id = 1

  type reg_type = TypeFullExp.t list

  type mem_type = (int64 * TypeFullExp.t) list

  type state_type = {
    reg_type: reg_type;
    mem_type: mem_type;
    cond_type: (TypeFullExp.t * TypeFullExp.t)  (* Cmp and test and etc. will set this; Cond branch will use this *)
  }

  let rec get_imm_single_exp (i: Isa.immediate) : SingleExp.t =
    match i with
    | ImmNum v -> SingleConst v
    | ImmLabel v -> SingleVar v
    | ImmBExp (i1, i2) -> SingleBExp (SingleAdd, get_imm_single_exp i1, get_imm_single_exp i2)

  let get_imm_type (i: Isa.immediate) : TypeFullExp.t =
    (TypeSingle (get_imm_single_exp i), TypeFullExp.CondVarSet.empty)

  let get_reg_type (curr_state: state_type) (r: Isa.register) : TypeFullExp.t =
    let reg_type_list = curr_state.reg_type in
    let reg_idx = Isa.get_reg_idx r in
    List.nth reg_type_list reg_idx

  let set_reg_type (curr_state: state_type) (r: Isa.register) (t: TypeFullExp.t) : state_type =
    let reg_list = curr_state.reg_type in
    let reg_idx = Isa.get_reg_idx r in
    let new_reg_list = List.mapi (fun idx reg_type -> if idx = reg_idx then t else reg_type) reg_list in
    {curr_state with reg_type = new_reg_list}

  let get_mem_op_type (curr_state: state_type)
      (disp: Isa.immediate option) (base: Isa.register option)
      (index: Isa.register option) (scale: Isa.scale option) : TypeFullExp.t =
    let disp_type, _ = match disp with
    | Some d -> get_imm_type d
    | None -> get_imm_type (ImmNum 0L)
    in
    let base_type, base_cond = match base with
    | Some b -> get_reg_type curr_state b
    | None -> get_imm_type (ImmNum 0L)
    in
    let index_type, index_cond = match index with
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
    let new_cond = TypeFullExp.CondVarSet.union base_cond index_cond in
    (new_type, new_cond)

  let get_mem_idx (addr_type: TypeExp.t) : int64 option =
    match addr_type with
    | TypeSingle s -> 
      begin match s with
      | SingleVar v -> if v = stack_base_id then Some 0L else None
      | SingleBExp (op, v1, v2) -> 
        begin match op, v1, v2 with
        | SingleAdd, SingleVar v, SingleConst c
        | SingleAdd, SingleConst c, SingleVar v -> if v = stack_base_id then Some c else None
        | SingleSub, SingleVar v, SingleConst c -> if v = stack_base_id then Some (Int64.neg c) else None (* NOTE: this maybe not needed*)
        | _ -> None
        end
      | _ -> None
      end
    | _ -> None

  let get_mem_idx_type (curr_state: state_type) (offset: int64) : TypeFullExp.t =
    let mem_type = curr_state.mem_type in
    let idx_find = List.find_opt (fun (off, _) -> off = offset) mem_type in
    match idx_find with
    | Some (_, t) -> t
    | None -> (TypeTop, TypeFullExp.CondVarSet.empty)
      (* TODO: Add this assert back later!!! *)
      (* code_type_error ("get_mem_idx_type: stack offset " ^ (string_of_int offset) ^ " not in record") *)

  let set_mem_idx_type (curr_state: state_type) (offset: int64) (new_type: TypeFullExp.t) : state_type =
    let mem_type = curr_state.mem_type in
    let idx_find = List.find_opt (fun (off, _) -> off = offset) mem_type in
    match idx_find with
    | Some _ -> 
      {curr_state with mem_type = 
        List.map (fun (off, x) -> if off = offset then (off, new_type) else (off, x)) mem_type}
    | None -> {curr_state with mem_type = (offset, new_type) :: mem_type}

  let get_ld_op_type (curr_state: state_type)
      (disp: Isa.immediate option) (base: Isa.register option)
      (index: Isa.register option) (scale: Isa.scale option)
      (size: int64) : TypeFullExp.t =
    let addr_type, _ = get_mem_op_type curr_state disp base index scale in
    let mem_idx = get_mem_idx addr_type in
    match mem_idx with
    | Some v -> get_mem_idx_type curr_state v
    | None -> 
      (TypeExp.TypePtr (addr_type, size), TypeFullExp.CondVarSet.empty) (* NOTE: Cond here is not correct *)
      (* (TypeTop, TypeFullExp.CondVarSet.empty) *)

  let set_st_op_type(curr_state: state_type) 
      (disp: Isa.immediate option) (base: Isa.register option)
      (index: Isa.register option) (scale: Isa.scale option)
      (new_type: TypeFullExp.t) : state_type =
    let addr_type, _ = get_mem_op_type curr_state disp base index scale in
    let mem_idx = get_mem_idx addr_type in
    match mem_idx with
    | Some v -> set_mem_idx_type curr_state v new_type (* TODO!!!*)
    | None -> curr_state

  let get_src_op_type (curr_state: state_type) (src: Isa.operand) : TypeFullExp.t =
    match src with
    | ImmOp imm -> get_imm_type imm
    | RegOp r -> get_reg_type curr_state r
    | MemOp (disp, base, index, scale) -> get_mem_op_type curr_state disp base index scale
    | LdOp (disp, base, index, scale, size) -> get_ld_op_type curr_state disp base index scale size
    | StOp _ -> code_type_error ("get_src_op_type: cannot get src op type of a st op")
    | LabelOp _ -> code_type_error ("get_src_op_type: cannot get src op type of a label op")

  let set_dest_op_type (curr_state: state_type) (dest: Isa.operand) (new_type: TypeFullExp.t) : state_type =
    match dest with
    | RegOp r -> set_reg_type curr_state r new_type
    | StOp (disp, base, index, scale, _) -> set_st_op_type curr_state disp base index scale new_type
    | _ -> code_type_error ("set_dest_op_type: dest is not reg or st op")

  let add_type_cond (tf: TypeFullExp.t) (cond_idx: int) : TypeFullExp.t =
    let t, cond = tf in (t, TypeFullExp.CondVarSet.add cond_idx cond)
  
  let add_type_cond_set (tf: TypeFullExp.t) (cond_set: TypeFullExp.CondVarSet.t) : TypeFullExp.t =
    let t, cond = tf in (t, TypeFullExp.CondVarSet.union cond_set cond)

  let add_reg_type_cond (reg_list: reg_type) (cond_idx: int) : reg_type =
    List.map (fun reg_type -> add_type_cond reg_type cond_idx) reg_list

  let add_mem_type_cond (mem_list: mem_type) (cond_idx: int) : mem_type =
    List.map (fun (off, tf) -> (off, add_type_cond tf cond_idx)) mem_list
    
  let add_state_type_cond (curr_state: state_type) (cond_idx: int) : state_type =
    {
      reg_type = add_reg_type_cond curr_state.reg_type cond_idx;
      mem_type = add_mem_type_cond curr_state.mem_type cond_idx;
      cond_type = curr_state.cond_type
    }

  let type_prop_inst (curr_state: state_type) (cond_list: CondType.t list) (inst: Isa.instruction) : state_type * (CondType.t list) =
    match inst with
    | Mov (dest, src)
    | MovS (dest, src)
    | MovZ (dest, src)
    | Lea (dest, src) ->
      let src_type = get_src_op_type curr_state src in
      (set_dest_op_type curr_state dest src_type, cond_list)
    | Add (dest, src1, src2) ->
      let src1_type, src1_cond = get_src_op_type curr_state src1 in
      let src2_type, src2_cond = get_src_op_type curr_state src2 in
      let dest_type = (TypeExp.eval (TypeBExp (TypeAdd, src1_type, src2_type)), TypeFullExp.CondVarSet.union src1_cond src2_cond) in
      (set_dest_op_type curr_state dest dest_type, cond_list)
    | Sub (dest, src1, src2) ->
      let src1_type, src1_cond = get_src_op_type curr_state src1 in
      let src2_type, src2_cond = get_src_op_type curr_state src2 in
      let dest_type = (TypeExp.eval (TypeBExp (TypeSub, src1_type, src2_type)), TypeFullExp.CondVarSet.union src1_cond src2_cond) in
      (set_dest_op_type curr_state dest dest_type, cond_list)
    | Sal (dest, src1, src2) ->
      let src1_type, src1_cond = get_src_op_type curr_state src1 in
      let src2_type, src2_cond = get_src_op_type curr_state src2 in
      let dest_type = (TypeExp.eval (TypeBExp (TypeSal, src1_type, src2_type)), TypeFullExp.CondVarSet.union src1_cond src2_cond) in
      (set_dest_op_type curr_state dest dest_type, cond_list)
    | Shr (dest, src1, src2) ->
      let src1_type, src1_cond = get_src_op_type curr_state src1 in
      let src2_type, src2_cond = get_src_op_type curr_state src2 in
      let dest_type = (TypeExp.eval (TypeBExp (TypeShr, src1_type, src2_type)), TypeFullExp.CondVarSet.union src1_cond src2_cond) in
      (set_dest_op_type curr_state dest dest_type, cond_list)
    | Sar (dest, src1, src2) ->
      let src1_type, src1_cond = get_src_op_type curr_state src1 in
      let src2_type, src2_cond = get_src_op_type curr_state src2 in
      let dest_type = (TypeExp.eval (TypeBExp (TypeSar, src1_type, src2_type)), TypeFullExp.CondVarSet.union src1_cond src2_cond) in
      (set_dest_op_type curr_state dest dest_type, cond_list)
    | Xor (dest, src1, src2) ->
      (* TODO: Add special case for xor %rax %rax *)
      begin match src1, src2 with
      | RegOp r1, RegOp r2 ->
        if r1 = r2 then (set_dest_op_type curr_state dest (TypeSingle (SingleConst 0L), TypeFullExp.CondVarSet.empty), cond_list)
        else begin
          let src1_type, src1_cond = get_src_op_type curr_state src1 in
          let src2_type, src2_cond = get_src_op_type curr_state src2 in
          let dest_type = (TypeExp.eval (TypeBExp (TypeXor, src1_type, src2_type)), TypeFullExp.CondVarSet.union src1_cond src2_cond) in
          (set_dest_op_type curr_state dest dest_type, cond_list)
        end
      | _ ->
        let src1_type, src1_cond = get_src_op_type curr_state src1 in
        let src2_type, src2_cond = get_src_op_type curr_state src2 in
        let dest_type = (TypeExp.eval (TypeBExp (TypeXor, src1_type, src2_type)), TypeFullExp.CondVarSet.union src1_cond src2_cond) in
        (set_dest_op_type curr_state dest dest_type, cond_list)
      end
    | Not (dest, src) ->
      let src_type, src_cond = get_src_op_type curr_state src in
      let dest_type = (TypeExp.eval (TypeUExp (TypeNot, src_type)), src_cond) in
      (set_dest_op_type curr_state dest dest_type, cond_list)
    | And (dest, src1, src2) ->
      let src1_type, src1_cond = get_src_op_type curr_state src1 in
      let src2_type, src2_cond = get_src_op_type curr_state src2 in
      let dest_type = (TypeExp.eval (TypeBExp (TypeAnd, src1_type, src2_type)), TypeFullExp.CondVarSet.union src1_cond src2_cond) in
      (set_dest_op_type curr_state dest dest_type, cond_list)
    | Or (dest, src1, src2) ->
      let src1_type, src1_cond = get_src_op_type curr_state src1 in
      let src2_type, src2_cond = get_src_op_type curr_state src2 in
      let dest_type = (TypeExp.eval (TypeBExp (TypeOr, src1_type, src2_type)), TypeFullExp.CondVarSet.union src1_cond src2_cond) in
      (set_dest_op_type curr_state dest dest_type, cond_list)
    | Cmp (src1, src2) ->
      let src1_full_type = get_src_op_type curr_state src1 in
      let src2_full_type = get_src_op_type curr_state src2 in
      ({curr_state with cond_type = (src1_full_type, src2_full_type)}, cond_list)
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
      (add_state_type_cond curr_state new_cond_idx, new_cond_list)
    | Jmp _ -> (curr_state, cond_list)
    | Push _ ->
      let src1_type, src1_cond = get_src_op_type curr_state (Isa.RegOp Isa.RSP) in
      let src2_type = TypeExp.TypeSingle (SingleConst (-8L)) in
      (* let src2_type, src2_cond = get_src_op_type curr_state src2 in *)
      let dest_type = (TypeExp.eval (TypeBExp (TypeAdd, src1_type, src2_type)), src1_cond) in
      (set_dest_op_type curr_state (Isa.RegOp Isa.RSP) dest_type, cond_list)
    | Pop _ ->
      let src1_type, src1_cond = get_src_op_type curr_state (Isa.RegOp Isa.RSP) in
      let src2_type = TypeExp.TypeSingle (SingleConst 8L) in
      (* let src2_type, src2_cond = get_src_op_type curr_state src2 in *)
      let dest_type = (TypeExp.eval (TypeBExp (TypeAdd, src1_type, src2_type)), src1_cond) in
      (set_dest_op_type curr_state (Isa.RegOp Isa.RSP) dest_type, cond_list)
    | _ -> begin
        print_endline ("[Warning] type_prop_inst: instruction not handled: " ^ (Isa.mnemonic_of_instruction inst));
        (curr_state, cond_list) (* TODO *)
    end

  type block_type = {
    label: Isa.label;
    block_code_type: state_type;
  }

  type t = block_type list

  let init_reg_type (start_var_idx: (TypeExp.type_var_id, Isa.imm_var_id) Either.t) (rsp_offset: int64) : ((TypeExp.type_var_id, Isa.imm_var_id) Either.t) * reg_type = 
    let rec helper (var_idx: (TypeExp.type_var_id, Isa.imm_var_id) Either.t) (r_type: reg_type) (idx: int) : ((TypeExp.type_var_id, Isa.imm_var_id) Either.t) * reg_type =
      if idx = Isa.total_reg_num
      then (var_idx, r_type)
      else begin
        if idx = Isa.rsp_idx then
          helper var_idx ((TypeSingle (SingleExp.eval (SingleBExp (SingleAdd, SingleVar stack_base_id, SingleConst rsp_offset))), TypeFullExp.CondVarSet.empty) :: r_type) (idx + 1)
          (* helper (type_var_idx, (TypeSingle (Option.get init_rsp), TypeFullExp.CondVarSet.empty) :: reg_type) (idx + 1) *)
        else begin
          match var_idx with
          | Left type_var_idx -> helper (Left (type_var_idx + 1)) ((TypeVar type_var_idx, TypeFullExp.CondVarSet.empty) :: r_type) (idx + 1)
          | Right imm_var_idx -> helper (Right (imm_var_idx + 1)) ((TypeSingle (SingleVar imm_var_idx), TypeFullExp.CondVarSet.empty) :: r_type) (idx + 1)
          (* helper (type_var_idx + 1, (TypeVar type_var_idx, TypeFullExp.CondVarSet.empty) :: reg_type) (idx + 1) *)
        end
      end
    in
    let var_idx, reg_type = helper start_var_idx [] 0 in
    (var_idx, List.rev reg_type)

  let init_mem_type (start_var_idx: (TypeExp.type_var_id, Isa.imm_var_id) Either.t) (mem_off_list: int64 list) : ((TypeExp.type_var_id, Isa.imm_var_id) Either.t) * mem_type =
    match start_var_idx with
    | Left start_type_var_idx -> 
      let end_id, end_type = List.fold_left_map (fun acc a -> (acc + 1, (a, (TypeExp.TypeVar acc, TypeFullExp.CondVarSet.empty)))) start_type_var_idx mem_off_list in
      (Left end_id, end_type)
    | Right start_imm_var_idx -> 
      let end_id, end_type = List.fold_left_map (fun acc a -> (acc + 1, (a, (TypeExp.TypeSingle (SingleVar acc), TypeFullExp.CondVarSet.empty)))) start_imm_var_idx mem_off_list in
      (Right end_id, end_type)
    (* List.fold_left_map (fun acc a -> (acc + 1, (a, (TypeVar acc, TypeFullExp.CondVarSet.empty)))) start_type_var_idx mem_off_list *)

  let init_code_type_var (start_type_var_idx: TypeExp.type_var_id) (prog: Isa.program) (mem_off_list: int64 list) : (TypeExp.type_var_id * Isa.imm_var_id) * t =
    let start_imm_var = stack_base_id + Isa.StrM.cardinal prog.imm_var_map + 1 in
    let helper_0 (acc: (TypeExp.type_var_id, Isa.imm_var_id) Either.t) (block: Isa.basic_block) : ((TypeExp.type_var_id, Isa.imm_var_id) Either.t) * block_type =
      let acc1, reg_t = init_reg_type acc block.rsp_offset in
      let acc2, mem_t = init_mem_type acc1 mem_off_list in
      (acc2, {
        label = block.label;
        block_code_type = {
          reg_type = reg_t;
          mem_type = mem_t;
          cond_type = ((TypeSingle (SingleConst 0L), TypeFullExp.CondVarSet.empty), (TypeSingle (SingleConst 0L), TypeFullExp.CondVarSet.empty));
        }
      }) in
    let helper (acc: TypeExp.type_var_id * Isa.imm_var_id) (block: Isa.basic_block) : (TypeExp.type_var_id * Isa.imm_var_id) * block_type = begin
      let type_acc, imm_acc = acc in
      if Isa.is_label_function_entry block.label then begin
        let new_acc, b_type = helper_0 (Right imm_acc) block in
        match new_acc with
        | Left _ -> code_type_error ("init_code_type_var: return idx should be imm_idx")
        | Right new_imm_acc -> ((type_acc, new_imm_acc), b_type) 
      end else begin
        let new_acc, b_type = helper_0 (Left type_acc) block in
        match new_acc with
        | Left new_type_acc -> ((new_type_acc, imm_acc), b_type)
        | Right _ ->  code_type_error ("init_code_type_var: return idx should be type_idx")
      end
      (* let acc1, reg_t = init_reg_type acc rsp_offset in
      let acc2, mem_t = init_mem_type acc1 mem_off_list in
      (acc2, {
        label = block.label;
        block_code_type = {
          reg_type = reg_t;
          mem_type = mem_t;
          cond_type = ((TypeSingle (SingleConst 0), TypeFullExp.CondVarSet.empty), (TypeSingle (SingleConst 0), TypeFullExp.CondVarSet.empty));
        }
      })  *)
    end in
    List.fold_left_map helper (start_type_var_idx, start_imm_var) prog.bbs
    
  let get_label_type (code_type: t) (label: Isa.label) : state_type =
    (List.find (fun x -> label = x.label) code_type).block_code_type

  let pp_state_type (lvl: int) (s: state_type) =
    PP.print_lvl lvl "Reg:\n";
    List.iteri (fun i x ->
      PP.print_lvl (lvl + 1) "<Reg %d> " i;
      TypeFullExp.pp_type_full_exp (lvl + 2) x;
      Printf.printf "\n"
    ) s.reg_type;
    PP.print_lvl lvl "Mem:\n";
    List.iter (fun (off, x) ->
      PP.print_lvl (lvl + 1) "<offset %Ld> " off;
      TypeFullExp.pp_type_full_exp (lvl + 2) x;
      Printf.printf "\n"
    ) s.mem_type;
    PP.print_lvl lvl "Cond:\n";
    TypeFullExp.pp_type_full_exp (lvl + 1) (fst s.cond_type);
    Printf.printf "\n";
    TypeFullExp.pp_type_full_exp (lvl + 1) (snd s.cond_type)

  let pp_block_types (lvl: int) (block_types: t) =
    List.iter (fun x ->
      PP.print_lvl lvl "<Block \"%s\">\n" x.label;
      pp_state_type (lvl + 1) x.block_code_type;
      Printf.printf "\n"
    ) block_types

  (* TODO: Initialize register and stack type based on program *)
  (* TODO: Maybe we can simply remove Lea!!! *)
  (* TODO: RSP type should always be represented as TypeSingle (stack_id + offset) to track stack spill. So we need to derive rsp first. *)
end
