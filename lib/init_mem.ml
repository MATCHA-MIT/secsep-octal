open Isa
open Single_exp
open Type_exp
open Type_full_exp
open Cond_type
open Code_type
open Pretty_print

module InitMem = struct
  exception InitMemError of string

  let init_mem_error msg = raise (InitMemError ("[Init Mem Error] " ^ msg))

  let init_addr_exp (p: Isa.program) (init_code_type: CodeType.t) : (TypeFullExp.t * int) list =
    let helper_0 
        (acc: (TypeFullExp.t * int) list * CodeType.state_type * CondType.t list) 
        (inst: Isa.instruction) : 
        (TypeFullExp.t * int) list * CodeType.state_type * CondType.t list =
      let addr_list, state, cond_list = acc in
      let get_addr_type (op: Isa.operand) : (TypeFullExp.t * int) option =
        match op with
        | LdOp (disp, base, index, scale, size) 
        | StOp (disp, base, index, scale, size) -> Some (CodeType.get_mem_op_type state disp base index scale, size)
        | _ -> None
      in
      let new_addr_list = List.filter_map get_addr_type (Isa.get_op_list inst) in
      let new_state, new_cond_list = CodeType.type_prop_inst state cond_list inst in
      (new_addr_list @ addr_list, new_state, new_cond_list)
    in
    let helper 
        (acc: (TypeFullExp.t * int) list * CondType.t list)
        (block_type: CodeType.block_type)
        (block: Isa.basic_block) : 
        (TypeFullExp.t * int) list * CondType.t list =
      if block_type.label = block.label then
        let addr_list, cond_list = acc in
        let new_addr_list, _, new_cond_list = List.fold_left helper_0 (addr_list, block_type.block_code_type, cond_list) block.insts in
        (new_addr_list, new_cond_list)
      else init_mem_error ("init_addr_exp helper block type and block label mismatch: " ^ block_type.label ^ " != " ^ block.label)
    in
    let new_addr_list, _ = List.fold_left2 helper ([], []) init_code_type p.bbs in new_addr_list

  let repl_addr_exp (addr_exp_list: (TypeFullExp.t * int) list) (subtype_sol: (TypeExp.type_var_id * TypeFullExp.type_sol) list) : (TypeFullExp.t * int) list =
    let helper (x: TypeFullExp.t * int) : TypeFullExp.t * int =
      let e, size = x in 
      let new_e = TypeFullExp.repl_all_sol subtype_sol e in (new_e, size)
    in
    List.map helper addr_exp_list

  let get_addr_range (addr_exp_list: (TypeFullExp.t * int) list) : (SingleExp.t * SingleExp.t) list =
    let helper (addr_exp: TypeFullExp.t * int) : SingleExp.t * SingleExp.t = 
      let (addr, _), size = addr_exp in
      match addr with
      | TypeSingle s -> (s, SingleExp.SingleConst size)
      | TypeRange (s1, true, s2, true, _) ->
        (s1, SingleExp.eval (SingleBExp (SingleExp.SingleAdd, SingleBExp (SingleExp.SingleSub, s2, s1), SingleExp.SingleConst size)))
      | _ -> init_mem_error "get_addr_range not implement this case"
    in
    List.map helper addr_exp_list

  let pp_addr_exp (lvl: int) (addr_exp: (TypeFullExp.t * int) list) =
    PP.print_lvl lvl "Addr exp list:\n";
    List.iteri (
      fun i x ->
        let exp, size = x in
        PP.print_lvl (lvl + 1) "<Addr %d>" i;
        TypeFullExp.pp_type_full_exp (lvl + 2) exp;
        PP.print_lvl (lvl + 1) "%d" size;
        Printf.printf "\n"
    ) addr_exp

  let pp_addr_range (lvl: int) (addr_range: (SingleExp.t * SingleExp.t) list) =
    PP.print_lvl lvl "Addr range list:\n";
    List.iteri (
      fun i x ->
        let base, size = x in
        PP.print_lvl (lvl + 1) "<Addr Range %d>" i;
        SingleExp.pp_single_exp (lvl + 2) base;
        SingleExp.pp_single_exp (lvl + 2) size;
        Printf.printf "\n"
    ) addr_range

  let rec filter_single_var (e: SingleExp.t) : SingleExp.SingleVarSet.t * SingleExp.SingleVarSet.t =
    match e with
    | SingleVar x -> (SingleExp.SingleVarSet.singleton x, SingleExp.SingleVarSet.empty)
    | SingleBExp (SingleExp.SingleAdd, l, r) ->
      let left_ptr, left_no_ptr = filter_single_var l in
      let right_ptr, right_no_ptr = filter_single_var r in
      (SingleExp.SingleVarSet.union left_ptr right_ptr, SingleExp.SingleVarSet.union left_no_ptr right_no_ptr)
    | SingleBExp (SingleExp.SingleMul, l, r) ->
      let left_ptr, left_no_ptr = filter_single_var l in
      let right_ptr, right_no_ptr = filter_single_var r in
      (SingleExp.SingleVarSet.empty, SingleExp.SingleVarSet.union (SingleExp.SingleVarSet.union left_ptr right_ptr) (SingleExp.SingleVarSet.union left_no_ptr right_no_ptr))
    | _ -> (SingleExp.SingleVarSet.empty, SingleExp.SingleVarSet.empty)

  let filter_type_single_var (e: TypeExp.t) : SingleExp.SingleVarSet.t * SingleExp.SingleVarSet.t =
    match e with
    | TypeSingle s -> filter_single_var s
    | TypeRange (l, _, r, _, _) ->
      let left_ptr, left_no_ptr = filter_single_var l in
      let right_ptr, right_no_ptr = filter_single_var r in
      let diff1 = SingleExp.SingleVarSet.diff left_ptr right_ptr in
      let diff2 = SingleExp.SingleVarSet.diff right_ptr left_ptr in
      (SingleExp.SingleVarSet.inter left_ptr right_ptr,
      SingleExp.SingleVarSet.union (SingleExp.SingleVarSet.union diff1 diff2) (SingleExp.SingleVarSet.union left_no_ptr right_no_ptr))
    | _ -> (SingleExp.SingleVarSet.empty, SingleExp.SingleVarSet.empty)

  let try_solve_base 
      (acc: SingleExp.SingleVarSet.t * SingleExp.SingleVarSet.t)
      (base_list: (SingleExp.SingleVarSet.t, Isa.imm_var_id) Either.t) : 
      (SingleExp.SingleVarSet.t * SingleExp.SingleVarSet.t) * ((SingleExp.SingleVarSet.t, Isa.imm_var_id) Either.t) =
    let ptr_list, no_ptr_list = acc in
    match base_list with
    | Left base_list ->
      let base_list = SingleExp.SingleVarSet.diff base_list no_ptr_list in
      let inter_list = SingleExp.SingleVarSet.inter base_list ptr_list in
      let diff_list = SingleExp.SingleVarSet.diff base_list ptr_list in
      begin match SingleExp.SingleVarSet.elements inter_list, SingleExp.SingleVarSet.elements diff_list with
      | [], [] -> init_mem_error "try_solve_base get empty base candidate"
      | [], hd :: [] -> ((SingleExp.SingleVarSet.add hd ptr_list, no_ptr_list), Right hd)
      | [], _ :: _ -> (acc, Left diff_list)
      | hd :: [], _ -> ((ptr_list, SingleExp.SingleVarSet.union diff_list no_ptr_list), Right hd)
      | _ :: _, _ -> init_mem_error "try_solve_base add more than one ptrs"
      end
      (* begin match SingleExp.SingleVarSet.elements base_list with
      | [] -> init_mem_error "try_solve_base get empty base candidate"
      | hd :: [] -> Right ((SingleExp.SingleVarSet.add hd ptr_list, no_ptr_list), Right hd)
      end *)
    | Right _ -> (acc, base_list)
  
  let rec solve_base 
      (acc: SingleExp.SingleVarSet.t * SingleExp.SingleVarSet.t)
      (base_list: ((SingleExp.SingleVarSet.t, Isa.imm_var_id) Either.t) list)
      (iter: int) : 
      Isa.imm_var_id list =
    let to_id (x: (SingleExp.SingleVarSet.t, Isa.imm_var_id) Either.t) : Isa.imm_var_id =
      match x with
      | Left _ -> init_mem_error "solve_base cannot find solution"
      | Right id -> id
    in
    if iter = 0 then List.map to_id base_list
    else
      let new_acc, new_list = List.fold_left_map try_solve_base acc base_list in
      solve_base new_acc new_list (iter - 1)

  let get_base (ptr_list: SingleExp.SingleVarSet.t) (addr_list: (TypeFullExp.t * int) list) : Isa.imm_var_id list =
    let ptr_set_list, no_ptr_set_list = List.split (List.map (fun ((e, _), _) -> filter_type_single_var e) addr_list) in
    let no_ptr_set = List.fold_left (fun acc x -> SingleExp.SingleVarSet.union acc x) SingleExp.SingleVarSet.empty no_ptr_set_list in
    let ptr_set_list = List.map (
      fun x : (SingleExp.SingleVarSet.t, Isa.imm_var_id) Either.t -> 
        Left (SingleExp.SingleVarSet.diff x no_ptr_set)) 
      ptr_set_list in
    solve_base (ptr_list, SingleExp.SingleVarSet.empty) ptr_set_list 1
    
  let pp_base (lvl: int) (base_list: Isa.imm_var_id list) =
    PP.print_lvl lvl "Base list:\n";
    List.iteri (
      fun i x ->
        PP.print_lvl (lvl + 1) "<Addr Range %d> %d\n" i x
    ) base_list

  let get_addr_base_range 
      (ptr_list: SingleExp.SingleVarSet.t) 
      (addr_list: (TypeFullExp.t * int) list) : 
      (Isa.imm_var_id * SingleExp.t * SingleExp.t) list =
    let base_list = get_base ptr_list addr_list in
    let helper (base_id: Isa.imm_var_id) (mem_access: TypeFullExp.t * int) : Isa.imm_var_id * SingleExp.t * SingleExp.t =
      let (addr, _), size = mem_access in
      match addr with
      | TypeSingle x -> (base_id, 
          SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleSub, x, SingleExp.SingleVar base_id)),
          SingleExp.eval (SingleExp.SingleBExp (
            SingleExp.SingleAdd, 
            SingleExp.SingleBExp (SingleExp.SingleSub, x, SingleExp.SingleVar base_id), 
            SingleExp.SingleConst size)))
      | TypeRange (l, _, r, _, _) -> (base_id,
          SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleSub, l, SingleExp.SingleVar base_id)),
          SingleExp.eval (SingleExp.SingleBExp (
            SingleExp.SingleAdd,
            SingleExp.SingleBExp (SingleExp.SingleSub, r, SingleExp.SingleVar base_id),
            SingleExp.SingleConst size)))
      | _ -> init_mem_error "get_addr_base_range cannot handle this case"
    in
    List.map2 helper base_list addr_list

  let pp_base_range (lvl: int) (base_range_list: (Isa.imm_var_id * SingleExp.t * SingleExp.t) list) =
    PP.print_lvl lvl "Base range list:\n";
    List.iteri (
      fun i x ->
        let base_id, left, right = x in
        PP.print_lvl (lvl + 1) "<Addr Range %d> %d" i base_id;
        SingleExp.pp_single_exp (lvl + 2) left;
        SingleExp.pp_single_exp (lvl + 2) right;
        Printf.printf "\n"
    ) base_range_list

end