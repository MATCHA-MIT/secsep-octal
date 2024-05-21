open Isa
open Code_type
open Pretty_print

module InitMem = struct
  exception InitMemError of string

  let init_mem_error msg = raise (InitMemError ("[Init Mem Error] " ^ msg))

  let init_addr_exp (p: Isa.program) (init_code_type: CodeType.t) : (CodeType.type_full_exp * int) list =
    let helper_0 
        (acc: (CodeType.type_full_exp * int) list * CodeType.state_type * CodeType.cond_type list) 
        (inst: Isa.instruction) : 
        (CodeType.type_full_exp * int) list * CodeType.state_type * CodeType.cond_type list =
      let addr_list, state, cond_list = acc in
      let get_addr_type (op: Isa.operand) : (CodeType.type_full_exp * int) option =
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
        (acc: (CodeType.type_full_exp * int) list * CodeType.cond_type list)
        (block_type: CodeType.block_type)
        (block: Isa.basic_block) : 
        (CodeType.type_full_exp * int) list * CodeType.cond_type list =
      if block_type.label = block.label then
        let addr_list, cond_list = acc in
        let new_addr_list, _, new_cond_list = List.fold_left helper_0 (addr_list, block_type.block_code_type, cond_list) block.insts in
        (new_addr_list, new_cond_list)
      else init_mem_error ("init_addr_exp helper block type and block label mismatch: " ^ block_type.label ^ " != " ^ block.label)
    in
    let new_addr_list, _ = List.fold_left2 helper ([], []) init_code_type p.bbs in new_addr_list

  let repl_addr_exp (addr_exp_list: (CodeType.type_full_exp * int) list) (subtype_sol: (CodeType.type_var_id * CodeType.type_sol) list) : (CodeType.type_full_exp * int) list =
    let helper (x: CodeType.type_full_exp * int) : CodeType.type_full_exp * int =
      let e, size = x in 
      let new_e = CodeType.repl_all_sol_full_exp subtype_sol e in (new_e, size)
    in
    List.map helper addr_exp_list

  let get_addr_range (addr_exp_list: (CodeType.type_full_exp * int) list) : (CodeType.single_exp * CodeType.single_exp) list =
    let helper (addr_exp: CodeType.type_full_exp * int) : CodeType.single_exp * CodeType.single_exp = 
      let (addr, _), size = addr_exp in
      match addr with
      | TypeSingle s -> (s, CodeType.SingleConst size)
      | TypeRange (s1, true, CodeType.SingleBExp (CodeType.SingleAdd, s2, range_size), true, _) ->
        if CodeType.cmp_single_exp s1 s2 then (s1, CodeType.eval_single_exp (CodeType.SingleBExp (CodeType.SingleAdd, range_size, CodeType.SingleConst size)))
        else init_mem_error "get_addr_range not implement this case for range simplification"
      | _ -> init_mem_error "get_addr_range not implement this case"
    in
    List.map helper addr_exp_list

  let pp_addr_exp (lvl: int) (addr_exp: (CodeType.type_full_exp * int) list) =
    PP.print_lvl lvl "Addr exp list:\n";
    List.iteri (
      fun i x ->
        let exp, size = x in
        PP.print_lvl (lvl + 1) "<Addr %d>" i;
        CodeType.pp_type_full_exp (lvl + 2) exp;
        PP.print_lvl (lvl + 1) "%d" size;
        Printf.printf "\n"
    ) addr_exp

  let pp_addr_range (lvl: int) (addr_range: (CodeType.single_exp * CodeType.single_exp) list) =
    PP.print_lvl lvl "Addr range list:\n";
    List.iteri (
      fun i x ->
        let base, size = x in
        PP.print_lvl (lvl + 1) "<Addr Range %d>" i;
        CodeType.pp_single_exp (lvl + 2) base;
        CodeType.pp_single_exp (lvl + 2) size;
        Printf.printf "\n"
    ) addr_range
end