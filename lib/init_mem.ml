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

  let get_base_set (e: SingleExp.t list) : SingleExp.SingleVarSet =
    let helper (acc: SingleExp.SingleVarSet) (e: SingleExp.t list) : SingleExp.SingleVarSet =
      match e with
      | [ SingleVar v ] -> SingleExp.SingleVarSet.add v acc
      | _ -> acc
    in
    List.fold_left helper SingleExp.SingleVarSet.empty e

end