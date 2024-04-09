(* Type generation *)

open Isa
open Code_type
open Subtype

module GenType = struct
  exception GenTypeError of string

  let gen_type_error msg = raise (GenTypeError ("[Gen Type Error] " ^ msg))


  let gen_block_subtype_rel 
      (tv_rel: SubType.t) 
      (block_type: CodeType.state_type) 
      (cond_list: CodeType.cond_type list) 
      (inst_list: Isa.instruction list) 
      (code_type: CodeType.t) : SubType.t * (CodeType.cond_type list) =
    let helper (acc: SubType.t * CodeType.state_type * (CodeType.cond_type list)) (inst: Isa.instruction) : SubType.t * CodeType.state_type * (CodeType.cond_type list) =
      let acc_tv_rel, acc_state_type, acc_cond_list = acc in
      let new_state_type, new_cond_list = CodeType.type_prop_inst acc_state_type acc_cond_list inst in
      match inst with
      | Jcond (target_label, _) ->
        let taken_state_type = CodeType.add_state_type_cond acc_state_type ((List.length new_cond_list) * 2 + 1) in
        let target_state_type = CodeType.get_label_type code_type target_label in
        let new_tv_rel = SubType.add_sub_state_type acc_tv_rel taken_state_type target_state_type in
        (new_tv_rel, new_state_type, new_cond_list)
      | Jmp target_label ->
        let target_state_type = CodeType.get_label_type code_type target_label in
        let new_tv_rel = SubType.add_sub_state_type acc_tv_rel new_state_type target_state_type in
        (new_tv_rel, new_state_type, new_cond_list)
      | _ -> (acc_tv_rel, new_state_type, new_cond_list) in
    let new_tv_rel, _, new_cond_list = List.fold_left helper (tv_rel, block_type, cond_list) inst_list in
    (new_tv_rel, new_cond_list)
  (* NOTE: For convenience, we should add uncond jmp at the end of each block (except those end with ret)!!!*)

  (* TODO: Condsider to rename program to code or CodeType, code_type, etc. to program type *)
  let gen_subtype_rel (p: Isa.program) (init_code_type: CodeType.t) (total_type_var: int) : SubType.t * (CodeType.cond_type list) =
    let helper (acc: SubType.t * (CodeType.cond_type list)) (block_type: CodeType.block_type) (block_inst: Isa.basic_block) : SubType.t * (CodeType.cond_type list) =
      if block_type.label = block_inst.label then
        let acc_tv_rel, acc_cond_list = acc in
        gen_block_subtype_rel acc_tv_rel block_type.block_code_type acc_cond_list block_inst.insts init_code_type
      else gen_type_error ("gen_subtype_rel helper: block type and block label mismatch: " ^ block_type.label ^ " != " ^ block_inst.label) in
    let init_tv_rel = SubType.init total_type_var in
    List.fold_left2 helper (init_tv_rel, []) init_code_type p

  let gen_init_type_subtype_rel (p: Isa.program) : SubType.t * CodeType.t * (CodeType.cond_type list) =
    let total_type_var, init_code_type = CodeType.init_code_type_var 0 p [] in (* TODO: Add functionality to init mem_off_list!!! *) 
    let tv_rel, cond_list = gen_subtype_rel p init_code_type total_type_var in
    (tv_rel, init_code_type, cond_list)

end
