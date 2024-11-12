open Isa
open Single_exp
open Single_entry_type
open Mem_offset_new
open Constraint
(* open Constraint *)
open Single_subtype
open Smt_emitter
open Pretty_print
open Full_mem_anno
open Sexplib.Std

module SingleTypeInfer = struct
  exception SingleTypeInferError of string

  module Isa = Isa (FullMemAnno)

  let single_type_infer_error msg = raise (SingleTypeInferError ("[Single Type Infer Error] " ^ msg))

  module ArchType = SingleSubtype.ArchType
  module FuncInterface = ArchType.FuncInterface

  type t = {
    func_name: Isa.label;
    func: Isa.basic_block list;
    func_type: ArchType.t list;
    single_subtype: SingleSubtype.t;
    next_var: SingleEntryType.t;
    input_var_set: SingleEntryType.SingleVarSet.t;
    smt_ctx: SmtEmitter.t;
  }
  [@@deriving sexp]

  let state_list_to_file (filename: string) (infer_result: t list) =
    let open Sexplib in
    let channel = open_out filename in
    Sexp.output_hum channel (sexp_of_list sexp_of_t infer_result)

  let state_list_from_file (filename: string) : t list =
    let open Sexplib in
    let channel = open_in filename in
    let s_exp = Sexp.input_sexp channel in
    list_of_sexp t_of_sexp s_exp

  let pp_func_type (lvl: int) (infer_state: t) =
    List.iter (fun x -> ArchType.pp_arch_type lvl x) infer_state.func_type

  let pp_ocaml_state (lvl: int) (buf: Buffer.t) (infer_state: t) =
    PP.bprint_lvl lvl buf "{\n";
    PP.bprint_lvl (lvl + 1) buf "func_name = \"%s\";\n" infer_state.func_name;
    PP.bprint_lvl (lvl + 1) buf "func =\n"; Isa.pp_ocaml_block_list (lvl + 2) buf infer_state.func; PP.bprint_lvl (lvl + 2) buf ";\n";
    PP.bprint_lvl (lvl + 1) buf "func_type =\n"; ArchType.pp_ocaml_arch_type_list (lvl + 2) buf infer_state.func_type; PP.bprint_lvl (lvl + 2) buf ";\n";
    PP.bprint_lvl (lvl + 1) buf "single_subtype =\n"; SingleSubtype.pp_ocaml_single_subtype (lvl + 2) buf infer_state.single_subtype; PP.bprint_lvl (lvl + 2) buf ";\n";
    PP.bprint_lvl (lvl + 1) buf "next_var = SingleTop;\n";
    PP.bprint_lvl (lvl + 1) buf "input_var_set = %s;\n" (SingleExp.var_set_to_ocaml_string infer_state.input_var_set);
    PP.bprint_lvl (lvl + 1) buf "smt_ctx = SmtEmitter.init_smt_ctx ();\n";
    PP.bprint_lvl lvl buf "}\n"

  let init
      (prog: Isa.prog)
      (func_name: string)
      (func_mem_interface: ArchType.MemType.t) : t =
    let global_var_list = List.map (fun (_, x) -> x) (Isa.StrM.to_list prog.imm_var_map) in
    let global_var_set = SingleExp.SingleVarSet.of_list global_var_list in
    let min_global_var =
      List.fold_left (
        fun acc x -> if x < acc then x else acc
      ) 0 global_var_list
    in
    let start_pc = 1 - min_global_var in
    (* I am still not sure whether to use offset or ptr+offset in MemType, but just add base here :) *)
    let func_mem_interface = ArchType.MemType.add_base_to_offset func_mem_interface in
    let max_var =
      ArchType.MemType.fold_left (
        fun acc (x: SingleEntryType.t) ->
          match x with
          | SingleVar v -> if v > acc then v else acc
          | SingleTop -> acc
          | _ -> single_type_infer_error "[init] func_mem_interface contains dep type exp"
      ) (Isa.total_reg_num) func_mem_interface
    in
    let start_var = SingleEntryType.SingleVar (max_var + 1) in
    let input_var_set = 
      SingleEntryType.SingleVarSet.of_list 
      (List.init (max_var - min_global_var + 1) (fun x -> x + min_global_var)) 
    in
    let func_body = (Isa.get_func prog func_name).body in
    let (_, next_var), arch_type_list =
      List.fold_left_map (
        fun (start_pc, start_var) (bb: Isa.basic_block) ->
          let next_pc = start_pc + List.length bb.insts + 1 in
          if bb.label = func_name then begin
            Printf.printf "BB %s pc = %d\n" bb.label start_pc;
            (
              (next_pc, start_var), 
              ArchType.init_func_input_from_layout 
                bb.label (SingleVar 0) start_pc func_mem_interface 
                global_var_set ArchType.TypeInferDep
            )
          end else begin
            Printf.printf "BB %s pc = %d\n" bb.label start_pc;
            let next_var, arch_type =
              ArchType.init_from_layout 
                bb.label start_var start_pc func_mem_interface 
                global_var_set ArchType.TypeInferDep
            in
            ((next_pc, next_var), arch_type)
          end
      ) (start_pc, start_var) func_body
    in
    (* ArchType.pp_arch_type_list 0 arch_type_list; *)
    {
      func_name = func_name;
      func = func_body;
      func_type = arch_type_list;
      single_subtype = [];
      next_var = next_var;
      input_var_set = input_var_set;
      smt_ctx = SmtEmitter.init_smt_ctx ();
    }

  let type_prop_all_blocks
    (func_interface_list: FuncInterface.t list)
    (* (infer_state: t) : t * (ArchType.block_subtype_t list) = *)
    (infer_state: t) (iter_left: int) : t * (ArchType.block_subtype_t list) =
    let ctx, solver = infer_state.smt_ctx in
    let helper 
        (acc: (ArchType.block_subtype_t list) * (Isa.instruction list list)) 
        (block: Isa.basic_block) (block_type: ArchType.t) : 
        (ArchType.block_subtype_t list) * (Isa.instruction list list) =
      Z3.Solver.push solver;
      SingleSubtype.update_block_smt_ctx (ctx, solver) infer_state.single_subtype block_type.useful_var;
      (* Printf.printf "Block %s solver \n%s\n" block.label (Z3.Solver.to_string solver); *)
      (* Printf.printf "type_prop_block %s\n" block.label; *)
      let _ = iter_left in
      let block_subtype, update_block_list = acc in
      let (_, block_subtype), block =
        (* if iter_left = 1 && block.label = ".L2" then begin
          Printf.printf "skip prop %s\n" block.label;
          block_type, block_subtype
        end else *)
        ArchType.type_prop_block (ctx, solver) 
          (SingleSubtype.sub_sol_single_to_range_opt infer_state.single_subtype infer_state.input_var_set) 
          func_interface_list block_type block.insts block_subtype
      in
      (* Printf.printf "After prop block %s\n" block.label; *)
      Z3.Solver.pop solver 1;
      block_subtype, block :: update_block_list
    in
    let block_subtype = ArchType.init_block_subtype_list_from_block_type_list infer_state.func_type in
    Printf.printf "func len %d, type len %d\n" (List.length infer_state.func) (List.length infer_state.func_type);
    let block_subtype, rev_new_block_list = List.fold_left2 helper (block_subtype, []) infer_state.func infer_state.func_type in
    Printf.printf "block_subtype len %d\n" (List.length block_subtype);
    let new_func = 
      List.map2 (
        fun (x: Isa.basic_block) (y: Isa.instruction list) -> { x with insts = y }
      ) infer_state.func (List.rev rev_new_block_list)
    in
    { infer_state with 
      func = new_func;
      func_type = ArchType.update_with_block_subtype block_subtype infer_state.func_type },
    block_subtype
    (* let single_subtype, block_subtype = SingleSubtype.init block_subtype in
    Printf.printf "2\n";
    let single_subtype = SingleSubtype.solve_vars single_subtype block_subtype infer_state.input_var_set solver_iter in
    { infer_state with 
      func_type = ArchType.update_with_block_subtype block_subtype infer_state.func_type; 
      single_subtype = single_subtype 
    } *)

  let update_mem (infer_state: t) : t =
    let update_list = ArchType.MemType.init_stack_update_list (List.nth infer_state.func_type 0).mem_type in
    let helper (acc: (MemOffset.t * bool) list) (a_type: ArchType.t) : (MemOffset.t * bool) list =
      let unknown_list = Constraint.get_unknown a_type.constraint_list in
      (* MemOffset.pp_unknown_list 0 unknown_list; *)
      let unknown_range = 
        List.map (
          fun ((l, r), pc) -> 
            SingleSubtype.sub_sol_single_to_range infer_state.single_subtype infer_state.input_var_set (l, pc),
            SingleSubtype.sub_sol_single_to_range infer_state.single_subtype infer_state.input_var_set (r, pc)
        ) unknown_list
      in
      let new_offset_list = List.filter_map MemOffset.from_range unknown_range in
      (* Printf.printf "update_mem\n";
      MemOffset.pp_off_list 0 new_offset_list;  *)
      MemOffset.insert_new_offset_list infer_state.smt_ctx acc new_offset_list
    in
    let update_list = List.fold_left helper update_list infer_state.func_type in
    let next_var, func_type =
      List.fold_left_map (
        fun (acc: SingleEntryType.t) (a_type: ArchType.t) ->
          let acc, new_mem = ArchType.MemType.update_mem acc a_type.mem_type update_list in
          acc, { a_type with mem_type = new_mem }
      ) infer_state.next_var infer_state.func_type
    in
    let local_var_set = 
      List.fold_left (
        fun acc entry -> 
          SingleExp.SingleVarSet.union acc (ArchType.get_local_var_set entry)
      ) SingleExp.SingleVarSet.empty func_type
    in
    let single_subtype = SingleSubtype.filter_entry infer_state.single_subtype local_var_set in
    { infer_state with
      func_type = func_type;
      single_subtype = single_subtype;
      next_var = next_var
    }
    (* TODO: Also remove removed vars in single_subtype *)

  let clean_up_func_type (infer_state: t) : t =
    { infer_state with
      func_type = List.map ArchType.clean_up infer_state.func_type
    }

  let infer_one_func
      (prog: Isa.prog)
      (func_interface_list: FuncInterface.t list)
      (func_name: Isa.label)
      (func_mem_interface: ArchType.MemType.t)
      (iter: int)
      (solver_iter: int) : t =
    let init_infer_state = init prog func_name func_mem_interface in
    let rec helper (state: t) (iter_left: int) : t =
      if iter_left = 0 then
        state
      else begin
        let curr_iter = iter - iter_left + 1 in
        (* Prepare SMT context *)
        let solver = snd state.smt_ctx in
        Z3.Solver.push solver;
        ArchType.MemType.gen_implicit_mem_constraints state.smt_ctx (List.hd state.func_type).mem_type;
        (* gen_implicit_mem_constraints state; *)
        (* 1. Prop *)
        Printf.printf "\n\nInfer iter %d type_prop_all_blocks\n\n" curr_iter;
        let state, block_subtype = type_prop_all_blocks func_interface_list state iter_left in
        (* 2. Insert stack addr in unknown list to mem type *)
        let unknown_resolved = 
          List.fold_left 
            (fun acc (x: ArchType.t) -> acc + List.length (Constraint.get_unknown x.constraint_list)) 
            0 state.func_type = 0
        in
        Printf.printf "After infer, unknown list:\n";
        List.iter (
          fun (x: ArchType.t) -> 
            Printf.printf "%s\n" x.label;
            MemOffset.pp_unknown_list 0 (Constraint.get_unknown x.constraint_list)
        ) state.func_type;
        Printf.printf "\n\n%s: Infer iter %d update_mem\n\n" func_name curr_iter;
        let state = update_mem state in
        Printf.printf "\n\nInfer iter %d after update_mem\n\n" curr_iter;
        pp_func_type 0 state;
        (* 3. Single type infer *)
        let single_subtype, block_subtype = SingleSubtype.init func_name block_subtype in
        let single_subtype = SingleSubtype.solve_vars single_subtype block_subtype state.input_var_set solver_iter in
        let state = { state with single_subtype = single_subtype } in

        Z3.Solver.pop solver 1;

        Printf.printf "After infer, single subtype\n";
        SingleSubtype.pp_single_subtype 0 state.single_subtype;
        if unknown_resolved then begin
          (* Directly return if unknown are all resolved. *)
          Printf.printf "\n\nSuccessfully resolved all memory accesses for %s at iter %d\n\n" func_name curr_iter;
          state
        end else begin
          helper (clean_up_func_type state) (iter_left - 1)
        end
      end
    in
    helper init_infer_state iter

  let get_func_interface
      (infer_state: t) : FuncInterface.t =
    let sub_sol = 
      SingleSubtype.sub_sol_single_to_single_func_interface 
        infer_state.single_subtype infer_state.input_var_set 
    in
    ArchType.get_func_interface
      infer_state.smt_ctx
      infer_state.func_name
      infer_state.func_type
      sub_sol

  (* let get_func_interface
      (func_name: Isa.label)
      (infer_state: t) : FuncInterface.t =
    Z3.Solver.push (snd infer_state.smt_ctx);
    ArchType.MemType.gen_implicit_mem_constraints infer_state.smt_ctx (List.hd infer_state.func_type).mem_type;
    let in_state = List.find (fun (x: ArchType.t) -> x.label = func_name) infer_state.func_type in
    let out_state = List.find (fun (x: ArchType.t) -> x.label = Isa.ret_label) infer_state.func_type in
    let helper (pc: int) (e: SingleEntryType.t) : SingleEntryType.t =
      let r = 
        SingleSubtype.sub_sol_single_to_range 
          infer_state.single_subtype infer_state.input_var_set (e, pc) 
      in
      match r with
      | Single exp -> exp
      | _ -> SingleTop
    in
    let res: FuncInterface.t = {
      func_name = func_name;
      in_reg = in_state.reg_type;
      in_mem = ArchType.MemType.remove_local_mem_quick_cmp infer_state.smt_ctx in_state.mem_type;
      context = [];
      out_reg = List.map (helper out_state.pc) out_state.reg_type;
      out_mem = ArchType.MemType.map (helper out_state.pc) (ArchType.MemType.remove_local_mem_quick_cmp infer_state.smt_ctx out_state.mem_type)
    }
    in
    Z3.Solver.pop (snd infer_state.smt_ctx) 1;
    res *)

  let pp_ocaml_infer_result (lvl: int) (buf: Buffer.t) (func_type_list: t list) =
    PP.bprint_lvl lvl buf "[\n";
    List.iter (
      fun x -> 
        pp_ocaml_state (lvl + 1) buf x;
        PP.bprint_lvl (lvl + 1) buf ";\n"
    ) func_type_list;
    PP.bprint_lvl lvl buf "]\n"

  let infer
      (prog_name: Isa.label)
      (prog: Isa.prog)
      (func_mem_interface_list: (Isa.label * ArchType.MemType.t) list)
      (iter: int)
      (solver_iter: int) : t list =
    (* TODO: The correct order is for each function, infer its single type, then taint type, then next function.
      So this function should be moved to a upper-level model that infer all types. *)
    let helper 
        (acc: FuncInterface.t list) (entry: Isa.label * ArchType.MemType.t) :
        (FuncInterface.t list) * t =
      let func_name, func_mem_interface = entry in
      let infer_state = infer_one_func prog acc func_name func_mem_interface iter solver_iter in
      let func_interface = get_func_interface infer_state in
      Printf.printf "Infer state of func %s\n" func_name;
      pp_func_type 0 infer_state;
      FuncInterface.pp_func_interface 0 func_interface;
      (* let buf = Buffer.create 1000 in
      pp_ocaml_state 0 buf infer_state;
      Printf.printf "%s" (String.of_bytes (Buffer.to_bytes buf)); *)
      func_interface :: acc, infer_state
    in
    let _, infer_result = List.fold_left_map helper [] func_mem_interface_list in
    let buf = Buffer.create 1000 in
    pp_ocaml_infer_result 0 buf infer_result;
    Printf.printf "let %s_single_infer_state : SingleTypeInfer.t list =\n" prog_name;
    Printf.printf "%s" (String.of_bytes (Buffer.to_bytes buf));
    infer_result

end
