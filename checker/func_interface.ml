open Type.Isa_basic
open Type.Smt_emitter
open Mem_anno
open Reg_type
open Mem_type
open Arch_type_basic
open Call_anno

module FuncInterface = struct
  exception FuncInterfaceError of string

  let func_interface_error msg = raise (FuncInterfaceError ("[Func Interface Error] " ^ msg))

  type base_info =
    | BaseAsReg of IsaBasic.register
    | BaseAsSlot of MemAnno.slot_t
    | BaseAsGlobal
  [@@deriving sexp]

  type t = {
    func_name: IsaBasic.label;
    in_type: ArchTypeBasic.t;
    out_type: ArchTypeBasic.t;

    (* base_info: base_info MemType.mem_content; *)
    (* <TODO>: check func interface validity also needs to check base_info *)
  }
  [@@deriving sexp]

  let get_func_interface
      (fi_list: t list) (func_name: string) : t =
    let f_opt =
      List.find_opt (
        fun (x: t) -> x.func_name = func_name
      ) fi_list
    in
    match f_opt with
    | Some f -> f
    | None -> func_interface_error (Printf.sprintf "get_func_interface cannot find func %s" func_name)

  let interface_list_to_file (filename: string) (fi_list: t list) : unit =
    let open Sexplib in
    let channel = open_out filename in
    Sexp.output_hum channel (Std.sexp_of_list sexp_of_t fi_list)
  
  let interface_list_from_file (filename: string) : t list =
    let open Sexplib in
    let channel = open_in filename in
    Std.list_of_sexp t_of_sexp (Sexp.input_sexp channel)
  
  let add_in_mem_range_in_off_constr
      (smt_ctx: SmtEmitter.t)
      (fi: t) : unit =
    let in_mem = fi.in_type.mem_type in
    let constrs = MemType.fold_left_full (
      fun acc (slot: MemType.entry_t MemType.mem_slot) ->
        let (off_l, off_r), _, range, _ = slot in
        if MemRange.is_empty range then acc else
        let (range_l, range_r) = MemRange.get_boundary range in
        [
          Z3.BitVector.mk_sle (fst smt_ctx) off_l range_l;
          Z3.BitVector.mk_sle (fst smt_ctx) range_l range_r;
          Z3.BitVector.mk_sle (fst smt_ctx) range_r off_r;
        ] @ acc
    ) [] in_mem
    in
    SmtEmitter.add_assertions smt_ctx constrs
  
  let prop_check_call
      (smt_ctx: SmtEmitter.t)
      (pr_type: ArchTypeBasic.t)
      (func_interface: t)
      (call_anno: CallAnno.t) : ArchTypeBasic.t option =
    (* <NOTE> <TODO> Extra child var in out_type should also be in call_anno's ctx_map *)
    (* 1. Check input type *)
    let check_input = 
      ArchTypeBasic.check_subtype smt_ctx pr_type func_interface.in_type call_anno.ctx_map (Some call_anno.mem_map)
    in
    if not check_input then None else
    (* 2. Check pr_reg in call_anno *)
    let check_pr_reg =
      RegType.check_taint_eq smt_ctx pr_type.reg_type call_anno.pr_reg
    in
    if not check_pr_reg then None else
    (* 3. Update reg, flag, mem, context, and check update rights at the same time *)
    let out_reg_type, out_flag_type, out_mem_type, (out_dep_context, out_taint_context) =
      ArchTypeBasic.sub_ctx_map_helper smt_ctx func_interface.out_type call_anno.ctx_map
    in

    (* we should add context first before updating memory, which may need context for vars of out interface *)

    Printf.printf "adding out_ctx, original ctx is %s\n" (SmtEmitter.check_context smt_ctx |> SmtEmitter.sexp_of_sat_result_t |> Sexplib.Sexp.to_string_hum);
    (* List.iter (fun e ->
      let single_check = SmtEmitter.check_compliance smt_ctx [e] != SatNo in
      if not single_check then begin
        Printf.printf "single check failed: %s\nctx=%s\n" (Z3.Expr.to_string e) (SmtEmitter.to_string smt_ctx);
      end
    ) out_dep_context; *)

    SmtEmitter.add_assertions smt_ctx out_dep_context;
    Printf.printf "after adding out_ctx, ctx is %s\n" (SmtEmitter.check_context smt_ctx |> SmtEmitter.sexp_of_sat_result_t |> Sexplib.Sexp.to_string_hum);

    let out_mem_opt =
      MemType.set_mem_type_with_other smt_ctx
        pr_type.mem_type out_mem_type call_anno.mem_map
    in
    match out_mem_opt with
    | None ->
      Printf.printf "Warning: prop_check_call failed\n"; 
      None
    | Some out_mem_type ->
      let dep_context, taint_context = pr_type.context in
      Some { pr_type with
        pc = pr_type.pc + 1;
        reg_type = out_reg_type;
        flag_type = out_flag_type;
        mem_type = out_mem_type;
        context = (out_dep_context @ dep_context), (out_taint_context @ taint_context);
      }

end
