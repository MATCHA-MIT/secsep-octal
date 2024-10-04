open Isa
open Taint_exp
open Taint_subtype
open Taint_type_infer
open Full_mem_anno
open Pretty_print

module Transform = struct

  exception TransformError of string
  let transform_error msg = raise (TransformError ("[Transform Error] " ^ msg))

  module MemAnno = FullMemAnno
  module Isa = Isa (MemAnno)

  module ArchType = TaintTypeInfer.ArchType
  module MemType = ArchType.MemType
  module FuncInterface = ArchType.FuncInterface

  type unity =
    | Unified
    | Ununified

  type func_state = {
    bbs: Isa.basic_block list;
    bb_types: ArchType.t list;
    init_mem_unity: (Isa.imm_var_id * unity) list;
  }

  let delta: int64 = -0x100000L (* -1MB *)

  let init_func_state (ti_state: TaintTypeInfer.t) : func_state * Isa.imm_var_id =
    let helper_get_unity (mem: MemType.t) (rsp_var_id: Isa.imm_var_id) : (Isa.imm_var_id * unity) list =
        List.map (fun (mem_part: (MemType.entry_t MemType.mem_part)) ->
          let base_id, slots = mem_part in
          let tot = List.length slots in
          if tot = 0 then
            transform_error "Empty memory part";
          if base_id = rsp_var_id then (base_id, Ununified) else
          let cnt_t, cnt_f, var = List.fold_left (fun (acc: (int * int * ((TaintExp.taint_var_id option) option))) (slot: MemType.entry_t MemType.mem_slot) ->
            let acc_t, acc_f, acc_var = acc in
            let _, _, (_, taint) = slot in
            match taint with
            | TaintConst true -> (acc_t + 1, acc_f, acc_var)
            | TaintConst false -> (acc_t, acc_f + 1, acc_var)
            | TaintVar id ->
              (
                acc_t,
                acc_f,
                match acc_var with
                | Some None -> Some None (* two or more different variable exist *)
                | None -> Some (Some id)
                | Some (Some id') ->
                  if id = id' then Some (Some id)
                  else Some None
              )
            | _ -> transform_error "Unexpected slot with taint type of taint expression"
          ) (0, 0, None) slots
          in
          let unity = match cnt_t, cnt_f, var with
          | x, 0, None when x = tot -> Unified
          | 0, x, None when x = tot -> Unified
          | 0, 0, Some (Some _) -> Unified
          | _, _, None -> Ununified
          | _ -> transform_error (Printf.sprintf "Transformation rejected due to mem part of %d" base_id)
          in
          (base_id, unity)
      ) mem
    in
    let first_bb_state = List.hd ti_state.func_type in
    if not (Isa.is_label_function_entry first_bb_state.label) then
      transform_error "First basic block is not function entry";
    let rsp_var_id = match TaintSubtype.TaintEntryType.get_single_exp (ArchType.RegType.get_reg_type first_bb_state.reg_type Isa.RSP) with
    | SingleVar var_id -> var_id
    | _ -> transform_error "Unexpected single expression for RSP"
    in
    {
      bbs = ti_state.func;
      bb_types = ti_state.func_type;
      init_mem_unity = helper_get_unity first_bb_state.mem_type rsp_var_id;
    }, rsp_var_id

  let get_slot_unity (mem_unity: (Isa.imm_var_id * unity) list) (base_id: Isa.imm_var_id) : unity =
    let result = List.find_map (fun (id, unity) ->
      if id = base_id then Some unity
      else None
    ) mem_unity
    in
    Option.get result

  let transform_instruction (func_state: func_state) (inst: Isa.instruction) (rsp_var_id: Isa.imm_var_id) : Isa.instruction list =
    let helper_add_delta (mem_op: Isa.mem_op) : Isa.mem_op * (Isa.instruction list) =
      let d, b, i, s = mem_op in
      match d, b with
      | None, _ -> ((Some (ImmNum delta), b, i, s), [])
      | Some (ImmLabel _), Some base -> (mem_op, [Isa.make_inst_add_i_r delta base])
      | Some disp, _ -> begin
          match disp with
          | ImmNum x -> ((Some (ImmNum (Int64.add x delta)), b, i, s), [])
          | ImmLabel _ -> transform_error "TODO: add delta for memory operand with label but no base"
          | ImmBExp _ -> transform_error "Unexpected immediate expression in memory operand"
        end
    in
    let helper_prepare_memop (operand: Isa.operand) : Isa.operand * (Isa.instruction list)  =
      (* if not ld/st, do nothing *)
      (* otherwise, return instructions for preparation and transformed operand *)
      match operand with
      | LdOp (d, b, i, s, w, mem_anno)
      | StOp (d, b, i, s, w, mem_anno) -> begin
          let slot_anno, taint_anno = mem_anno in
          if slot_anno = None || taint_anno = None then
            transform_error "Memory annotation missing information";
          let (slot_base, _, _) = Option.get slot_anno in
          let taint_anno = Option.get taint_anno in
          match get_slot_unity func_state.init_mem_unity slot_base with
          | Unified -> (operand, [])
          | Ununified -> begin
              match taint_anno with
              | TaintConst true -> begin
                  (* accessing ununified memory, taint is true -> do the transformation *)
                  (* Printf.printf "transforming operand %s\n" (Isa.string_of_operand operand); *)
                  let (d', b', i', s'), prep_insts = helper_add_delta (d, b, i, s) in
                  match operand with
                    | LdOp _ -> (LdOp (d', b', i', s', w, mem_anno), prep_insts)
                    | StOp _ -> (StOp (d', b', i', s', w, mem_anno), prep_insts)
                    | _ -> transform_error "Unexpected memory operand accessing ununified memory"
                end
              | TaintConst false -> (operand, [])
              | _ -> transform_error "Unexpected taint annotation for memory operand accessing ununified memory"
            end
        end
      | _ -> (operand, [])
    in
    match inst with
    | BInst (bop, o1, o2, o3) -> begin
        let o1', prep_insts1 = helper_prepare_memop o1 in
        let o2', prep_insts2 = helper_prepare_memop o2 in
        let o3', prep_insts3 = helper_prepare_memop o3 in
        let inst' = Isa.BInst (bop, o1', o2', o3') in
        inst' :: (prep_insts3 @ prep_insts2 @ prep_insts1)
      end
    | UInst (uop, o1, o2) -> begin
        let o1', prep_insts1 = helper_prepare_memop o1 in
        let o2', prep_insts2 = helper_prepare_memop o2 in
        let inst' = Isa.UInst (uop, o1', o2') in
        inst' :: (prep_insts2 @ prep_insts1)
      end
    | Xchg (o1, o2, o3, o4) -> begin
        let o1', prep_insts1 = helper_prepare_memop o1 in
        let o2', prep_insts2 = helper_prepare_memop o2 in
        let o3', prep_insts3 = helper_prepare_memop o3 in
        let o4', prep_insts4 = helper_prepare_memop o4 in
        let inst' = Isa.Xchg (o1', o2', o3', o4') in
        inst' :: (prep_insts4 @ prep_insts3 @ prep_insts2 @ prep_insts1)
      end
    | Cmp (o1, o2) -> begin
        let o1', prep_insts1 = helper_prepare_memop o1 in
        let o2', prep_insts2 = helper_prepare_memop o2 in
        let inst' = Isa.Cmp (o1', o2') in
        inst' :: (prep_insts2 @ prep_insts1)
      end
    | Test (o1, o2) -> begin
        let o1', prep_insts1 = helper_prepare_memop o1 in
        let o2', prep_insts2 = helper_prepare_memop o2 in
        let inst' = Isa.Test (o1', o2') in
        inst' :: (prep_insts2 @ prep_insts1)
      end
    | Jmp _
    | Jcond _
    | Nop
    | Syscall
    | Hlt -> [inst]
    | Push (o, mem_anno)
    | Pop (o, mem_anno) -> begin
        let slot_anno, taint_anno = mem_anno in
        if slot_anno = None || taint_anno = None then
          transform_error "Memory annotation missing information";
        (* sanity check on stack access *)
        let (slot_base, _, _) = Option.get slot_anno in
        if slot_base != rsp_var_id then
          transform_error "Push/Pop not accessing stack memory";
        let taint_anno = Option.get taint_anno in
        let offset_as_disp = match taint_anno with
        | TaintConst true -> Some (Isa.ImmNum delta)
        | TaintConst false -> None
        | TaintVar _ -> transform_error "TaintVar not expected for Push/Pop simulation"
        | _ -> transform_error "Unexpected taint annotation for Push/Pop simulation"
        in
        let o', prep_insts = helper_prepare_memop o in
        let o_size = match o' with
        | RegOp reg -> Isa.get_reg_size reg
        | LdOp (_, _, _, _, w, _)
        | StOp (_, _, _, _, w, _) -> w
        | _ -> transform_error "Operand not supported for Push/Pop simulation"
        in
        match inst with
        | Push _ ->
          let inst_rsp_sub = Isa.make_inst_add_i_r (Int64.neg o_size) Isa.RSP in
          let inst_st = Isa.UInst(Isa.Mov, Isa.StOp(offset_as_disp, Some Isa.RSP, None, None, o_size, mem_anno), o') in
          inst_st :: inst_rsp_sub :: prep_insts
        | Pop _ ->
          let inst_ld = Isa.UInst(Isa.Mov, o', Isa.LdOp(offset_as_disp, Some Isa.RSP, None, None, o_size, mem_anno)) in
          let inst_rsp_add = Isa.make_inst_add_i_r o_size Isa.RSP in
          inst_rsp_add :: inst_ld :: prep_insts
        | _ -> transform_error "Expecting only Push/Pop here"
      end
    | Call _ -> transform_error "Call unimplemented"
    | RepStosq -> transform_error "RepStosq unimplemented"
    | RepMovsq -> transform_error "RepStosq unimplemented"

  let pp_func_state (func_state: func_state) =
    let buf = Buffer.create 1000 in

    PP.bprint_lvl 0 buf "\nTransformed function %s\n" (List.hd func_state.bbs).label;
    PP.bprint_lvl 0 buf "Program:\n";
    Isa.pp_ocaml_block_list 0 buf func_state.bbs;
    PP.bprint_lvl 0 buf "\nBB Types:\n";
    ArchType.pp_ocaml_arch_type_list 0 buf func_state.bb_types;
    PP.bprint_lvl 0 buf "\n";

    Printf.printf "%s" (String.of_bytes (Buffer.to_bytes buf));
    ()

  let transform_basic_block (func_state: func_state) (bb: Isa.basic_block) (rsp_var_id: Isa.imm_var_id) : Isa.basic_block =
    let new_insts = List.fold_left (
      fun (acc: Isa.instruction list) (inst: Isa.instruction) ->
        (transform_instruction func_state inst rsp_var_id) @ acc
    ) [] bb.insts in
    { bb with insts = List.rev new_insts }

  let transform_one_function 
      ((* func_interface_list *) _: FuncInterface.t list)
      (taint_infer_state: TaintTypeInfer.t)
      : func_state =
    let func_state, rsp_var_id = init_func_state taint_infer_state in
    let func_state =
      { func_state with bbs = List.map (fun (bb: Isa.basic_block) -> transform_basic_block func_state bb rsp_var_id) func_state.bbs }
    in
    pp_func_state func_state;
    func_state
end
