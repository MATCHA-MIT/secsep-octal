open Isa
open Taint_exp
open Taint_subtype
open Taint_type_infer
open Mem_type_new
open Mem_offset_new
open Full_mem_anno
open Call_anno_type
open Single_exp
open Pretty_print

module MemAnno = FullMemAnno
module Isa = Isa (MemAnno)

module ArchType = TaintTypeInfer.ArchType
module MemType = ArchType.MemType
module FuncInterface = ArchType.FuncInterface

exception TransformError of string
let transform_error msg = raise (TransformError ("[Transform Error] " ^ msg))

module InstTransform = struct

  type t = {
    (* was *)
    orig: Isa.instruction;

    (* now *)
    inst: Isa.instruction; (* transformed instruction *)
    inst_pre: Isa.instruction list; (* additional instruction inserted ahead, in reversed order *)
    inst_post: Isa.instruction list; (* additional instruction inserted after, in reversed order *)

    (* extra info *)
    changed: bool;
  }

  let init (orig: Isa.instruction) : t =
    {
      orig = orig;
      inst = orig;
      inst_pre = [];
      inst_post = [];
      changed = false;
    }

  (* make sure to set values through this function, so that extra info can be updated *)
  let assign (t: t) (inst: Isa.instruction) (inst_pre: Isa.instruction list) (inst_post: Isa.instruction list) : t =
    if t.changed then transform_error "Change a changed instruction not supported yet";
    let changed =
      not (List.is_empty inst_pre) ||
      not (List.is_empty inst_post) ||
      (Isa.string_of_instruction t.orig <> Isa.string_of_instruction inst)
    in
    {
      t with
      inst = inst;
      inst_pre = inst_pre;
      inst_post = inst_post;
      changed = changed;
    }

  let get_res_rev (t: t) : Isa.instruction list =
    t.inst_post @ [t.inst] @ t.inst_pre

  let get_res (t: t) : Isa.instruction list = List.rev (get_res_rev t)

  let get_transformed_bb (t_list: t list) =
    List.fold_left (
      fun acc t -> (get_res_rev t) @ acc
    ) [] t_list
    |> List.rev

  let pp_transform (lvl: int) (buf: Buffer.t) (t: t) =
    if t.changed = false then begin
      PP.bprint_lvl lvl buf "%s;\n" (Isa.ocaml_string_of_instruction t.orig);
    end else begin
      PP.bprint_lvl lvl buf "\n";
      PP.bprint_lvl lvl buf "(* >>>>>>>>>>>>> WAS >>>>>>>>>>>>> *)\n";

      PP.bprint_lvl lvl buf "(* %s *)\n" (Isa.ocaml_string_of_instruction t.orig);

      List.iter (fun inst ->
        PP.bprint_lvl lvl buf "   %s;\n" (Isa.ocaml_string_of_instruction inst);
      ) (get_res t);

      PP.bprint_lvl lvl buf "(* <<<<<<<<<<<<< NOW <<<<<<<<<<<<< *)\n\n";
    end

  let pp_transformed (lvl: int) (buf: Buffer.t) (t: t) =
    List.iter (fun inst ->
      PP.bprint_lvl lvl buf "%s\n" (Isa.string_of_instruction inst);
    ) (get_res t);
    ()

end

module Transform = struct

  type unity =
    | Unified
    | Ununified

  type basic_block = {
    label: Isa.label;
    insts: InstTransform.t list;
  }

  (* function may uses some stack slot to save callee-saved registers *)
  type callee_slot = Isa.register * MemOffset.t

  type func_state = {
    bbs: basic_block list;
    bb_types: ArchType.t list;
    init_mem_unity: (Isa.imm_var_id * unity) list;
    callee_saving_slots: callee_slot list;
  }

  let delta: int64 = -0x100000L (* -1MB *)

  let mem_part_taint_counter
      (mem_part: 'a MemTypeBasic.mem_part)
      (get_entry_taint: Isa.imm_var_id -> MemOffset.t -> 'a -> TaintExp.t)
      : int * int * ((TaintExp.taint_var_id option) option) (* Counter for taint true, taint false, taint var *) =
    let mem_part_base, mem_slots = mem_part in
    List.fold_left(
      fun (acc: int * int * ((TaintExp.taint_var_id option) option)) (slot: 'a MemTypeBasic.mem_slot) ->
        let acc_t, acc_f, acc_var = acc in
        let offset, _, entry = slot in
        match get_entry_taint mem_part_base offset entry with
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
    ) (0, 0, None) mem_slots

  let get_mem_part_unity
      (mem_part: 'a MemTypeBasic.mem_part)
      (get_entry_taint: Isa.imm_var_id -> MemOffset.t -> 'a -> TaintExp.t)
      : unity =
    let base_id, slots = mem_part in
    let tot = List.length slots in
    if tot = 0 then transform_error "Empty memory part";
    match mem_part_taint_counter mem_part get_entry_taint with
    | x, 0, None when x = tot -> Unified
    | 0, x, None when x = tot -> Unified
    | 0, 0, Some (Some _) -> Unified
    | _, _, None -> Ununified
    | _ -> transform_error (Printf.sprintf "Transformation rejected due to mem part of %d" base_id)

  let init_func_state (ti_state: TaintTypeInfer.t) : func_state * Isa.imm_var_id =
    let helper_get_unity (mem: MemType.t) (rsp_var_id: Isa.imm_var_id) : (Isa.imm_var_id * unity) list =
        List.map (fun (mem_part: (MemType.entry_t MemType.mem_part)) ->
          let base_id, _ = mem_part in
          if base_id = rsp_var_id then (base_id, Ununified) else
          let unity =
            get_mem_part_unity mem_part (fun _ _ (entry: MemType.entry_t) -> let (_, taint) = entry in taint)
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
    let extended_bbs = List.map (fun (bb: Isa.basic_block) ->
      {
        label = bb.label;
        insts = List.map (fun inst -> InstTransform.init inst) bb.insts;
      }
    ) ti_state.func
    in
    {
      bbs = extended_bbs;
      bb_types = ti_state.func_type;
      init_mem_unity = helper_get_unity first_bb_state.mem_type rsp_var_id;
      callee_saving_slots = []
    }, rsp_var_id

  let get_slot_unity (mem_unity: (Isa.imm_var_id * unity) list) (base_id: Isa.imm_var_id) : unity =
    let result = List.find_map (fun (id, unity) ->
      if id = base_id then Some unity
      else None
    ) mem_unity
    in
    Option.get result

  let add_delta_on_imm (imm: Isa.immediate) (delta: int64) : Isa.immediate =
    match imm with
    | ImmNum x -> ImmNum (Int64.add x delta)
    | ImmLabel label -> ImmBExp (ImmLabel label, ImmNum delta)
    | ImmBExp (e1, e2) -> begin
        match e1, e2 with
        | ImmNum x, _ -> ImmBExp (ImmNum (Int64.add x delta), e2)
        | _, ImmNum y -> ImmBExp (e1, ImmNum (Int64.add y delta))
        | _ -> transform_error "Unexpected expression in immediate where displacement is an ImmBExp"
      end

  let add_delta_on_disp (mem_op: Isa.mem_op) : Isa.mem_op =
    let d, b, i, s = mem_op in
    match d with
    | None -> (Some (ImmNum delta), b, i, s)
    | Some x -> (Some (add_delta_on_imm x delta), b, i, s)

  let transform_instruction
      (func_state: func_state)
      (tf: InstTransform.t)
      (rsp_var_id: Isa.imm_var_id)
      (css: callee_slot list) (* css == callee_saving_slots *)
      : InstTransform.t * (callee_slot list) =
    let helper_add_callee_saving_slot (l: callee_slot list) (reg: Isa.register) (offset: MemOffset.t) : callee_slot list =
      match List.find_opt (fun (r, _) -> r = reg) l with
      | Some _ -> l
      | None -> (reg, offset) :: l
    in
    let helper_prepare_memop (operand: Isa.operand) : Isa.operand =
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
          | Unified -> operand
          | Ununified -> begin
              match taint_anno with
              | TaintConst true -> begin
                  (* accessing ununified memory, taint is true -> do the transformation *)
                  (* Printf.printf "transforming operand %s\n" (Isa.string_of_operand operand); *)
                  let (d', b', i', s') = add_delta_on_disp (d, b, i, s) in
                  match operand with
                    | LdOp _ -> LdOp (d', b', i', s', w, mem_anno)
                    | StOp _ -> StOp (d', b', i', s', w, mem_anno)
                    | _ -> transform_error "Unexpected memory operand accessing ununified memory"
                end
              | TaintConst false -> operand
              | _ -> transform_error "Unexpected taint annotation for memory operand accessing ununified memory"
            end
        end
      | _ -> operand
    in
    let orig = tf.orig in
    let (inst, inst_pre, inst_post), css = match orig with
    | BInst (bop, o1, o2, o3) -> begin
        let o1' = helper_prepare_memop o1 in
        let o2' = helper_prepare_memop o2 in
        let o3' = helper_prepare_memop o3 in
        let inst' = Isa.BInst (bop, o1', o2', o3') in
        (inst', [], []), css
      end
    | UInst (uop, o1, o2) -> begin
        (* find potential instrucitons saving callee-saved registers *)
        let css' = match uop, o1, o2 with
        | Isa.Mov, Isa.StOp(_, _, _, _, _, (slot_info, taint_info)), Isa.RegOp reg when Isa.is_reg_callee_saved reg ->
          (* TODO: is this enough to cover all cases? *)
          let (slot_base, slot_offset, slot_full), taint_info = Option.get slot_info, Option.get taint_info in
          if slot_base = rsp_var_id && slot_full && MemOffset.is_8byte_slot slot_offset && taint_info = TaintConst true then
            helper_add_callee_saving_slot css reg slot_offset
          else
            css
        | _ -> css
        in
        let o1' = helper_prepare_memop o1 in
        let o2' = helper_prepare_memop o2 in
        let inst' = Isa.UInst (uop, o1', o2') in
        (inst', [], []), css'
      end
    | Xchg (o1, o2, o3, o4) -> begin
        let o1' = helper_prepare_memop o1 in
        let o2' = helper_prepare_memop o2 in
        let o3' = helper_prepare_memop o3 in
        let o4' = helper_prepare_memop o4 in
        let inst' = Isa.Xchg (o1', o2', o3', o4') in
        (inst', [], []), css
      end
    | Cmp (o1, o2) -> begin
        let o1' = helper_prepare_memop o1 in
        let o2' = helper_prepare_memop o2 in
        let inst' = Isa.Cmp (o1', o2') in
        (inst', [], []), css
      end
    | Test (o1, o2) -> begin
        let o1' = helper_prepare_memop o1 in
        let o2' = helper_prepare_memop o2 in
        let inst' = Isa.Test (o1', o2') in
        (inst', [], []), css
      end
    | Jmp _
    | Jcond _
    | Nop
    | Syscall
    | Hlt -> (orig, [], []), css
    | Push (o, mem_anno)
    | Pop (o, mem_anno) -> begin
        let slot_anno, taint_anno = mem_anno in
        if slot_anno = None || taint_anno = None then
          transform_error "Memory annotation missing information";
        (* sanity check on stack access *)
        let (slot_base, slot_offset, slot_is_full) = Option.get slot_anno in
        if slot_base != rsp_var_id then
          transform_error "Push/Pop not accessing stack memory";
        if not slot_is_full then
          transform_error "Push/Pop not accessing full slot";
        let taint_anno = Option.get taint_anno in
        let offset_as_disp = match taint_anno with
        | TaintConst true -> Some (Isa.ImmNum delta)
        | TaintConst false -> None
        | TaintVar _ -> transform_error "TaintVar not expected for Push/Pop simulation"
        | _ -> transform_error "Unexpected taint annotation for Push/Pop simulation"
        in
        let o' = helper_prepare_memop o in
        let o_size = match o' with
        | RegOp reg -> Isa.get_reg_size reg
        | LdOp (_, _, _, _, w, _)
        | StOp (_, _, _, _, w, _) -> w
        | _ -> transform_error "Operand not supported for Push/Pop simulation"
        in
        match orig with
        | Push _ ->
          let inst_rsp_sub = Isa.make_inst_add_i_r (Int64.neg o_size) Isa.RSP in
          let inst_st = Isa.UInst(Isa.Mov, Isa.StOp(offset_as_disp, Some Isa.RSP, None, None, o_size, mem_anno), o') in
          let callee_saved_reg = Isa.is_opr_callee_saved_reg o' in
          let css' = if Option.is_some callee_saved_reg && Option.is_some offset_as_disp (* means the data is tainted *) then
            helper_add_callee_saving_slot css (Option.get callee_saved_reg) slot_offset
          else
            css
          in
          (inst_st, [inst_rsp_sub], []), css'
        | Pop _ ->
          let inst_ld = Isa.UInst(Isa.Mov, o', Isa.LdOp(offset_as_disp, Some Isa.RSP, None, None, o_size, mem_anno)) in
          let inst_rsp_add = Isa.make_inst_add_i_r o_size Isa.RSP in
          (inst_ld, [], [inst_rsp_add]), css
        | _ -> transform_error "Expecting only Push/Pop here"
      end
    | Call _ -> transform_error "Call unimplemented"
    | RepStosq -> transform_error "RepStosq unimplemented"
    | RepMovsq -> transform_error "RepStosq unimplemented"
    in
    (InstTransform.assign tf inst inst_pre inst_post), css

  let get_parent_slot_taint (parent_mem_type: MemType.t) (slot: CallAnno.slot_t) : TaintExp.t =
    (* get taint of the parent slot to which the current slot is mapped *)
    let (parent_slot_base, parent_slot_off, _), _ = slot in
    let _, taint = MemType.get_mem_type_strict parent_mem_type (parent_slot_base, parent_slot_off) |> Option.get in
    let _ = match taint with
    | TaintVar _ -> transform_error "Unexpected taint var when processing memory mapping in call"
    | _ -> ()
    in
    taint

  let get_child_slot_taint
        (child_fi_in_mem: FuncInterface.MemType.t)
        (base: Isa.imm_var_id)
        (offset: MemOffset.t)
        (_: 'a)
        : TaintExp.t =
    let _, taint = MemType.get_mem_type_strict child_fi_in_mem (base, offset) |> Option.get in
    taint

  let extract_bases_to_be_changed
      (func_state: func_state)
      (child_fi_in_mem: FuncInterface.MemType.t) (* wanna check taint type of slots the moment the call happens *)
      (anno: CallAnno.t')
      (rsp_var_id: Isa.imm_var_id)
      : CallAnno.base_info list =
    let res = List.fold_left (
      fun (acc: CallAnno.base_info list) (mem_part: CallAnno.slot_t MemType.mem_part) : CallAnno.base_info list ->
        let child_base, slots = mem_part in
        if List.length slots = 0 then transform_error "Empty memory part";

        (* sample the first slot to get info of base pointer, should be the same accross all slots *)
        let _, _, ((parent_base, _, _), parent_base_info) = (List.hd slots) in
        (* sanity check: all slots with same base in the child maps have same info about base *)
        List.iter (
          fun (_, _, ((parent_base', _, _), parent_base_info')) ->
            if parent_base' != parent_base || CallAnno.cmp_base_info parent_base_info parent_base_info' != 0 then
              transform_error "Different parent base in the same child memory part"
        ) (List.tl slots);

        (* do not transform, if parent mem is already unified or dealing with child's RSP *)
        if get_slot_unity func_state.init_mem_unity parent_base = Unified || child_base = rsp_var_id then acc else
        (* see if all slots are taint true or all slots are taint false *)
        let unity = get_mem_part_unity mem_part (get_child_slot_taint child_fi_in_mem) in
        if unity = Unified then parent_base_info :: acc else acc
    ) [] anno.ch_mem in
    List.sort_uniq CallAnno.cmp_base_info res

  let try_change_delta_of_dest_reg (tf: InstTransform.t) (reg: Isa.register) : InstTransform.t option =
    let dest_matched = match tf.orig with
      | BInst (_, RegOp reg', _, _) -> reg' = reg
      | UInst (_, RegOp reg', _) -> reg' = reg
      | Xchg (RegOp reg', _, _, _)
      | Xchg (_, RegOp reg', _, _) -> reg' = reg
      | Pop (RegOp reg', _) -> reg' = reg
      | _ -> false
    in
    if not dest_matched then None else
    if tf.changed then None else (* multiple transforms on same instruction not supported *)
    let res = match tf.orig with
    | BInst (Add, dst, src1, src2) -> begin
        let replaced = match src1, src2 with
        | ImmOp imm, _ -> Some (Isa.ImmOp (add_delta_on_imm imm delta), src2)
        | _, ImmOp imm -> Some (src1, Isa.ImmOp (add_delta_on_imm imm delta))
        | _ -> None (* TODO: not supported yet, can we do better? *)
        in
        match replaced with
        | None -> None
        | Some (src1', src2') -> Some (
            InstTransform.assign tf
              (BInst (Add, dst, src1', src2'))
              []
              []
          )
      end
    | UInst (Lea, dst, src) -> begin
        match src with
        | MemOp (d, b, i, s) -> Some (
            InstTransform.assign tf
              (UInst (Lea, dst, add_delta_on_disp (d, b, i, s) |> Isa.memop_to_mem_operand))
              []
              []
          )
        | _ -> None
      end
    | _ -> None (* TODO: implement more cases *)
    in
    match res with
    | Some _ -> res
    | None -> begin
        (* for cases that cannot be handled in the original instruction, we insert a post instruction to shift it *)
        Some (
          InstTransform.assign tf
            tf.orig
            []
            [Isa.make_inst_add_i_r delta reg]
        )
      end

  let get_stack_slot_dyn_offset (rsp_var_id: Isa.imm_var_id) (slot: MemOffset.t) (curr_rsp: SingleExp.t) : int64 =
    let addr_beg, _ = slot in
    let offset1 = SingleExp.match_const_offset addr_beg rsp_var_id |> Option.get in
    let offset2 = SingleExp.match_const_offset curr_rsp rsp_var_id |> Option.get in
    Int64.sub offset1 offset2

  let transform_basic_block_calls
      (func_state: func_state)
      (func_interfaces: FuncInterface.t list)
      (bb: basic_block)
      (rsp_var_id: Isa.imm_var_id)
      : basic_block =
    let insts' = List.fold_left (
      fun (acc_tf: InstTransform.t list) (* processed insts (all previous inst), reversed *)
          (tf: InstTransform.t) (* current inst *) ->
        match tf.orig with
        | Call (child_label, anno) -> begin
            let child_fi = FuncInterface.find_fi func_interfaces child_label |> Option.get in
            let anno = Option.get anno in
            let bases_to_change = extract_bases_to_be_changed func_state child_fi.in_mem anno rsp_var_id in
            let new_prev_tfs, bases_to_change = List.fold_left (
              (* sweep (in reversed order) and find instructions that prepare bases *)
              fun (acc: (InstTransform.t list) * (CallAnno.base_info list)) (tf: InstTransform.t) ->
                let acc_tf, bases_to_change = acc in
                if List.length bases_to_change = 0 then (tf :: acc_tf, []) else
                let result = List.find_map (
                    fun (base_info: CallAnno.base_info) ->
                      match base_info with
                      | BaseAsReg reg -> begin
                        let replaced = try_change_delta_of_dest_reg tf reg in
                        match replaced with
                        | Some tf' -> Some (tf', base_info)
                        | None -> None
                        end
                      | BaseAsSlot _ -> None (* we will handle this later, see below *)
                  ) bases_to_change in
                  if Option.is_some result then begin
                    let new_tf, handled_base = Option.get result in
                    let bases_to_change = List.filter (fun base_info -> CallAnno.cmp_base_info handled_base base_info != 0) bases_to_change in
                    (new_tf :: acc_tf, bases_to_change)
                  end else
                    (tf :: acc_tf, bases_to_change)
            ) ([], bases_to_change) acc_tf
            in
            (* deal with all BaseAsSlot stuff here *)
            let inst_pre, inst_post = List.fold_left (
              fun (acc: (Isa.instruction list * Isa.instruction list)) (base_info: CallAnno.base_info) ->
                let acc_inst_pre, acc_inst_post = acc in
                match base_info with
                | BaseAsReg reg ->
                  if Isa.is_reg_callee_saved reg then
                    transform_error "trying to temporarily transform a callee saved register, unexpceted";
                  (
                    (Isa.make_inst_add_i_r delta reg) :: acc_inst_pre,
                    acc_inst_post (* no need to restore, since the register is caller saved *)
                  )
                | BaseAsSlot (ch_base, ch_base_off) ->
                  (* where it is in child's memory slot *)
                  (* sanity check: the slot is a full slot in parent's memory *)
                  let ch_slot_anno = match MemType.get_mem_type_strict anno.ch_mem (ch_base, ch_base_off) with
                  | None -> transform_error "sanity check failed: BaseAsSlot does not describe a full slot in child's memory";
                  | Some x -> x
                  in
                  let (_, pa_base_off, is_full), base_info = ch_slot_anno in
                  if not is_full then
                    transform_error "sanity check failed: BaseAsSlot is not mapped to a full slot of parent";
                  if not (MemOffset.is_8byte_slot pa_base_off) then
                    transform_error "sanity check failed: BaseAsSlot does not access slot of 8 bytes";
                  let base_reg_of_base = match base_info with
                  | BaseAsReg r -> r
                  | BaseAsSlot (b, o) ->
                    transform_error (Printf.sprintf
                      "Unexpected recursive base: some base is passed in memory of base %d, which is still in memory of %d (%s)"
                      ch_base
                      b
                      (MemOffset.to_string o)
                    )
                  in
                  let off_of_base = SingleExp.match_const_offset (fst ch_base_off) ch_base |> Option.get in
                  let mem_op = (Some (Isa.ImmNum off_of_base), Some base_reg_of_base, None, None) in
                  (* get the memory operand of the slot to be changed *)
                  let mem_op = match get_child_slot_taint child_fi.in_mem ch_base ch_base_off None with
                  | TaintConst true -> begin
                      transform_error "we assert the base is untainted";
                      (* let d, b, i, s = mem_op in     *)
                      (* add_delta_on_disp (d, b, i, s) *)
                    end
                  | TaintConst false -> mem_op
                  | _ -> transform_error "Unexpected taint annotation for BaseAsSlot"
                  in
                  (
                    (Isa.make_inst_add_i_m64 delta mem_op) :: acc_inst_pre,
                    (Isa.make_inst_add_i_m64 (Int64.neg delta) mem_op) :: acc_inst_post
                  )
            ) ([], []) bases_to_change in
            (* save/restore active callee-saved registers in caller's scope *)
            let saves, restores = List.split (
              List.filter_map (fun (slot: callee_slot) ->
                let reg, offset = slot in
                let (curr_rsp, curr_rsp_taint) = CallAnno.TaintRegType.get_reg_type anno.pr_reg Isa.RSP in
                (* no conservative here, as the register is confirmed tainted *)
                if curr_rsp_taint = TaintConst true then None else
                let disp = get_stack_slot_dyn_offset rsp_var_id offset curr_rsp in
                let disp = if disp != 0L then Some (Isa.ImmNum disp) else None in
                let mem_op = (disp, Some Isa.RSP, None, None) in
                let inst_save = Isa.make_inst_st_r64_m64 reg mem_op in
                let inst_restore = Isa.make_inst_ld_r64_m64 reg mem_op in
                Some (inst_save, inst_restore)
              ) func_state.callee_saving_slots
            ) in
            let inst_pre = saves @ inst_pre in
            let inst_post = inst_post @ restores in
            (* transformation done *)
            let tf = InstTransform.assign tf tf.orig inst_pre inst_post in
            tf :: (List.rev new_prev_tfs)
          end
        | _ -> tf :: acc_tf
    ) [] bb.insts
    in
    { bb with insts = insts' }

  let pp_ocaml_block_list (lvl: int) (buf: Buffer.t) (block_list: basic_block list) =
    PP.bprint_lvl lvl buf "[\n";
    List.iter (
      fun bb ->
        PP.bprint_lvl (lvl + 1) buf "{\n";
        PP.bprint_lvl (lvl + 2) buf "label = \"%s\";\n" bb.label;
        PP.bprint_lvl (lvl + 2) buf "insts = [\n";
        List.iter (
          fun tf -> InstTransform.pp_transform (lvl + 3) buf tf
        ) bb.insts;
        PP.bprint_lvl (lvl + 2) buf "]\n";
        PP.bprint_lvl (lvl + 1) buf "};\n";
    ) block_list;
    PP.bprint_lvl lvl buf "]\n"

  let pp_func_state (func_state: func_state) =
    let buf = Buffer.create 1000 in

    PP.bprint_lvl 0 buf "\nTransformed function %s\n" (List.hd func_state.bb_types).label;
    PP.bprint_lvl 0 buf "Program:\n";
    pp_ocaml_block_list 0 buf func_state.bbs;
    PP.bprint_lvl 0 buf "\nBB Types:\n";
    ArchType.pp_ocaml_arch_type_list 0 buf func_state.bb_types;
    PP.bprint_lvl 0 buf "\n";

    Printf.printf "%s" (String.of_bytes (Buffer.to_bytes buf));
    ()

  let transform_basic_block_basic
      (func_state: func_state)
      (bb: basic_block)
      (rsp_var_id: Isa.imm_var_id)
      (css: callee_slot list)
      : basic_block * (callee_slot list) =
    let new_insts, css' = List.fold_left (
      fun (acc: (InstTransform.t list) * (callee_slot list)) (trans: InstTransform.t) ->
        let acc_insts, acc_css = acc in
        let tf, css = transform_instruction func_state trans rsp_var_id acc_css in
        tf :: acc_insts, css
    ) ([], css) bb.insts in
    { bb with insts = List.rev new_insts }, css'

  let transform_one_function
      (fi_list: FuncInterface.t list)
      (taint_infer_state: TaintTypeInfer.t)
      : func_state =
    let func_state, rsp_var_id = init_func_state taint_infer_state in

    (* basic pass *)
    let bbs, css = List.fold_left (
      fun acc (bb: basic_block) ->
        let acc_bbs, acc_css = acc in
        let bb', css = transform_basic_block_basic func_state bb rsp_var_id acc_css in
        bb' :: acc_bbs, css
    ) ([], []) func_state.bbs in
    let func_state = {
      func_state with
      bbs = List.rev bbs;
      callee_saving_slots = css;
    }
    in

    (* call handling pass *)
    let bbs = List.map (
      fun bb -> transform_basic_block_calls func_state fi_list bb rsp_var_id
    ) func_state.bbs in
    let func_state = {
      func_state with
      bbs = bbs;
    }
    in

    pp_func_state func_state;
    func_state
end
