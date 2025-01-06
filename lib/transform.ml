open Isa
open Parser
open Taint_exp
open Taint_entry_type
open Taint_type_infer
open Mem_type_new
open Mem_offset_new
open Full_mem_anno
open Call_anno_type
open Single_exp
open Pretty_print

exception TransformError of string
let transform_error msg = raise (TransformError ("[Transform Error] " ^ msg))

module InstTransform = struct

  module MemAnno = FullMemAnno
  module Isa = Isa (MemAnno)

  type t = {
    (* was *)
    mnemonic: string;
    orig_asm: string option;
    orig: Isa.instruction;

    (* now *)
    inst: Isa.instruction; (* transformed instruction *)
    inst_pre: Isa.instruction list; (* additional instruction inserted ahead, in reversed order *)
    inst_post: Isa.instruction list; (* additional instruction inserted after, in reversed order *)

    (* extra info *)
    changed: bool;
    use_orig_mne: bool; (* when output, whether to use t.mnemonic as mnemonic or let t.inst to decide *)
    failed: bool;
  }

  let init (orig: Isa.instruction) (mnemonic: string) (orig_asm: string option) : t =
    {
      mnemonic = mnemonic;
      orig_asm = orig_asm;
      orig = orig;
      inst = orig;
      inst_pre = [];
      inst_post = [];
      changed = false;
      use_orig_mne = true;
      failed = false;
    }

  (* make sure to set values through this function, so that extra info can be updated *)
  let assign (t: t) (inst: Isa.instruction) (inst_pre: Isa.instruction list) (inst_post: Isa.instruction list) (use_orig_mne: bool) : t =
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
      use_orig_mne = use_orig_mne;
    }

  let set_failed (t: t) : t =
    Printf.printf "transformation on inst failed: %s\n" (Isa.string_of_instruction t.orig);
    {
      t with
      inst = t.orig;
      inst_pre = [];
      inst_post = [];
      changed = false;
      use_orig_mne = true;
      failed = true;
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

  module MemAnno = FullMemAnno
  module Isa = Isa (MemAnno)

  module ArchType = TaintTypeInfer.ArchType
  module RegType = ArchType.RegType
  module MemType = ArchType.MemType
  module FuncInterface = ArchType.FuncInterface

  type unity =
    | Unified
    | Ununified

  type basic_block = {
    label: Isa.label;
    insts: InstTransform.t list;
    alive_inst_cnt: int;
  }

  (* Unity of memory of each base in a function *)
  type mem_unity_t = (Isa.imm_var_id * unity) list

  (* function may uses some stack slot to save callee-saved registers *)
  type callee_slot = Isa.register * MemOffset.t

  type func_state = {
    func_name: string;
    bbs: basic_block list;
    bb_types: ArchType.t list;
    init_mem_unity: mem_unity_t;
    init_rsp_var_id: Isa.imm_var_id;
    callee_saving_slots: callee_slot list;
  }

  type tv_ittt = TaintExp.t -> TaintExp.t

  type tv_fault_t = Isa_basic.IsaBasic.label * Isa_basic.IsaBasic.label * TaintExp.taint_var_id * string

  let delta: int64 = -0x100000L (* -1MB *)

  let is_bb_transformed (bb: basic_block) : bool =
    List.exists (fun (tf: InstTransform.t) -> tf.changed) bb.insts

  let is_func_transformed (func_state: func_state) : bool =
    List.exists is_bb_transformed func_state.bbs

  let get_rsp_var_from_reg_type (reg_type: RegType.t) : Isa.imm_var_id =
    let rsp_var_id = match TaintEntryType.get_single_exp (ArchType.RegType.get_reg_type reg_type Isa.RSP) with
    | SingleVar var_id -> var_id
    | _ -> transform_error "Unexpected single expression for RSP"
    in
    rsp_var_id

  let get_func_init_rsp_var (ti_state: TaintTypeInfer.t) : Isa.imm_var_id =
    let first_bb_state = List.hd ti_state.func_type in

    if not (Isa.is_label_function_entry first_bb_state.label) then
      transform_error "First basic block is not function entry";

    get_rsp_var_from_reg_type first_bb_state.reg_type

  let mem_part_taint_counter
      (mem_part: 'a MemTypeBasic.mem_part)
      (get_entry_taint: Isa.imm_var_id -> MemOffset.t -> 'a -> TaintExp.t)
      : int * int * ((TaintExp.taint_var_id option) option) (* Counter for taint true, taint false, taint var *) =
    let (mem_part_base, _), mem_slots = mem_part in
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
    let (base_id, _), slots = mem_part in
    let tot = List.length slots in
    if tot = 0 then transform_error "Empty memory part";
    match mem_part_taint_counter mem_part get_entry_taint with
    | x, 0, None when x = tot -> Unified
    | 0, x, None when x = tot -> Unified
    | 0, 0, Some (Some _) -> Unified
    | _, _, None -> Ununified
    | _ -> transform_error (Printf.sprintf "Transformation rejected due to mem part of %d" base_id)

  (* mock the real instantiate step *)

  (*
  let get_taint_var_instantiate_map (func_name: string) : TaintExp.local_var_map_t =
    let helper (untaint_list: TaintExp.taint_var_id list) (taint_list: TaintExp.taint_var_id list) =
      List.map (fun x -> (x, TaintExp.TaintConst false)) untaint_list @
      List.map (fun x -> (x, TaintExp.TaintConst true)) taint_list
    in
    match func_name with
    | "salsa20_words" -> helper [] [4; 6; 13; 14; 15; 16]
    | "salsa20_block" -> helper [] [4]
    | "salsa20" -> helper [] [4; 6; 13; 14; 15]
    | "_start" -> helper [] [4; 6]
    | _ -> []
  *)

  let get_taint_var_instantiate_map (ti: TaintTypeInfer.t) : TaintExp.local_var_map_t =
    let helper (untaint_list: TaintExp.taint_var_id list) (taint_list: TaintExp.taint_var_id list) =
      List.map (fun x -> (x, TaintExp.TaintConst false)) untaint_list @
      List.map (fun x -> (x, TaintExp.TaintConst true)) taint_list
    in
    let first_bb_type = List.hd ti.func_type in
    let taint_tv_list = List.filter_map (fun idx ->
      let reg_type = List.nth first_bb_type.reg_type idx in
      match snd reg_type with
      | TaintVar tv -> Some tv
      | _ -> None
    ) Isa.callee_saved_reg_idx in
    helper [] taint_tv_list

  let taint_var_ittt (ittt_map: TaintExp.local_var_map_t) (taint_exp: TaintExp.t) : TaintExp.t =
    match taint_exp with
    | TaintVar tv -> begin
        let sub = List.find_map (fun (id, taint) ->
          if id = tv then Some taint
          else None
        ) ittt_map in
        match sub with
        | Some x -> x
        | None -> taint_exp
      end
    | _ -> taint_exp

  (*
  (* TODO: Use it to handle callee-saved registers, init them as TaintConst false *)
  let instantiate_taint_vars
      (_: TaintExp.local_var_map_t)
      (ti_state: TaintTypeInfer.t)
      : TaintTypeInfer.t =
    (* TODO: instantiate TV in program (i.e. all MemAnno and CallAnno) *)
    ti_state
  *)

  (* mock the real instantiate step *)

  let init_func_state
      (ti_state: TaintTypeInfer.t)
      (init_mem_unity: mem_unity_t)
      : func_state =
    let extended_bbs = List.map2 (fun (bb: Isa.basic_block) (bb_type: TaintTypeInfer.ArchType.t) ->
      if (List.length bb.mnemonics) != (List.length bb.insts) ||
         (List.length bb.orig_asm) - 1 != (List.length bb.insts) then (* first orig_asm is a label *)
        transform_error (Printf.sprintf "Mismatch between mnemonics/orig_asm and instructions %d %d %d" (List.length bb.mnemonics) (List.length bb.orig_asm) (List.length bb.insts));
      {
        label = bb.label;
        insts = List.map2 (
          fun inst (mnemonic, orig_asm) -> InstTransform.init inst mnemonic orig_asm
        ) bb.insts (List.combine bb.mnemonics (List.tl bb.orig_asm (* first orig_asm is a label *)));
        alive_inst_cnt = bb_type.dead_pc - bb_type.pc;
      }
    ) ti_state.func ti_state.func_type in
    {
      func_name = ti_state.func_name;
      bbs = extended_bbs;
      bb_types = ti_state.func_type;
      init_mem_unity = init_mem_unity;
      init_rsp_var_id = get_func_init_rsp_var ti_state;
      callee_saving_slots = [];
    }

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
      (tv_ittt: tv_ittt)
      (tf: InstTransform.t)
      (css: callee_slot list) (* css == callee_saving_slots *)
      : InstTransform.t * (callee_slot list) * ((TaintExp.taint_var_id * string) list) =
    let helper_add_callee_saving_slot (l: callee_slot list) (reg: Isa.register) (offset: MemOffset.t) : callee_slot list =
      match List.find_opt (fun (r, _) -> r = reg) l with
      | Some _ -> l
      | None -> (reg, offset) :: l
    in
    let helper_prepare_memop (operand: Isa.operand) : Isa.operand * ((TaintExp.taint_var_id * string) list) =
      (* if not ld/st, do nothing *)
      (* otherwise, return instructions for preparation and transformed operand *)
      match operand with
      | LdOp (d, b, i, s, w, mem_anno)
      | StOp (d, b, i, s, w, mem_anno) -> begin
          let slot_anno, taint_anno = mem_anno in
          if slot_anno = None || taint_anno = None then
            transform_error "Memory annotation missing information";
          let (slot_base, _, _, _) = Option.get slot_anno in
          let taint_anno = Option.get taint_anno in
          let taint_anno = tv_ittt taint_anno in (* mocking *)
          match get_slot_unity func_state.init_mem_unity slot_base with
          | Unified -> operand, []
          | Ununified -> begin
              match taint_anno with
              | TaintConst true -> begin
                  (* accessing ununified memory, taint is true -> do the transformation *)
                  (* Printf.printf "transforming operand %s\n" (Isa.string_of_operand operand); *)
                  let (d', b', i', s') = add_delta_on_disp (d, b, i, s) in
                  match operand with
                    | LdOp _ -> LdOp (d', b', i', s', w, mem_anno), []
                    | StOp _ -> StOp (d', b', i', s', w, mem_anno), []
                    | _ -> transform_error "Unexpected memory operand accessing ununified memory"
                end
              | TaintConst false -> operand, []
              | TaintVar v -> operand, [(v, "ld/st with variable taint")] (* soft fault *)
              | TaintExp v_set -> operand, TaintExp.TaintVarSet.to_list v_set |> List.map (fun x -> (x, "ld/st with variable taint")) (* soft fault *)
              | TaintUnknown -> transform_error (Printf.sprintf "ld/st encounters unknown taint status: %s" (Isa.string_of_operand operand));
            end
        end
      | _ -> operand, []
    in
    let orig = tf.orig in
    let (inst, inst_pre, inst_post, use_orig_mnemonic), css, sf = match orig with
    | BInst (bop, o1, o2, o3) -> begin
        let o1', sf1 = helper_prepare_memop o1 in
        let o2', sf2 = helper_prepare_memop o2 in
        let o3', sf3 = helper_prepare_memop o3 in
        let inst' = Isa.BInst (bop, o1', o2', o3') in
        (inst', [], [], true), css, (sf1 @ sf2 @ sf3)
      end
    | UInst (uop, o1, o2) -> begin
        (* find potential instrucitons saving callee-saved registers *)
        let css' = match uop, o1, o2 with
        | Isa.Mov, Isa.StOp(_, _, _, _, _, (slot_info, taint_info)), Isa.RegOp reg when Isa.is_reg_callee_saved reg && (String.starts_with ~prefix:"push" tf.mnemonic) ->
          (* TODO: is this enough to cover all cases? *)
          (* TODO: Can you also handle the case of accessing multiple slots here?*)
          let (slot_base, slot_offset, slot_full, _), taint_info = Option.get slot_info, Option.get taint_info in
          let taint_info = tv_ittt taint_info in (* mocking *)
          if slot_base = func_state.init_rsp_var_id && slot_full && MemOffset.is_8byte_slot slot_offset && taint_info = TaintConst true then
            helper_add_callee_saving_slot css reg slot_offset
          else
            css
        | _ -> css
        in
        let o1', sf1 = helper_prepare_memop o1 in
        let o2', sf2 = helper_prepare_memop o2 in
        let inst' = Isa.UInst (uop, o1', o2') in
        (inst', [], [], true), css', (sf1 @ sf2)
      end
    | TInst (top, o_dst, o_srcs) ->
        let o_dst', sf1 = helper_prepare_memop o_dst in
        let sf2, o_srcs' = List.fold_left_map (fun acc_sf o_src ->
          let o_src', sf = helper_prepare_memop o_src in
          sf @ acc_sf, o_src'
        ) [] o_srcs in
        let inst' = Isa.TInst (top, o_dst', o_srcs') in
        (inst', [], [], true), css, (sf1 @ sf2)
    | Xchg (o1, o2, o3, o4) -> begin
        let o1', sf1 = helper_prepare_memop o1 in
        let o2', sf2 = helper_prepare_memop o2 in
        let o3', sf3 = helper_prepare_memop o3 in
        let o4', sf4 = helper_prepare_memop o4 in
        let inst' = Isa.Xchg (o1', o2', o3', o4') in
        (inst', [], [], true), css, (sf1 @ sf2 @ sf3 @ sf4)
      end
    | Cmp (o1, o2) -> begin
        let o1', sf1 = helper_prepare_memop o1 in
        let o2', sf2 = helper_prepare_memop o2 in
        let inst' = Isa.Cmp (o1', o2') in
        (inst', [], [], true), css, (sf1 @ sf2)
      end
    | Test (o1, o2) -> begin
        let o1', sf1 = helper_prepare_memop o1 in
        let o2', sf2 = helper_prepare_memop o2 in
        let inst' = Isa.Test (o1', o2') in
        (inst', [], [], true), css, (sf1 @ sf2)
      end
    | Jmp _
    | Jcond _
    | Nop
    | Syscall
    | Annotation _
    | Hlt -> (orig, [], [], true), css, []
    | Push (o, mem_anno)
    | Pop (o, mem_anno) -> begin
        let slot_anno, taint_anno = mem_anno in
        if slot_anno = None || taint_anno = None then
          transform_error "Memory annotation missing information";
        (* sanity check on stack access *)
        (* TODO: Can you also handle the case of accessing multiple slots here?*)
        let (slot_base, slot_offset, slot_is_full, _) = Option.get slot_anno in
        if slot_base != func_state.init_rsp_var_id then
          transform_error "Push/Pop not accessing stack memory";
        if not slot_is_full then
          transform_error "Push/Pop not accessing full slot";
        let taint_anno = Option.get taint_anno in
        let taint_anno = tv_ittt taint_anno in (* mocking *)
        let offset_as_disp, sf1 = match taint_anno with
        | TaintConst true -> Some (Isa.ImmNum delta), []
        | TaintConst false -> None, []
        | TaintVar v -> None, [(v, "push/pop with variable taint")] (* soft fault *)
        | TaintExp v_set -> None, TaintExp.TaintVarSet.to_list v_set |> List.map (fun x -> (x, "ld/st with variable taint")) (* soft fault *)
        | TaintUnknown -> transform_error (Printf.sprintf "push/pop encounters unknown taint status: %s" (Isa.string_of_instruction orig));
        in
        let o', sf2 = helper_prepare_memop o in
        let o_size = match o' with
        | RegOp reg -> Isa.get_reg_size reg
        | LdOp (_, _, _, _, w, _)
        | StOp (_, _, _, _, w, _) -> w
        | _ -> transform_error "Operand not supported for Push/Pop simulation"
        in
        let sf = sf1 @ sf2 in
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
          (inst_st, [inst_rsp_sub], [], false), css', sf
        | Pop _ ->
          let inst_ld = Isa.UInst(Isa.Mov, o', Isa.LdOp(offset_as_disp, Some Isa.RSP, None, None, o_size, mem_anno)) in
          let inst_rsp_add = Isa.make_inst_add_i_r o_size Isa.RSP in
          (inst_ld, [], [inst_rsp_add], false), css, sf
        | _ -> transform_error "Expecting only Push/Pop here"
      end
    | Call _ -> (orig, [], [], true), css, [] (* handled in another pass *)
    | RepMovs _ -> transform_error "RepMovs unimplemented"
    | RepLods _ -> transform_error "RepLods unimplemented"
    | RepStos _ -> transform_error "RepStos unimplemented"
    (* | RepStosq -> transform_error "RepStosq unimplemented"
    | RepMovsq -> transform_error "RepStosq unimplemented" *)
    in
    (InstTransform.assign tf inst inst_pre inst_post use_orig_mnemonic), css, sf

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
      (child_fi: FuncInterface.t) (* wanna check taint type of slots the moment the call happens *)
      (anno: CallAnno.t')
      : CallAnno.base_info list =
    let child_rsp_var_id = get_rsp_var_from_reg_type child_fi.in_reg in
    let res = List.fold_left (
      fun (acc: CallAnno.base_info list) (mem_part: CallAnno.slot_t MemType.mem_part) : CallAnno.base_info list ->
        let (child_base, _), slots = mem_part in
        if List.length slots = 0 && child_base <> child_rsp_var_id then transform_error "Empty memory part";
        if List.length slots = 0 then acc else

        (* sample the first slot to get info of base pointer, should be the same accross all slots *)
        let _, _, ((parent_base, _, _), parent_base_info) = (List.hd slots) in
        (* sanity check: all slots with same base in the child maps have same info about base *)
        List.iter (
          fun (_, _, ((parent_base', _, _), parent_base_info')) ->
            if parent_base' != parent_base || CallAnno.cmp_base_info parent_base_info parent_base_info' != 0 then
              transform_error "Different parent base in the same child memory part"
        ) (List.tl slots);

        (* do not transform, if dealing with child's RSP or global base, or parent mem is already unified *)
        if child_base = func_state.init_rsp_var_id ||
           parent_base_info = BaseAsGlobal ||
           get_slot_unity func_state.init_mem_unity parent_base = Unified 
        then acc else
        (* see if all slots are taint true or all slots are taint false *)
        let unity = get_mem_part_unity mem_part (get_child_slot_taint child_fi.in_mem) in
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
              true
          )
      end
    | UInst (Lea, dst, src) -> begin
        match src with
        | MemOp (d, b, i, s) -> Some (
            InstTransform.assign tf
              (UInst (Lea, dst, add_delta_on_disp (d, b, i, s) |> Isa.memop_to_mem_operand))
              []
              []
              true
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
            true
        )
      end

  let get_stack_slot_dyn_offset (rsp_var_id: Isa.imm_var_id) (slot: MemOffset.t) (curr_rsp: SingleExp.t) : int64 =
    let addr_beg, _ = slot in
    (*
    PP.print_lvl 0 "get_stack_slot_dyn_offset\n";
    PP.print_lvl 1 "addr_beg: %s\n" (SingleExp.to_string addr_beg);
    PP.print_lvl 1 "curr_rsp: %s\n" (SingleExp.to_string curr_rsp);
    *)
    let offset1 = SingleExp.match_const_offset addr_beg rsp_var_id |> Option.get in
    let offset2 = SingleExp.match_const_offset curr_rsp rsp_var_id |> Option.get in
    (* curr_rsp has already been subtracted by 8 to push to return address *)
    let offset2 = Int64.add offset2 8L in
    Int64.sub offset1 offset2

  let transform_basic_block_calls
      (func_state: func_state)
      (tv_ittt: tv_ittt)
      (func_interfaces: FuncInterface.t list)
      (bb: basic_block)
      : basic_block * (tv_fault_t list) =
    let insts', sf = List.fold_left (
      fun (acc: (InstTransform.t list) * (tv_fault_t list)) (* processed insts (all previous inst), reversed *)
          (tf: InstTransform.t) (* current inst *) ->
        let acc_tf, acc_sf = acc in
        match tf.orig with
        | Call (child_label, anno) -> begin
            let child_fi = FuncInterface.find_fi func_interfaces child_label |> Option.get in
            let anno = Option.get anno in
            let bases_to_change = extract_bases_to_be_changed func_state child_fi anno in
            let new_prev_tfs, bases_to_change = List.fold_left (
              (* sweep (in reversed order) and find instructions that prepare bases *)
              fun (acc: (InstTransform.t list) * (CallAnno.base_info list)) (tf: InstTransform.t) ->
                let acc_tf, bases_to_change = acc in
                if List.length bases_to_change = 0 then (tf :: acc_tf, []) else
                let result = List.find_map (
                    fun (base_info: CallAnno.base_info) ->
                      match base_info with
                      | BaseAsReg _ (* reg *) -> begin
                          (*
                          let replaced = try_change_delta_of_dest_reg tf reg in
                          match replaced with
                          | Some tf' -> Some (tf', base_info)
                          | None -> None
                          *)
                          None (* do not use this logic for now, since it may modify reg's value before some other instructions use it *)
                        end
                      | BaseAsSlot _ -> None (* we will handle this later, see below *)
                      | BaseAsGlobal -> transform_error "BaseAsGlobal not expected to be in bases_to_change"
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
            let inst_pre, inst_post, sf = List.fold_left (
              fun (acc: (Isa.instruction list * Isa.instruction list * tv_fault_t list)) (base_info: CallAnno.base_info) ->
                let acc_inst_pre, acc_inst_post, acc_sf = acc in
                match base_info with
                | BaseAsReg reg ->
                  if Isa.is_reg_callee_saved reg then
                    transform_error "trying to temporarily transform a callee saved register, unexpceted";
                  (
                    (Isa.make_inst_add_i_r delta reg) :: acc_inst_pre,
                    acc_inst_post (* no need to restore, since the register is caller saved *),
                    acc_sf
                  )
                | BaseAsSlot (ch_base, ch_base_off) ->
                  (* where it is in child's memory slot *)
                  (* sanity check: the slot is a full slot in parent's memory *)
                  let ch_slot_anno = match MemType.get_mem_type_strict anno.ch_mem (ch_base, ch_base_off) with
                  | None -> transform_error "sanity check failed: BaseAsSlot does not describe a full slot in child's memory";
                  | Some x -> x
                  in
                  let (pa_base, pa_base_off, is_full), base_info = ch_slot_anno in
                  if not is_full then
                    transform_error "sanity check failed: BaseAsSlot is not mapped to a full slot of parent";
                  if not (MemOffset.is_8byte_slot pa_base_off) then
                    transform_error "sanity check failed: BaseAsSlot does not access slot of 8 bytes";
                  let off_of_base = SingleExp.match_const_offset (fst ch_base_off) ch_base |> Option.get in
                  let mem_op = match base_info with
                  | BaseAsReg r -> (Some (Isa.ImmNum off_of_base), Some r, None, None)
                  | BaseAsSlot (b, o) ->
                    transform_error (Printf.sprintf
                      "Unexpected recursive base: some base is passed in memory of base %d, which is still in memory of %d (%s)"
                      ch_base
                      b
                      (MemOffset.to_string o)
                    )
                  | BaseAsGlobal ->
                    if pa_base >= Parser.imm_var_unset then
                      transform_error "Invalid var id for global label";
                    (Some (Isa.ImmBExp (Isa.ImmLabel pa_base, Isa.ImmNum off_of_base)), None, None, None)
                  in
                  (* get the memory operand of the slot to be changed *)
                  let mem_op, sf = match get_child_slot_taint child_fi.in_mem ch_base ch_base_off None with
                  | TaintConst true -> begin
                      transform_error "we assert the base is untainted";
                      (* let d, b, i, s = mem_op in     *)
                      (* add_delta_on_disp (d, b, i, s) *)
                    end
                  | TaintConst false -> mem_op, []
                  | TaintVar v -> mem_op, [(func_state.func_name, bb.label, v, "call finds child's slot has variable taint")] (* soft fault *)
                  | TaintExp v_set -> mem_op, v_set |> TaintExp.TaintVarSet.to_list |> List.map (fun v -> func_state.func_name, bb.label, v, "call finds child's slot has variable taint") (* soft fault *)
                  | TaintUnknown -> transform_error "Unexpected unknown taint annotation for BaseAsSlot"
                  in
                  (
                    (Isa.make_inst_add_i_m64 delta mem_op) :: acc_inst_pre,
                    (Isa.make_inst_add_i_m64 (Int64.neg delta) mem_op) :: acc_inst_post,
                    sf @ acc_sf
                  )
                | BaseAsGlobal -> transform_error "BaseAsGlobal not expected to be in bases_to_change"
            ) ([], [], []) bases_to_change in
            (* save/restore active callee-saved registers in caller's scope *)
            let (curr_rsp, curr_rsp_taint) = CallAnno.TaintRegType.get_reg_type anno.pr_reg Isa.RSP in
            let curr_rsp_taint = tv_ittt curr_rsp_taint in (* mocking *)
            if curr_rsp_taint = TaintConst true then
              transform_error "RSP is tainted, not expected";
            let saves, restores = List.split (
              List.filter_map (fun (slot: callee_slot) ->
                let reg, offset = slot in
                let disp = get_stack_slot_dyn_offset func_state.init_rsp_var_id offset curr_rsp in
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
            let tf =
              if List.length sf > 0 then InstTransform.set_failed tf
              else InstTransform.assign tf tf.orig inst_pre inst_post true
            in
            tf :: (List.rev new_prev_tfs), sf @ acc_sf
          end
        | _ -> tf :: acc_tf, acc_sf
    ) ([], []) bb.insts in
    { bb with insts = List.rev insts' }, sf

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
      (tv_ittt: tv_ittt)
      (bb: basic_block)
      (css: callee_slot list)
      : basic_block * (callee_slot list) * (tv_fault_t list) =
    let new_insts, css', sf, _ = List.fold_left (
      fun (acc: (InstTransform.t list) * (callee_slot list) * (tv_fault_t list) * int) (trans: InstTransform.t) ->
        let acc_insts, acc_css, acc_sf, left = acc in

        if left = 0 then acc else (* skip dead instructions *)

        let tf, css, sf = transform_instruction func_state tv_ittt trans acc_css in

        let sf = List.map (fun (v, reason) -> func_state.func_name, bb.label, v, reason) sf in
        let tf = if List.length sf > 0 then InstTransform.set_failed tf else tf in

        tf :: acc_insts, css, sf @ acc_sf, left - 1
    ) ([], css, [], bb.alive_inst_cnt) bb.insts in
    { bb with insts = List.rev new_insts }, css', sf

  (* Step 2 *)
  let get_func_init_mem_unity (ti_state: TaintTypeInfer.t) : mem_unity_t =
    let rsp_var_id = get_func_init_rsp_var ti_state in
    List.map (fun (mem_part: (MemType.entry_t MemType.mem_part)) ->
      let (base_id, _), _ = mem_part in
      if base_id = rsp_var_id then (base_id, Ununified) else
      let unity =
        get_mem_part_unity mem_part (fun _ _ (entry: MemType.entry_t) -> let (_, taint) = entry in taint)
      in
      (base_id, unity)
    ) (List.hd ti_state.func_type).mem_type

  (* Step 3 *)
  let get_func_instantiate_requests
      (ti_state: TaintTypeInfer.t)
      (init_mem_unity: mem_unity_t)
      : TaintExp.taint_var_id list =
    let _, _ = ti_state, init_mem_unity in
    [] (* TODO *)

  let simplify_bb_push_pops
    (_: func_state)
    (bb: basic_block)
    : basic_block =
    let extract_offset_helper (inst: Isa.instruction) =
      match inst with
      | BInst (Add, RegOp reg1, RegOp reg2, ImmOp (ImmNum imm)) when reg1 = reg2 ->
        Some imm
      | _ -> None
    in
    let helper (acc_insts: InstTransform.t list) (tf: InstTransform.t) : InstTransform.t list =
      if acc_insts = [] then [tf] else
      let x = List.hd acc_insts in
      let x, tf = match x.orig, tf.orig with
      | Push _, Push _ ->
        if List.length x.inst_pre != 1 || List.length x.inst_post != 0 ||
           List.length tf.inst_pre != 1 || List.length tf.inst_post != 0 then
          transform_error "unexpected transformed push";
        let acc_offset = extract_offset_helper (List.hd x.inst_pre) |> Option.get in
        let new_offset = extract_offset_helper (List.hd tf.inst_pre) |> Option.get in

        let x_inst = match x.inst with
        | UInst(Mov, StOp(offset_as_disp, Some RSP, None, None, o_size, mem_anno), o') ->
          let disp = match offset_as_disp with
          | None -> acc_offset
          | Some (ImmNum v) -> Int64.add v acc_offset
          | _ -> transform_error "unexpected transformed disp"
          in
          Isa.UInst (Mov, StOp(Some (ImmNum disp), Some RSP, None, None, o_size, mem_anno), o')
        | _ -> transform_error "unexpected transformed push";
        in

        let x = { x with
          inst = x_inst;
          inst_pre = [];
        } in

        let tf = { tf with
          inst_pre = [Isa.make_inst_add_i_r (Int64.add acc_offset new_offset) Isa.RSP]
        } in

        (x, tf)
      | Pop _, Pop _ -> (x, tf)
      | _, _ -> (x, tf)
      in
      tf :: x :: (List.tl acc_insts)
    in
    let new_insts = List.fold_left helper [] bb.insts in
    { bb with insts = List.rev new_insts }

  (* Step 5 *)
  let transform_one_function
      (fi_list: FuncInterface.t list)
      (ti_state: TaintTypeInfer.t)
      (init_mem_unity: mem_unity_t)
      : func_state * (tv_fault_t list) =
    let func_state = init_func_state ti_state init_mem_unity in

    (* mocking tv ittt *)
    (* let ittt_map = get_taint_var_instantiate_map func_state.func_name in *)
    let ittt_map = get_taint_var_instantiate_map ti_state in
    let tv_ittt = taint_var_ittt ittt_map in

    (* basic pass *)
    let bbs, css, sf1 = List.fold_left (
      fun acc (bb: basic_block) ->
        let acc_bbs, acc_css, acc_sf = acc in
        let bb', css, sf = transform_basic_block_basic func_state tv_ittt bb acc_css in
        bb' :: acc_bbs, css, sf @ acc_sf
    ) ([], [], []) func_state.bbs in
    let func_state = {
      func_state with
      bbs = List.rev bbs;
      callee_saving_slots = css;
    }
    in

    (* call handling pass *)
    let sf2, bbs = List.fold_left_map (
      fun acc_sf bb ->
        let bb', sf = transform_basic_block_calls func_state tv_ittt fi_list bb in
        sf @ acc_sf, bb'
    ) [] func_state.bbs in
    let func_state = {
      func_state with
      bbs = bbs;
    }
    in

    (*
    (* simplify push/pop sequence *)
    let bbs = List.map (
      fun bb -> simplify_bb_push_pops func_state bb
    ) func_state.bbs in
    let func_state = {
      func_state with
      bbs = bbs;
    }
    in
    *)

    let sf = sf1 @ sf2 |> List.sort_uniq (fun info1 info2 ->
      let f1, b1, v1, r1 = info1 in
      let f2, b2, v2, r2 = info2 in
      if not (String.equal f1 f2) then String.compare f1 f2 else
      if not (String.equal b1 b2) then String.compare b1 b2 else
      if v1 != v2 then Int.compare v1 v2 else
      String.compare r1 r2
    ) in

    pp_func_state func_state;
    func_state, sf

  (* was use to call transformation on single function benchmark
  let transform_functions
      (fi_list: FuncInterface.t list)
      (taint_infer_states: TaintTypeInfer.t list)
      : func_state list =
    List.map (
      fun taint_infer_state -> transform_one_function fi_list taint_infer_state
    ) taint_infer_states

  *)
end
