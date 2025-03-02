open Arch_type
open Reg_type
open Basic_type
open Flag_type
open Mem_anno
open Mem_type
open Stack_spill_info
open Func_interface
open Type.Taint_type_infer
open Type.Single_entry_type
open Type.Taint_entry_type
open Type.Taint_exp
open Type.Single_context
open Type.Set_sexp
open Z3_sexp
open Branch_anno
open Call_anno
open Sexplib.Std

(* map variables at start of each BB to their size *)
type bb_var_size_map = (string * ((int * int) list)) list
[@@deriving sexp]

type checker_func = (* for each function ('s taint type infer) *)
    ArchType.Isa.label *              (* function name *)
    (ArchType.Isa.basic_block list) * (* basic blocks *)
    (ArchType.t list) *               (* basic blocks' arch type *)
    bb_var_size_map                   (* variable size in each basic block *)
[@@deriving sexp]

let get_size_of_var
    (sz_map: (int * int) list)
    (var_id: int)
    : int =
  List.find_map (
    fun (var_id', size) ->
      if var_id = var_id' then Some size else None
  ) sz_map |> Option.get

let get_size_finder_of_bb
    (bb_vsm: bb_var_size_map)
    (block: string) : (int -> int) =
  let sz_map = List.find_map (
    fun (block', sz_map) ->
      if block = block' then Some sz_map else None
  ) bb_vsm |> Option.get
  in
  get_size_of_var sz_map

let intermediate_sz = 64

let rec convert_dep_type_inner
    (ctx: Z3.context)
    (se: SingleEntryType.t)
    (get_var_size: int -> int)
    : DepType.t =
  let raw_res = match se with
  | SingleTop -> DepType.Top intermediate_sz
  | SingleConst c -> DepType.Exp (Z3Expr.mk_numeral_int ctx (Int64.to_int c) (Z3.BitVector.mk_sort ctx intermediate_sz))
  | SingleVar v ->
    let var_size = get_var_size v in
    DepType.Exp (Z3Expr.mk_const_s ctx ("s" ^ (string_of_int v)) (Z3.BitVector.mk_sort ctx (var_size * 8)))
  | SingleBExp (bop, e1, e2) ->
    let e1 = convert_dep_type_inner ctx e1 get_var_size in
    let e2 = convert_dep_type_inner ctx e2 get_var_size in
    (
      match bop, e1, e2 with
      | _, DepType.Top sz1, DepType.Top sz2 ->
        if sz1 != sz2 || sz1 != intermediate_sz then failwith "Top size mismatched" else
        DepType.Top sz1
      | _, DepType.Top sz, _
      | _, _, DepType.Top sz ->
        if sz != intermediate_sz then failwith "Top size mismatched" else
        DepType.Top sz
      | _, DepType.Exp e1', DepType.Exp e2' -> begin
          let e = match bop with
          | SingleAdd -> Z3.BitVector.mk_add ctx e1' e2'
          | SingleSub -> Z3.BitVector.mk_sub ctx e1' e2'
          | SingleMul -> Z3.BitVector.mk_mul ctx e1' e2'
          | SingleSal -> Z3.BitVector.mk_shl ctx e1' e2'
          | SingleSar -> Z3.BitVector.mk_ashr ctx e1' e2'
          | SingleXor -> Z3.BitVector.mk_xor ctx e1' e2'
          | SingleAnd -> Z3.BitVector.mk_and ctx e1' e2'
          | SingleOr ->  Z3.BitVector.mk_or ctx e1' e2'
          | SingleMod -> Z3.BitVector.mk_smod ctx e1' e2'
          in
          DepType.Exp e
        end
    )
  | SingleUExp (uop, e) ->
    let e = convert_dep_type_inner ctx e get_var_size in
    match uop, e with
    | _, DepType.Top sz ->
      if sz != intermediate_sz then failwith "Top size mismatched" else
      DepType.Top sz
    | _, DepType.Exp e' -> begin
        let out_e = match uop with
        | SingleNot -> Z3.BitVector.mk_not ctx e'
        in
        DepType.Exp out_e
      end
  in
  match raw_res with
  | DepType.Top sz ->
    if sz > intermediate_sz then failwith "Top size exceeds intermediate size" else
    DepType.Top intermediate_sz
  | DepType.Exp e ->
    let e_size = Z3.BitVector.get_size (Z3.Expr.get_sort e) in
    if e_size = intermediate_sz then
      DepType.Exp e
    else if e_size < intermediate_sz then
      DepType.Exp (Z3.BitVector.mk_sign_ext ctx (intermediate_sz - e_size) e)
    else 
      failwith "Exp size exceeds intermediate size"


let convert_dep_type
    (ctx: Z3.context)
    (se: SingleEntryType.t)
    (get_var_size: int -> int)
    (out_size: int)
    : DepType.t =
  match convert_dep_type_inner ctx se get_var_size with
  | DepType.Top _ -> DepType.Top out_size
  | DepType.Exp e ->
    let e_size = Z3.BitVector.get_size (Z3.Expr.get_sort e) in
    if e_size = out_size then
      DepType.Exp e
    else if e_size > out_size then
      DepType.Exp (Z3.BitVector.mk_extract ctx (out_size - 1) 0 e)
    else
      DepType.Exp (Z3.BitVector.mk_sign_ext ctx (out_size - e_size) e)

 
let convert_taint_type
    (ctx: Z3.context)
    (te: TaintExp.t)
    : TaintType.t =
  let var_name (v: TaintExp.taint_var_id) : string =
    "t" ^ (string_of_int v)
  in
  match te with
  | TaintConst true -> Z3.Boolean.mk_true ctx
  | TaintConst false -> Z3.Boolean.mk_false ctx
  | TaintVar v -> Z3.Boolean.mk_const_s ctx (var_name v)
  | TaintExp v_set -> begin
      let v_list = TaintExp.TaintVarSet.elements v_set in
      let t_list = List.map (fun v -> Z3.Boolean.mk_const_s ctx (var_name v)) v_list in
      Z3.Boolean.mk_or ctx t_list
    end
  | TaintUnknown ->
    Printf.printf "warn: TaintUnknown detected, converted to t_unknown\n";
    Z3.Boolean.mk_const_s ctx "t_unknown"

let convert_basic_type
    (ctx: Z3.context)
    (taint_entry_type: TaintEntryType.t)
    (get_var_size: int -> int)
    (size: int)
    : BasicType.t =
  let se, te = taint_entry_type in
  (
    convert_dep_type ctx se get_var_size size,
    convert_taint_type ctx te
  )

let convert_reg_type
    (ctx: Z3.context)
    (get_var_size: int -> int)
    (reg_type: TaintTypeInfer.ArchType.RegType.t)
    : RegType.t =
  if TaintTypeInfer.ArchType.Isa.total_reg_num != List.length reg_type then
    failwith "source reg_type's length is unexpected"
  else
    List.mapi (fun idx entry ->
      let size = idx
        |> TaintTypeInfer.ArchType.Isa.get_full_reg_by_idx
        |> TaintTypeInfer.ArchType.Isa.get_reg_size
        |> Int64.to_int
      in
      convert_basic_type ctx entry get_var_size size
    ) reg_type

let convert_flag_type
    (ctx: Z3.context)
    : FlagType.t =
  List.init TaintTypeInfer.ArchType.Isa.total_flag_num (fun _ ->
    DepType.get_top_flag (),
    Z3.Boolean.mk_const_s ctx "t_unknown"
  )

let convert_mem_offset
    (ctx: Z3.context)
    (get_var_size: int -> int)
    (off: Type.Mem_offset_new.MemOffset.t)
    : MemOffset.t =
  let off_l, off_r = off in
  let size = TaintTypeInfer.Isa.get_gpr_full_size () |> Int64.to_int in
  let off_l' = convert_dep_type ctx off_l get_var_size size in
  let off_r' = convert_dep_type ctx off_r get_var_size size in
  match off_l', off_r' with
  | Exp e_l, Exp e_r -> (e_l, e_r)
  | _ -> failwith "unexpected Top found in input's MemOffset"

let convert_mem_range
    (ctx: Z3.context)
    (get_var_size: int -> int)
    (range: Type.Mem_offset_new.MemRange.t)
    : MemRange.t * (int64 list) = (* empty list is a hint that the slot's DepType can be treated as Top *)
  match range with
  | Type.Mem_offset_new.MemRange.RangeConst off_list ->
    off_list
    |> List.map (fun off ->
      let off' = convert_mem_offset ctx get_var_size off in
      let size = Type.Mem_offset_new.MemOffset.get_size off |> Option.get in
      (off', size)
    )
    |> List.split
  | _ ->
    Printf.printf "warn: %s converted to empty list\n" (Type.Mem_offset_new.MemRange.to_string range);
    ([], [])

let convert_mem_generic
    (ctx: Z3.context)
    (get_var_size: int -> int)
    (mem_type: 'a Type.Mem_type_new.MemTypeBasic.mem_content)
    (convert_entry: Z3.context -> Type.Mem_offset_new.MemOffset.t -> Type.Mem_offset_new.MemRange.t -> 'a -> int64 list -> 'b)
    : 'b MemType.mem_content =
  List.map (fun (mem_part: 'a Type.Mem_type_new.MemTypeBasic.mem_part) ->
    let ptr_info, mem_slots = mem_part in
    let mem_slots' = List.map (fun (mem_slot: 'a Type.Mem_type_new.MemTypeBasic.mem_slot) ->
      let off, range, entry = mem_slot in
      let off' = convert_mem_offset ctx get_var_size off in
      let range', inited_range_sizes = convert_mem_range ctx get_var_size range in
      let entry' = convert_entry ctx off range entry inited_range_sizes in
      (off', range', entry')
    ) mem_slots
    in
    (ptr_info, mem_slots')
  ) mem_type

let convert_mem_type
    (ctx: Z3.context)
    (get_var_size: int -> int)
    (mem_type: TaintTypeInfer.ArchType.MemType.t)
    : MemType.t =
  let convert_entry ctx off _ entry inited_range_sizes =
      let default_top_type = (* DepType is top, TaintType is init accordingly *)
        convert_basic_type ctx (SingleEntryType.SingleTop, snd entry) (fun _ -> failwith "placeholder") DepType.top_unknown_size
      in
      match inited_range_sizes, Type.Mem_offset_new.MemOffset.get_size off with
      | [], _ -> default_top_type
      | [_], None -> default_top_type (* non-const size slot *)
      | [_], Some slot_size when slot_size > 8L -> default_top_type (* slot bigger than 8 bytes *)
      | [inited_size], Some _ ->
        convert_basic_type ctx entry get_var_size (Int64.to_int inited_size)
      | _ -> default_top_type (* more than one inited range *)
  in
  convert_mem_generic ctx get_var_size mem_type convert_entry

let convert_context
    (ctx: Z3.context)
    (single_context: SingleContext.t list)
    (taint_context: TaintExp.sub_t list)
    (taint_sol: TaintExp.local_var_map_t)
    : BasicType.ctx_t =
  let dep_ctx = List.map (
    fun entry -> SingleContext.to_smt_expr (ctx, Z3.Solver.mk_simple_solver ctx (* placeholder *)) entry
  ) single_context
  in
  let taint_ctx = List.map (
    fun (sub, sup) ->
      let sub = TaintExp.repl_context_var_no_error taint_sol sub in
      let sup = TaintExp.repl_context_var_no_error taint_sol sup in
      let sub' = convert_taint_type ctx sub in
      let sup' = convert_taint_type ctx sup in
      Z3.Boolean.mk_implies ctx sub' sup'
  ) taint_context
  in
  (dep_ctx, taint_ctx)

let infer_var_size_map
    (ctx: Z3.context)
    (reg_type: TaintTypeInfer.ArchType.RegType.t)
    (mem_type: TaintTypeInfer.ArchType.MemType.t)
    : (int * int) list =
  let res, _ = List.fold_left (
    fun (acc, id) (entry: TaintEntryType.t) ->
      let se, _ = entry in
      let reg = TaintTypeInfer.Isa.get_full_reg_by_idx id in
      match se with
      | SingleEntryType.SingleVar v ->
        let size = TaintTypeInfer.Isa.get_reg_size reg |> Int64.to_int in
        (v, size) :: acc, id + 1
      | SingleEntryType.SingleBExp _ | SingleEntryType.SingleUExp _ ->
        failwith "expecting var/const/top for reg at the start of BB"
      | _ -> acc, id + 1
  ) ([], 0) reg_type
  in
  let res = TaintTypeInfer.ArchType.MemType.fold_left_full (
    fun acc (_, range, entry) ->
      let se, _ = entry in
      match se with
      | SingleEntryType.SingleVar v -> begin
          let _, inited_range_sizes = convert_mem_range
            ctx
            (fun _ -> TaintTypeInfer.Isa.get_gpr_full_size () |> Int64.to_int) (* placeholder *)
            range 
          in
          match inited_range_sizes with
          | [] -> failwith "uninited mem slot should be SingleTop rather than SingleVar"
          | [sz] -> (v, Int64.to_int sz) :: acc
          | _ -> failwith "more than one inited range for SingleVar"
        end
      | SingleEntryType.SingleBExp _ | SingleEntryType.SingleUExp _ ->
        failwith "expecting var/const/top for mem slot at the start of BB"
      | _ -> acc
  ) res mem_type
  in
  res

let convert_arch_type
    (ctx: Z3.context)
    (tti: TaintTypeInfer.t)
    (arch_type: TaintTypeInfer.ArchType.t)
    (stack_spill_info: StackSpillInfo.t)
    (input_var: TaintExp.TaintVarSet.t)
    : ArchType.t * (string * ((int * int) list)) =
  let var_size_map = infer_var_size_map ctx arch_type.reg_type arch_type.mem_type in
  let get_var_size = get_size_of_var var_size_map in
  {
    label = arch_type.label;
    pc = arch_type.pc;
    dead_pc = arch_type.dead_pc;
    reg_type = convert_reg_type ctx get_var_size arch_type.reg_type;
    flag_type = convert_flag_type ctx;
    mem_type = convert_mem_type ctx get_var_size arch_type.mem_type;
    context = convert_context ctx arch_type.context tti.taint_context tti.taint_sol;
    stack_spill_info = stack_spill_info;

    global_var = arch_type.global_var;
    input_var = input_var;
    local_var = TaintTypeInfer.ArchType.get_local_var_set arch_type;
  }, (arch_type.label, var_size_map)

let convert_stack_spill_info
    (tti: TaintTypeInfer.t)
    : StackSpillInfo.t =
  (* imm_var_id of RSP upon function entry *)
  let rsp_ptr_idx = TaintTypeInfer.ArchType.Isa.rsp_idx in
  (* sanity check *)
  let input_arch = List.hd tti.func_type in
  let rsp_type, _ = TaintTypeInfer.ArchType.RegType.get_reg_type input_arch.reg_type RSP in
  let _ = match rsp_type with
  | SingleEntryType.SingleVar rsp_var when rsp_var = rsp_ptr_idx -> ()
  | _ -> failwith "rsp type is not SingleVar rsp_ptr_idx"
  in
 
  let rsp_mem = TaintTypeInfer.ArchType.MemType.get_part_mem input_arch.mem_type rsp_ptr_idx in
  let idx_list = List.map (
    fun (spill_slot_pr: int64 * int64) ->
      let l, r = spill_slot_pr in
      List.find_mapi (
        fun idx (off, _, _) ->
          let off_l, off_r = Type.Stack_spill_info.StackSpillInfo.to_off_pair
            rsp_ptr_idx off
          in
          if off_l = l && off_r = r then Some idx else None
      ) (snd rsp_mem)
      |> Option.get
  ) (Int64PairSet.to_list tti.stack_spill_info)
  in
  IntSet.of_list idx_list

let convert_input_var
    (tti: TaintTypeInfer.t)
    : TaintExp.TaintVarSet.t =
  let input_arch = List.hd tti.func_type in
  let merge_helper =
    fun acc (_, entry) -> TaintExp.TaintVarSet.union acc (TaintExp.get_var_set entry)
  in
  let input_var =
    List.fold_left merge_helper TaintExp.TaintVarSet.empty input_arch.reg_type
  in
  let input_var = 
    TaintTypeInfer.ArchType.MemType.fold_left merge_helper input_var input_arch.mem_type
  in
  input_var

let convert_slot
    (ref_mem: TaintEntryType.t TaintTypeInfer.ArchType.MemType.mem_content)
    (ptr: Type.Isa_basic.IsaBasic.imm_var_id)
    (off: Type.Mem_offset_new.MemOffset.t)
    (opt_full: bool option)
    (opt_num: int option)
    : MemAnno.slot_t =
  let part = TaintTypeInfer.ArchType.MemType.get_part_mem ref_mem ptr in
  let idx = List.find_index (
    fun (slot: TaintEntryType.t TaintTypeInfer.ArchType.MemType.mem_slot) ->
      let off', _, _ = slot in
      Type.Mem_offset_new.MemOffset.cmp off off' = 0
  ) (snd part) |> Option.get
  in
  let full = match opt_full with
  | Some b -> b
  | None -> true
  in
  let num = match opt_num with
  | Some x -> x
  | None -> 1
  in
  (ptr, idx, full, num) (* based is passed by a single full slot *)

let convert_base_info
    (ctx: Z3.context)
    (ref_mem: TaintEntryType.t TaintTypeInfer.ArchType.MemType.mem_content)
    (base_info: Type.Call_anno.CallAnno.base_info Type.Mem_type_new.MemTypeBasic.mem_content)
    (get_var_size: int -> int)
    : FuncInterface.base_info MemType.mem_content =
  let convert_entry _ _ _ entry _ =
    match entry with
    | Type.Call_anno.CallAnno.BaseAsReg r -> FuncInterface.BaseAsReg r
    | Type.Call_anno.CallAnno.BaseAsSlot (ptr, off) -> FuncInterface.BaseAsSlot (convert_slot ref_mem ptr off (Some true) (Some 1))
    | Type.Call_anno.CallAnno.BaseAsGlobal -> FuncInterface.BaseAsGlobal
  in
  convert_mem_generic ctx get_var_size base_info convert_entry

let convert_function_interface
    (ctx: Z3.context)
    (fi: TaintTypeInfer.FuncInterface.t)
    : FuncInterface.t =
  let var_size_map = infer_var_size_map ctx fi.in_reg fi.in_mem in
  let get_var_size_func = get_size_of_var var_size_map in
  let in_arch: ArchType.t = {
    label = fi.func_name;
    reg_type = convert_reg_type ctx get_var_size_func fi.in_reg;
    flag_type = convert_flag_type ctx;
    mem_type = convert_mem_type ctx get_var_size_func fi.in_mem;
    context = convert_context ctx fi.in_context fi.in_taint_context [] (* taint solution has been substituted *);

    (* ignored fields *)
    pc = -1; 
    dead_pc = -1;
    stack_spill_info = IntSet.empty;
    global_var = IntSet.empty;
    input_var = TaintExp.TaintVarSet.empty;
    local_var = TaintExp.TaintVarSet.empty;
  }
  in
  let out_arch: ArchType.t = {
    label = fi.func_name;
    reg_type = convert_reg_type ctx get_var_size_func fi.out_reg;
    flag_type = convert_flag_type ctx;
    mem_type = convert_mem_type ctx get_var_size_func fi.out_mem;
    context = convert_context ctx fi.out_context [] (* no out taint context *) [] (* taint solution has been substituted *);

    (* ignored fields *)
    pc = -1; 
    dead_pc = -1;
    stack_spill_info = IntSet.empty;
    global_var = IntSet.empty;
    input_var = TaintExp.TaintVarSet.empty;
    local_var = TaintExp.TaintVarSet.empty;
  }
  in
  {
    func_name = fi.func_name;
    in_type = in_arch;
    out_type = out_arch;
    base_info = convert_base_info ctx fi.in_mem fi.base_info get_var_size_func;
  }

let convert_single_var_map
    (ctx: Z3.context)
    (single_var_map: SingleEntryType.local_var_map_t)
    (from_get_var_size: int -> int)
    (targ_get_var_size: int -> int)
    : DepType.map_t =
  List.map (
    fun (targ_var, from_exp) ->
      let targ_var_size = targ_get_var_size targ_var in
      let from_exp' = convert_dep_type ctx from_exp from_get_var_size targ_var_size in
      (targ_var, from_exp')
  ) single_var_map

let convert_taint_var_map
    (ctx: Z3.context)
    (taint_var_map: TaintExp.local_var_map_t)
    : TaintType.map_t =
  List.map (
    fun (targ_var, from_exp) ->
      let from_exp' = convert_taint_type ctx from_exp in
      (targ_var, from_exp')
  ) taint_var_map

let convert_branch_anno
    (ctx: Z3.context)
    (anno: Type.Branch_anno.BranchAnno.t)
    (from_get_var_size: int -> int)
    (targ_get_var_size: int -> int)
    : BranchAnno.t =
  let anno = Option.get anno in
  let dep_map = convert_single_var_map ctx anno from_get_var_size targ_get_var_size in
  dep_map, [(* taint_map: empty, since the same (input) taint vars are used in both from and targ *)]

let convert_call_anno
    (ctx: Z3.context)
    (anno: Type.Call_anno.CallAnno.t)
    (from_mem: TaintTypeInfer.ArchType.MemType.t)
    (from_get_var_size: int -> int)
    (targ_get_var_size: int -> int)
    : CallAnno.t =
  let anno = Option.get anno in

  let pr_reg = List.mapi (
    fun reg_id (se, te) ->
      let reg_size = reg_id
        |> TaintTypeInfer.Isa.get_full_reg_by_idx
        |> TaintTypeInfer.Isa.get_reg_size
        |> Int64.to_int
      in
      let se' = convert_dep_type ctx se from_get_var_size reg_size in
      let te' = convert_taint_type ctx te in
      (se', te')
  ) anno.pr_reg in

  let convert_ch_mem_slot_info _ _ _ entry _ =
    let (slot_base, slot_off, slot_full), _ (* base *) = entry in
    (* reference memory is caller's memory *)
    convert_slot from_mem slot_base slot_off (Some slot_full) (Some 1)
  in
  let ch_mem_slot_info =
    convert_mem_generic ctx targ_get_var_size anno.ch_mem convert_ch_mem_slot_info
  in

  {
    pr_reg = pr_reg;
    ctx_map = (
      convert_single_var_map ctx anno.single_var_map from_get_var_size targ_get_var_size,
      convert_taint_var_map ctx anno.taint_var_map
    );
    mem_map = ch_mem_slot_info;
  }

let convert_mem_anno
    (ctx: Z3.context)
    (ref_mem: TaintEntryType.t TaintTypeInfer.ArchType.MemType.mem_content)
    (mem_anno: Type.Full_mem_anno.FullMemAnno.t)
    : MemAnno.t =
  let slot_info, taint_info = mem_anno in
  let slot_info = Option.get slot_info in
  let taint_info = Option.get taint_info in

  let slot_base, slot_off, slot_full, slot_num = slot_info in
  let slot = convert_slot ref_mem slot_base slot_off (Some slot_full) (Some slot_num) in

  let taint = convert_taint_type ctx taint_info in

  (slot, taint)

let convert_isa_operand
    (ctx: Z3.context)
    (ref_mem: TaintEntryType.t TaintTypeInfer.ArchType.MemType.mem_content)
    (opr: TaintTypeInfer.Isa.operand)
    : ArchType.Isa.operand =
  match opr with
  | ImmOp i -> ArchType.Isa.ImmOp i
  | RegOp r -> ArchType.Isa.RegOp r
  | RegMultOp r_list -> ArchType.Isa.RegMultOp r_list
  | MemOp (disp, base, index, scale, data_size) -> ArchType.Isa.MemOp (disp, base, index, scale, data_size)
  | LdOp (disp, base, index, scale, data_size, mem_anno) ->
    ArchType.Isa.LdOp (disp, base, index, scale, data_size, convert_mem_anno ctx ref_mem mem_anno)
  | StOp (disp, base, index, scale, data_size, mem_anno) ->
    ArchType.Isa.StOp (disp, base, index, scale, data_size, convert_mem_anno ctx ref_mem mem_anno)
  | LabelOp l -> ArchType.Isa.LabelOp l

let convert_taint_type_infer
    (ctx: Z3.context)
    (tti: TaintTypeInfer.t)
    : checker_func =
  let stack_spill_info = convert_stack_spill_info tti in
  let input_var = convert_input_var tti in
  let bb_vsm, archs = List.fold_left_map (
    fun acc arch_type ->
      let converted, bb_vsm_entry = convert_arch_type ctx tti arch_type stack_spill_info input_var in
      bb_vsm_entry :: acc, converted
  ) [] tti.func_type
  in
  let bbs = List.map2 (
    fun (bb: TaintTypeInfer.Isa.basic_block) (bb_type: TaintTypeInfer.ArchType.t) : ArchType.Isa.basic_block ->
      let f = convert_isa_operand ctx bb_type.mem_type in
      let g = convert_mem_anno ctx bb_type.mem_type in
      let inst' = List.map (fun (inst : TaintTypeInfer.Isa.instruction) ->
        match inst with
        | BInst (bop, od, o1, o2) -> ArchType.Isa.BInst (bop, f od, f o1, f o2)
        | UInst (uop, od, o1) -> ArchType.Isa.UInst (uop, f od, f o1)
        | TInst (top, od, o_list) -> ArchType.Isa.TInst (top, f od, List.map f o_list)
        | Xchg (o1, o2, o3, o4) -> ArchType.Isa.Xchg (f o1, f o2, f o3, f o4)
        | Cmp (o1, o2) -> ArchType.Isa.Cmp (f o1, f o2)
        | Test (o1, o2) -> ArchType.Isa.Test (f o1, f o2)
        | Push (o, mem_anno) -> ArchType.Isa.Push (f o, g mem_anno)
        | Pop (o, mem_anno) -> ArchType.Isa.Pop (f o, g mem_anno)
        | RepMovs (sz, mem_anno1, mem_anno2) -> ArchType.Isa.RepMovs (sz, g mem_anno1, g mem_anno2)
        | RepLods (sz, mem_anno) -> ArchType.Isa.RepLods (sz, g mem_anno)
        | RepStos (sz, mem_anno) -> ArchType.Isa.RepStos (sz, g mem_anno)
        | Jmp (targ, branch_anno) ->
          let from_get_var_size = get_size_finder_of_bb bb_vsm bb.label in
          let targ_get_var_size = get_size_finder_of_bb bb_vsm targ in
          ArchType.Isa.Jmp (targ, convert_branch_anno ctx branch_anno from_get_var_size targ_get_var_size)
        | Jcond (cond, targ, branch_anno) ->
          let from_get_var_size = get_size_finder_of_bb bb_vsm bb.label in
          let targ_get_var_size = get_size_finder_of_bb bb_vsm targ in
          ArchType.Isa.Jcond (cond, targ, convert_branch_anno ctx branch_anno from_get_var_size targ_get_var_size)
        | Call (targ, call_anno) ->
          let from_get_var_size = get_size_finder_of_bb bb_vsm bb.label in
          let targ_get_var_size = get_size_finder_of_bb bb_vsm targ in
          ArchType.Isa.Call (targ, convert_call_anno ctx call_anno bb_type.mem_type from_get_var_size targ_get_var_size)
        | Nop -> ArchType.Isa.Nop
        | Syscall -> ArchType.Isa.Syscall
        | Hlt -> ArchType.Isa.Hlt
        | Directive s -> ArchType.Isa.Directive s
      ) bb.insts in
      {
        label = bb.label;
        insts = inst';
        mnemonics = bb.mnemonics;
        orig_asm = bb.orig_asm;
      }
  ) tti.func tti.func_type
  in
  (tti.func_name, bbs, archs, bb_vsm)

let converted_to_file (filename: string) (cf_list: checker_func list) : unit =
  let open Sexplib in
  let channel = open_out filename in
  Sexp.output_hum channel (Std.sexp_of_list sexp_of_checker_func cf_list)
