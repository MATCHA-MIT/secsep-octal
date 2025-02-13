open Arch_type
open Reg_type
open Basic_type
open Flag_type
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

let rec convert_dep_type
    (ctx: Z3.context)
    (se: SingleEntryType.t)
    (size: int) (* assume that variables involved have consistent size *)
    (top_size: int)
    : DepType.t =
  match se with
  | SingleTop -> DepType.Top top_size
  | SingleConst c -> DepType.Exp (Z3Expr.mk_numeral_int ctx (Int64.to_int c) (Z3.BitVector.mk_sort ctx (size * 8)))
  | SingleVar v -> DepType.Exp (Z3Expr.mk_const_s ctx ("s" ^ (string_of_int v)) (Z3.BitVector.mk_sort ctx (size * 8)))
  | SingleBExp (bop, e1, e2) ->
    let e1 = convert_dep_type ctx e1 size top_size in
    let e2 = convert_dep_type ctx e2 size top_size in
    (
      match bop, e1, e2 with
      | _, DepType.Top sz, _
      | _, _, DepType.Top sz ->
        if sz != size then failwith "Top size mismatched" else
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
  | _ -> failwith "unsupported"
 
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
    (size: int) (* size in bytes for variables involved in DepType *)
    (top_size: int) (* size for top *)
    : BasicType.t =
  let se, te = taint_entry_type in
  (
    convert_dep_type ctx se size top_size,
    convert_taint_type ctx te
  )

let convert_reg_type
    (ctx: Z3.context)
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
      convert_basic_type ctx entry size size
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
    (off: Type.Mem_offset_new.MemOffset.t)
    : MemOffset.t =
  let off_l, off_r = off in
  let size = TaintTypeInfer.Isa.get_gpr_full_size () |> Int64.to_int in
  let off_l' = convert_dep_type ctx off_l size size in
  let off_r' = convert_dep_type ctx off_r size size in
  match off_l', off_r' with
  | Exp e_l, Exp e_r -> (e_l, e_r)
  | _ -> failwith "unexpected Top found in input's MemOffset"

let convert_mem_range
    (ctx: Z3.context)
    (range: Type.Mem_offset_new.MemRange.t)
    : MemRange.t =
  match range with
  | Type.Mem_offset_new.MemRange.RangeConst off_list ->
    List.map (fun off -> convert_mem_offset ctx off) off_list
  | Type.Mem_offset_new.MemRange.RangeVar v ->
    Printf.printf "warn: RangeVar %d detected, converted to empty list\n" v;
    []
  | _ -> failwith "unexpected variable MemRange" (* TODO: is variable expected after inference? *)

let convert_mem_type
    (ctx: Z3.context)
    (mem_type: TaintTypeInfer.ArchType.MemType.t)
    : MemType.t =
  List.map (fun (mem_part: TaintEntryType.t TaintTypeInfer.ArchType.MemType.mem_part) ->
    let ptr_info, mem_slots = mem_part in
    let mem_slots' = List.map (fun (mem_slot: TaintEntryType.t TaintTypeInfer.ArchType.MemType.mem_slot) ->
      let off, range, entry = mem_slot in
      let off' = convert_mem_offset ctx off in
      let range' = convert_mem_range ctx range in
      let entry' = match Type.Mem_offset_new.MemOffset.get_size off with
      | None -> (* non-const size slot *)
        convert_basic_type ctx (SingleEntryType.SingleTop, snd entry) DepType.top_unknown_size DepType.top_unknown_size
      | Some size when size > 8L -> (* slot bigger than 8 bytes *)
        convert_basic_type ctx (SingleEntryType.SingleTop, snd entry) DepType.top_unknown_size DepType.top_unknown_size
      | Some size ->
        convert_basic_type ctx entry (Int64.to_int size) DepType.top_unknown_size
      in
      (off', range', entry')
    ) mem_slots
    in
    (ptr_info, mem_slots')
  ) mem_type

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

let convert_arch_type
    (ctx: Z3.context)
    (tti: TaintTypeInfer.t)
    (arch_type: TaintTypeInfer.ArchType.t)
    (stack_spill_info: StackSpillInfo.t)
    (input_var: TaintExp.TaintVarSet.t)
    : ArchType.t =
  {
    label = arch_type.label;
    pc = arch_type.pc;
    dead_pc = arch_type.dead_pc;
    reg_type = convert_reg_type ctx arch_type.reg_type;
    flag_type = convert_flag_type ctx;
    mem_type = convert_mem_type ctx arch_type.mem_type;
    context = convert_context ctx arch_type.context tti.taint_context tti.taint_sol;
    stack_spill_info = stack_spill_info;

    global_var = arch_type.global_var;
    input_var = input_var;
    local_var = TaintTypeInfer.ArchType.get_local_var_set arch_type;
  }

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

let convert_function_interface
    (ctx: Z3.context)
    (fi: TaintTypeInfer.FuncInterface.t)
    : FuncInterface.t =
  let in_arch: ArchType.t = {
    label = fi.func_name;
    reg_type = convert_reg_type ctx fi.in_reg;
    flag_type = convert_flag_type ctx;
    mem_type = convert_mem_type ctx fi.in_mem;
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
    reg_type = convert_reg_type ctx fi.out_reg;
    flag_type = convert_flag_type ctx;
    mem_type = convert_mem_type ctx fi.out_mem;
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
  }

let convert_taint_type_infer
    (ctx: Z3.context)
    (tti: TaintTypeInfer.t)
    : ArchType.t list =
  (* currently we only convert arch types of TTI *)
  let stack_spill_info = convert_stack_spill_info tti in
  let input_var = convert_input_var tti in
  let archs = List.map (
    fun arch_type -> convert_arch_type ctx tti arch_type stack_spill_info input_var
  ) tti.func_type
  in
  archs
