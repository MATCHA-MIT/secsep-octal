open Arch_type
open Reg_type
open Basic_type
open Flag_type
open Mem_anno
open Mem_type
(* open Stack_spill_info *)
open Func_interface
open Type.Taint_type_infer
open Type.Single_entry_type
open Type.Taint_entry_type
open Type.Taint_exp
open Type.Single_context
open Type.Set_sexp
open Type.Smt_emitter
open Branch_anno
open Call_anno
open Sexplib.Std

exception ConvertError of string
let convert_error msg = raise (ConvertError ("[Convert Error] " ^ msg))

(* map variables in each function to their size *)
type func_var_size_map = (string * ((int * int) list)) list
[@@deriving sexp]

type checker_func = (* for each function ('s taint type infer) *)
    ArchType.Isa.label *              (* function name *)
    (ArchType.Isa.basic_block list) * (* basic blocks *)
    (ArchType.t list) *               (* basic blocks' arch type *)
    func_var_size_map                 (* variable size in each function *)
[@@deriving sexp]

let get_size_of_var
    (sz_map: (int * int) list)
    (var_id: int)
    : int option =
  let matched = List.filter_map (
    fun (var_id', size) ->
      if var_id = var_id' then Some size else None
  ) sz_map
  in
  match matched with
  | [] -> None
  | [sz] -> Some sz
  | sz_hd :: sz_tl ->
    if List.for_all ((=) sz_hd) sz_tl then
      Some sz_hd
    else
      failwith (Printf.sprintf "var %d has multiple sizes in size map" var_id)

let get_size_finder_of_func
    (func_vsm: func_var_size_map)
    (func_name: string) : (int -> int option) =
  let sz_map = List.find_map (
    fun (func_name', sz_map) ->
      if func_name = func_name' then Some sz_map else None
  ) func_vsm |> Option.get
  in
  get_size_of_var sz_map

let append_func_var_size_map
    (func_vsm: func_var_size_map)
    (func_name: string)
    (sz_map: (int * int) list)
    : func_var_size_map =
  let pr_cmp = fun (v1, s1) (v2, s2) -> if v1 != v2 then Int.compare v1 v2 else Int.compare s1 s2 in
  let sz_map = List.sort_uniq pr_cmp sz_map in
  let found, result = List.fold_left_map (
    fun found (func_name', sz_map') ->
      if func_name = func_name' then
        let new_sz_map = List.fold_left (
          fun curr_map (v, s) ->
            match List.find_opt (fun (v', _) -> v = v') curr_map with
            | None -> (v, s) :: curr_map
            | Some (_, s') ->
              if s = s' then
                curr_map
              else
                failwith (Printf.sprintf "var %d has multiple sizes in size map" v)
        ) sz_map' sz_map in
        let new_sz_map = List.sort pr_cmp new_sz_map in
        (true, (func_name', new_sz_map))
      else
        (found, (func_name', sz_map'))
  ) false func_vsm in
  match found with
  | true -> result
  | false -> (func_name, sz_map) :: func_vsm

let intermediate_sz = 64
let () = assert (intermediate_sz mod 8 = 0)

let rec convert_dep_type_inner
    (ctx: Z3.context)
    (se: SingleEntryType.t)
    (get_var_size: int -> int option)
    : DepType.t =
  let check_top_sz (e: DepType.t) = begin
    match e with
    | Top sz ->
      if sz != intermediate_sz then failwith "Top size mismatched";
    | Exp _ -> ()
  end in
  let raw_res = match se with
  | SingleTop -> DepType.Top intermediate_sz
  | SingleConst c -> DepType.Exp (Z3.BitVector.mk_numeral ctx (Int64.to_string c) intermediate_sz)
  | SingleVar v ->
    let var_size = get_var_size v |> Option.get in
    if var_size = -1 then
      (* this var is designated to be replaced by Top after conversion *)
      DepType.Top DepType.top_unknown_size
    else
      DepType.Exp (Z3.Expr.mk_const_s ctx ("s" ^ (string_of_int v)) (Z3.BitVector.mk_sort ctx (var_size * 8)))
  | SingleBExp (bop, e1, e2) ->
    let e1 = convert_dep_type_inner ctx e1 get_var_size in
    let e2 = convert_dep_type_inner ctx e2 get_var_size in
    (
      match bop, e1, e2 with
      | _, DepType.Exp e1', DepType.Exp e2' -> begin
          let e = match bop with
          | SingleAdd -> Z3.BitVector.mk_add ctx e1' e2'
          | SingleSub -> Z3.BitVector.mk_sub ctx e1' e2'
          | SingleMul -> Z3.BitVector.mk_mul ctx e1' e2'
          | SingleSal -> Z3.BitVector.mk_shl ctx e1' e2'
          | SingleSar -> Z3.BitVector.mk_ashr ctx e1' e2'
          | SingleShr -> Z3.BitVector.mk_lshr ctx e1' e2'
          | SingleXor -> Z3.BitVector.mk_xor ctx e1' e2'
          | SingleAnd -> Z3.BitVector.mk_and ctx e1' e2'
          | SingleOr ->  Z3.BitVector.mk_or ctx e1' e2'
          | SingleMod -> Z3.BitVector.mk_smod ctx e1' e2'
          in
          DepType.Exp e
        end
      | _, _, _ -> begin
          check_top_sz e1;
          check_top_sz e2;
          DepType.Top intermediate_sz
        end
    )
  | SingleUExp (uop, e) -> begin
      let e = convert_dep_type_inner ctx e get_var_size in
      match uop, e with
      | _, DepType.Exp e' -> begin
          let out_e = match uop with
          | SingleNot -> Z3.BitVector.mk_not ctx e'
          in
          DepType.Exp out_e
        end
      | _, _ -> begin
          check_top_sz e;
          DepType.Top intermediate_sz
        end
    end
  | SingleITE ((cond, cond_l, cond_r), then_exp, else_exp) -> begin
      let cond_l = convert_dep_type_inner ctx cond_l get_var_size in
      let cond_r = convert_dep_type_inner ctx cond_r get_var_size in
      let then_exp = convert_dep_type_inner ctx then_exp get_var_size in
      let else_exp = convert_dep_type_inner ctx else_exp get_var_size in
      match cond_l, cond_r, then_exp, else_exp with
      | DepType.Exp cond_l', DepType.Exp cond_r', DepType.Exp then_exp', DepType.Exp else_exp' ->
        let ite_cond = SmtEmitter.expr_of_ite_cond ctx cond cond_l' cond_r' in
        let ite = Z3.Boolean.mk_ite ctx ite_cond then_exp' else_exp' in
        DepType.Exp ite
      | _, _, _, _ ->
        let check_match (e: DepType.t) = begin
          match e with
          | Top sz ->
            if sz != intermediate_sz then failwith "Top size mismatched";
          | Exp _ -> ()
        end in
        check_match cond_l;
        check_match cond_r;
        check_match then_exp;
        check_match else_exp;
        DepType.Top intermediate_sz
    end
  in
  match raw_res with
  | DepType.Top sz ->
    DepType.Top (max sz intermediate_sz)
  | DepType.Exp e ->
    let e_size = Z3.BitVector.get_size (Z3.Expr.get_sort e) in
    if e_size = intermediate_sz then
      DepType.Exp e
    else if e_size < intermediate_sz then
      DepType.Exp (Z3.BitVector.mk_sign_ext ctx (intermediate_sz - e_size) e)
    else
      DepType.Top e_size

let convert_dep_type
    (ctx: Z3.context)
    (se: SingleEntryType.t)
    (get_var_size: int -> int option) (* in bytes *)
    (out_size: int) (* in bytes *)
    : DepType.t =
  let out_size = out_size * 8 in (* in bits *)
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
    Z3.Boolean.mk_const_s ctx "t_unknown"

let convert_basic_type
    (ctx: Z3.context)
    (taint_entry_type: TaintEntryType.t)
    (get_var_size: int -> int option)
    (size: int)
    : BasicType.t =
  let se, te = taint_entry_type in
  (
    convert_dep_type ctx se get_var_size size,
    convert_taint_type ctx te
  )

let convert_reg_type
    (ctx: Z3.context)
    (get_var_size: int -> int option)
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
    TaintType.get_taint_exp ctx
  )

let convert_mem_offset
    (ctx: Z3.context)
    (get_var_size: int -> int option)
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
    (get_var_size: int -> int option)
    (range: Type.Mem_offset_new.MemRange.t)
    : MemRange.t * (int64 option list) =
    (* range list and size list; size is None means the inited slot's DepType should be treated as Top *)
  match range with
  | Type.Mem_offset_new.MemRange.RangeConst off_list ->
    off_list
    |> List.map (fun off ->
      convert_mem_offset ctx get_var_size off,
      Type.Mem_offset_new.MemOffset.get_size off
    )
    |> List.split
  | _ ->
    ([], [])

let convert_mem_generic
    (ctx: Z3.context)
    (get_var_size: int -> int option)
    (is_forget_slot: (int * int) -> bool)
    (mem_type: 'a Type.Mem_type_new.MemTypeBasic.mem_content)
    (convert_entry: Z3.context -> Type.Mem_offset_new.MemOffset.t -> Type.Mem_offset_new.MemRange.t -> 'a -> int64 option list -> 'b)
    : 'b MemType.mem_content =
  List.map (fun (mem_part: 'a Type.Mem_type_new.MemTypeBasic.mem_part) ->
    let ptr_info, mem_slots = mem_part in
    let mem_slots' = List.mapi (fun (idx: int) (mem_slot: 'a Type.Mem_type_new.MemTypeBasic.mem_slot) ->
      let off, range, entry = mem_slot in
      let off' = convert_mem_offset ctx get_var_size off in
      let range', inited_range_sizes = convert_mem_range ctx get_var_size range in
      let entry' = convert_entry ctx off range entry inited_range_sizes in
      (off', is_forget_slot (fst ptr_info, idx), range', entry')
    ) mem_slots
    in
    (ptr_info, mem_slots')
  ) mem_type

let convert_mem_type
    (ctx: Z3.context)
    (get_var_size: int -> int option)
    (is_forget_slot: (int * int) -> bool)
    (mem_type: TaintTypeInfer.ArchType.MemType.t)
    : MemType.t =
  let convert_entry ctx off _ entry inited_range_sizes =
    let _, slot_taint = entry in
    match inited_range_sizes, Type.Mem_offset_new.MemOffset.get_size off with
    | [Some inited_sz], Some slot_size when inited_sz <= 8L && slot_size <= 8L ->
      convert_basic_type ctx entry get_var_size (Int64.to_int inited_sz)
    | _ ->
      convert_basic_type ctx (SingleEntryType.SingleTop, slot_taint) (fun _ -> failwith "placeholder, should not be called") DepType.top_unknown_size
  in
  convert_mem_generic ctx get_var_size is_forget_slot mem_type convert_entry

let convert_context
    (ctx: Z3.context)
    (get_var_size: int -> int option)
    (single_context: SingleContext.t list)
    (taint_context: TaintExp.sub_t list)
    (taint_sol: TaintExp.local_var_map_t)
    : BasicType.ctx_t =
  let dep_ctx = List.map (fun entry ->
    SingleContext.to_smt_expr
      ~get_var_size:(Some get_var_size)
      (ctx, Z3.Solver.mk_solver ctx None (* placeholder solver *))
      entry
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
  (* vars in register *)
  let res, _ = List.fold_left (
    fun (acc, id) (entry: TaintEntryType.t) ->
      let se, _ = entry in
      let reg = TaintTypeInfer.Isa.get_full_reg_by_idx id in
      match se with
      | SingleEntryType.SingleVar v ->
        let size = TaintTypeInfer.Isa.get_reg_size reg |> Int64.to_int in
        (v, size) :: acc, id + 1
      | _ -> acc, id + 1
  ) ([], 0) reg_type
  in
  (* vars in memory slots *)
  let res = TaintTypeInfer.ArchType.MemType.fold_left_full (
    fun acc (off, range, entry) ->
      let se, _ = entry in
      match se with
      | SingleEntryType.SingleVar v -> begin
          let _, inited_range_sizes = convert_mem_range
            ctx
            (fun _ -> Some (TaintTypeInfer.Isa.get_gpr_full_size () |> Int64.to_int)) (* placeholder *)
            range 
          in
          match inited_range_sizes, Type.Mem_offset_new.MemOffset.get_size off with
          | [], _ | _, None ->
            (* slot whose inited range is not a single const range *)
            (* we should replace the SingleVar with Top and ignore these vars when converting single_var_map *)
            (v, -1) :: acc (* -1 means the variable is to be replaced by Top *)
          | [Some inited_sz], Some slot_size ->
            (* constant-size slot with single continous constant-size inited range *)
            if inited_sz > slot_size then
              failwith "mem slot's inited range is bigger than the slot";
            (* use Top instead if slot / inited range is bigger than 8 bytes *)
            if inited_sz > 8L || slot_size > 8L then
              (v, -1) :: acc
            else
              (v, Int64.to_int inited_sz) :: acc
          | _, Some _ ->
            (* inited range is not a single const range, use Top *)
            (v, -1) :: acc
        end
      | _ -> acc
  ) res mem_type
  in
  (* vars as pointer *)
  let res = List.fold_left (
    fun acc mem_part ->
      let (ptr, _), _ = mem_part in
      (ptr, ArchType.Isa.get_gpr_full_size () |> Int64.to_int) :: acc
  ) res mem_type
  in
  res

let convert_arch_type
    (ctx: Z3.context)
    (tti: TaintTypeInfer.t)
    (arch_type: TaintTypeInfer.ArchType.t)
    (* (stack_spill_info: StackSpillInfo.t) *)
    (input_var: TaintExp.TaintVarSet.t)
    (func_vsm: func_var_size_map)
    (is_forget_slot: (int * int) -> bool)
    : ArchType.t * func_var_size_map =
  let bb_var_size_map = infer_var_size_map ctx arch_type.reg_type arch_type.mem_type in
  let func_vsm = append_func_var_size_map func_vsm tti.func_name bb_var_size_map in
  let get_var_size = get_size_finder_of_func func_vsm tti.func_name in
  {
    label = arch_type.label;
    pc = arch_type.pc;
    dead_pc = arch_type.dead_pc;
    reg_type = convert_reg_type ctx get_var_size arch_type.reg_type;
    flag_type = convert_flag_type ctx;
    mem_type = convert_mem_type ctx get_var_size is_forget_slot arch_type.mem_type;
    context = convert_context ctx get_var_size arch_type.context tti.taint_context tti.taint_sol;
    (* stack_spill_info = stack_spill_info; *)

    global_var = arch_type.global_var;
    input_var = input_var;
    local_var = TaintTypeInfer.ArchType.get_local_var_set arch_type;
  }, func_vsm

let convert_is_forget_slot
    (tti: TaintTypeInfer.t)
    : IntSet.t =
  (* This function generate set of stack slot indices that allow "forget valid/init range and taint" during its lifetime.
     Usually, spills and the big slot for callee's stack (that may contain callee's spill) allow this.
     If this property is true, the slot allows
     (1) shrink of valid region size
     (2) change of taint (placement) *)
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
      List.find_index (
        fun (off, _, _) ->
          let off_l, off_r = Type.Stack_spill_info.StackSpillInfo.to_off_pair
            rsp_ptr_idx off
          in
          off_l = l && off_r = r
      ) (snd rsp_mem)
      |> Option.get
  ) (Int64PairSet.to_list tti.stack_spill_info)
  in
  (* If the function has callee, the first slot on its stack will be used as its callee's stack, 
     and may contain callee's "forget valid region slot" during its lifetime.
     If not adding this slot here, the type check will fail. *)
  let idx_list =
    if tti.has_callee then 0 :: idx_list
    else idx_list
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
  (* Printf.printf "converting slot %s\n" (Type.Mem_offset_new.MemOffset.to_string off);
  Printf.printf "ref mem:\n";
  TaintTypeInfer.ArchType.MemType.pp_mem_type 1 ref_mem; *)
  let part = TaintTypeInfer.ArchType.MemType.get_part_mem ref_mem ptr in
  let off_l, off_r = off in
  let _, idx_l, idx_r = List.fold_left (
    fun acc (slot: TaintEntryType.t TaintTypeInfer.ArchType.MemType.mem_slot) ->
      let idx, idx_l, idx_r = acc in
      let (slot_off_l, slot_off_r), _, _ = slot in
      match idx_l, idx_r with
      | None, None ->
        if Type.Mem_offset_new.MemOffset.cmp (slot_off_l, slot_off_r) off = 0 then
          (idx + 1, Some idx, Some idx)
        else if SingleEntryType.cmp slot_off_l off_l = 0 then
          (idx + 1, Some idx, None)
        else
          (idx + 1, None, None)
      | Some l, None ->
        if SingleEntryType.cmp slot_off_r off_r = 0 then
          (idx + 1, Some l, Some idx)
        else
          (idx + 1, Some l, None)
      | Some l, Some r -> (idx + 1, Some l, Some r)
      | _ -> failwith "unexpected"
  ) (0, None, None) (snd part)
  in
  let idx_l = Option.get idx_l in
  let idx_r = Option.get idx_r in
  let full = match opt_full with
  | Some b -> b
  | None -> true
  in
  if idx_l < idx_r && full = false then
    failwith "slot is not full when off spans multiple slot";
  let num = match opt_num with
  | Some x -> x
  | None -> idx_r - idx_l + 1 
  in
  if idx_r - idx_l + 1 != num then
    failwith "given slot num mismatched with the actual slot num";
  (ptr, idx_l, full, num) (* based is passed by a single full slot *)

let convert_base_info
    (ctx: Z3.context)
    (ref_mem: TaintEntryType.t TaintTypeInfer.ArchType.MemType.mem_content)
    (base_info: Type.Call_anno.CallAnno.base_info Type.Mem_type_new.MemTypeBasic.mem_content)
    (get_var_size: int -> int option)
    : FuncInterface.base_info MemType.mem_content =
  let convert_entry _ _ _ entry _ =
    match entry with
    | Type.Call_anno.CallAnno.BaseAsReg r -> FuncInterface.BaseAsReg r
    | Type.Call_anno.CallAnno.BaseAsSlot (ptr, off) -> FuncInterface.BaseAsSlot (convert_slot ref_mem ptr off (Some true) (Some 1))
    | Type.Call_anno.CallAnno.BaseAsGlobal -> FuncInterface.BaseAsGlobal
  in
  (* <NOTE> In base info, the is_forget field is not important, so I just mark it as false.
     We may want to remove base info from checker or change it to another format. *)
  convert_mem_generic ctx get_var_size (fun _ -> false) base_info convert_entry

let func_interface_is_forget_slot (slot_ptr_idx: int * int) : bool =
  let ptr, idx = slot_ptr_idx in
  ptr = Type.Isa_basic.IsaBasic.rsp_idx && idx = 0

let convert_function_interface
    (ctx: Z3.context)
    (fi: TaintTypeInfer.FuncInterface.t)
    (func_vsm: func_var_size_map)
    : func_var_size_map * FuncInterface.t =
  (* Printf.printf "converting function interface of %s\n" fi.func_name; *)
  (* <TODO> This is a dirty fix to generate is_forget field for interface in/out memory.
     Later we should either check its correctness or generate it from checker's in/out block type. *)
  let var_size_map1 = infer_var_size_map ctx fi.in_reg fi.in_mem in
  let var_size_map2 = infer_var_size_map ctx fi.out_reg fi.out_mem in
  let func_vsm = append_func_var_size_map func_vsm fi.func_name (var_size_map1 @ var_size_map2) in
  let get_var_size_func = get_size_finder_of_func func_vsm fi.func_name in
  let in_arch: ArchType.t = {
    label = fi.func_name;
    reg_type = convert_reg_type ctx get_var_size_func fi.in_reg;
    flag_type = convert_flag_type ctx;
    mem_type = convert_mem_type ctx get_var_size_func func_interface_is_forget_slot fi.in_mem;
    context = convert_context ctx get_var_size_func fi.in_context fi.in_taint_context [] (* taint solution has been substituted *);

    (* ignored fields *)
    pc = -1; 
    dead_pc = -1;
    (* stack_spill_info = IntSet.empty; *)
    global_var = IntSet.empty;
    input_var = TaintExp.TaintVarSet.empty;
    local_var = TaintExp.TaintVarSet.empty;
  }
  in
  let out_arch: ArchType.t = {
    label = fi.func_name;
    reg_type = convert_reg_type ctx get_var_size_func fi.out_reg;
    flag_type = convert_flag_type ctx;
    mem_type = convert_mem_type ctx get_var_size_func func_interface_is_forget_slot fi.out_mem;
    context = convert_context ctx get_var_size_func fi.out_context [] (* no out taint context *) [] (* taint solution has been substituted *);

    (* ignored fields *)
    pc = -1; 
    dead_pc = -1;
    (* stack_spill_info = IntSet.empty; *)
    global_var = IntSet.empty;
    input_var = TaintExp.TaintVarSet.empty;
    local_var = TaintExp.TaintVarSet.empty;
  }
  in
  func_vsm, {
    func_name = fi.func_name;
    in_type = in_arch;
    out_type = out_arch;
    (* base_info = convert_base_info ctx fi.in_mem fi.base_info get_var_size_func; *)
  }

let convert_single_var_map
    (ctx: Z3.context)
    (single_var_map: SingleEntryType.local_var_map_t)
    (from_get_var_size: int -> int option)
    (targ_get_var_size: int -> int option)
    : DepType.map_t =
  List.filter_map (
    fun (targ_var, from_exp) ->
      (* targ_var --> from_exp *)
      let targ_var_size = targ_get_var_size targ_var |> Option.get in
      if targ_var_size = -1 then
        (* this var at target context has been replaced by Top; this map entry is skipped *)
        None
      else
        let from_get_var_size' (var: int) =
          (* size of var in from context could be unknown, since we only infer size for vars at block start *)
          (* if so, we assume the size be the same as LHS, i.e. targ_var *)
          match from_get_var_size var with
          | None ->
            Printf.printf "convert_single_var_map: Warning: size of var %d in from context is assumed to be the same as targ var (%d)\n"
              var targ_var_size;
            Some targ_var_size
          | Some sz -> Some sz
        in
        let from_exp' = convert_dep_type ctx from_exp from_get_var_size' targ_var_size in
        Some (targ_var, from_exp')
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
    (from_get_var_size: int -> int option)
    (targ_get_var_size: int -> int option)
    : BranchAnno.t =
  let anno = Option.get anno in
  let dep_map = convert_single_var_map ctx anno from_get_var_size targ_get_var_size in
  dep_map, [(* taint_map: empty, since the same (input) taint vars are used in both from and targ *)]

let convert_call_anno
    (ctx: Z3.context)
    (arch_type: TaintTypeInfer.ArchType.t)
    (anno: Type.Call_anno.CallAnno.t)
    (from_mem: TaintTypeInfer.ArchType.MemType.t)
    (from_get_var_size: int -> int option)
    (targ_get_var_size: int -> int option)
    (targ_is_forget: (int * int) -> bool)
    : CallAnno.t =
  let single_local_var_map = TaintEntryType.get_single_var_map arch_type.local_var_map in
  let get_var_size (fallback: int -> int option) (var: int) : int option =
    (* this is to return the size of global vars that is not contained in the func_vsm *)
    if var < 0 && SingleEntryType.SingleVarSet.exists (fun x -> x = var) arch_type.global_var then
      Some (ArchType.Isa.get_gpr_full_size () |> Int64.to_int)
    else
      fallback var
  in

  let anno = Option.get anno in

  let pr_reg = List.mapi (
    fun reg_id (se, te) ->
      let reg_size = reg_id
        |> TaintTypeInfer.Isa.get_full_reg_by_idx
        |> TaintTypeInfer.Isa.get_reg_size
        |> Int64.to_int
      in
      let se = SingleEntryType.repl_local_var single_local_var_map se in
      (* if pr_reg is a single var, then we know an extra info of var's size *)
      let from_get_var_size = match se with
      | SingleEntryType.SingleVar v -> fun var -> if var = v then Some reg_size else from_get_var_size var
      | _ -> from_get_var_size
      in
      let se' = convert_dep_type ctx se (get_var_size from_get_var_size) reg_size in
      let te' = convert_taint_type ctx te in
      (se', te')
  ) anno.pr_reg in

  let convert_ch_mem_slot_info _ _ _ entry _ =
    let (slot_base, slot_off, slot_full), _ (* base *) = entry in
    (* reference memory is caller's memory *)
    convert_slot from_mem slot_base slot_off (Some slot_full) (Some 1)
  in
  (* <NOTE> I think is_forget does not matter here, so just return false. *)
  let ch_mem_slot_info =
    convert_mem_generic ctx (get_var_size targ_get_var_size) targ_is_forget anno.ch_mem convert_ch_mem_slot_info
  in

  {
    pr_reg = pr_reg;
    ctx_map = (
      convert_single_var_map ctx anno.single_var_map (get_var_size from_get_var_size) (get_var_size targ_get_var_size),
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

  let slot = match slot_info with
  | Some slot_info ->
    let slot_base, slot_off, slot_full, slot_num = slot_info in
    Some (convert_slot ref_mem slot_base slot_off (Some slot_full) (Some slot_num))
  | None -> None
  in

  let taint = match taint_info with
  | Some taint_info -> Some (convert_taint_type ctx taint_info)
  | None -> None
  in

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

let convert_taint_type_infers
    (ctx: Z3.context)
    (tti_list: TaintTypeInfer.t list)
    (func_vsm: func_var_size_map)
    : checker_func list =
  let func_vsm, archs_of_func = List.fold_left_map (
    fun func_vsm tti ->
      (* <TODO> I want to rename stack_spill_info to forget (stack) slot list here. *)
      let stack_forget_slot_info = convert_is_forget_slot tti in
      let is_forget_slot (slot_ptr_idx: int * int) : bool =
        let ptr, idx = slot_ptr_idx in
        ptr = Type.Isa_basic.IsaBasic.rsp_idx &&
        IntSet.mem idx stack_forget_slot_info
      in
      let input_var = convert_input_var tti in
      let vsm, arch = List.fold_left_map (
        fun func_vsm arch_type ->
          let converted, func_vsm = convert_arch_type ctx tti arch_type input_var func_vsm is_forget_slot in
          func_vsm, converted
      ) func_vsm tti.func_type
      in
      (vsm, arch)
  ) func_vsm tti_list
  in

  Printf.printf "func var size map:\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_func_var_size_map func_vsm));

  List.map2 (fun (tti: TaintTypeInfer.t) (archs: ArchType.t list) ->
    let bbs = List.map2 (
      fun (bb: TaintTypeInfer.Isa.basic_block) (bb_type: TaintTypeInfer.ArchType.t) : ArchType.Isa.basic_block ->
        let f = convert_isa_operand ctx bb_type.mem_type in
        let g = convert_mem_anno ctx bb_type.mem_type in
        let _, inst' = List.fold_left_map (fun pc (inst : TaintTypeInfer.Isa.instruction) ->
          let inst' = match inst with
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
            if Option.is_none branch_anno then begin
                ArchType.Isa.Jmp (targ, BranchAnno.get_empty ())
            end else begin
              if not (TaintTypeInfer.Isa.block_list_contains_block tti.func targ) then
                failwith (Printf.sprintf "jump to block %s that is not in current function" targ);
              let from_get_var_size = get_size_finder_of_func func_vsm tti.func_name in
              let targ_get_var_size = get_size_finder_of_func func_vsm tti.func_name in
              ArchType.Isa.Jmp (targ, convert_branch_anno ctx branch_anno from_get_var_size targ_get_var_size)
            end
          | Jcond (cond, targ, branch_anno) ->
            if Option.is_none branch_anno then begin
                ArchType.Isa.Jcond (cond, targ, BranchAnno.get_empty ())
            end else begin
              if not (TaintTypeInfer.Isa.block_list_contains_block tti.func targ) then
                failwith (Printf.sprintf "jump to block %s that is not in current function" targ);
              let from_get_var_size = get_size_finder_of_func func_vsm tti.func_name in
              let targ_get_var_size = get_size_finder_of_func func_vsm tti.func_name in
              ArchType.Isa.Jcond (cond, targ, convert_branch_anno ctx branch_anno from_get_var_size targ_get_var_size)
            end
          | Call (targ, call_anno) ->
            if Option.is_none call_anno then begin
                ArchType.Isa.Call (targ, CallAnno.get_empty ())
            end else begin
              let from_get_var_size = get_size_finder_of_func func_vsm tti.func_name in
              let targ_get_var_size = get_size_finder_of_func func_vsm targ in
              ArchType.Isa.Call (targ, convert_call_anno ctx bb_type call_anno bb_type.mem_type from_get_var_size targ_get_var_size func_interface_is_forget_slot)
            end
          | Nop -> ArchType.Isa.Nop
          | Syscall -> ArchType.Isa.Syscall
          | Hlt -> ArchType.Isa.Hlt
          | Directive s -> ArchType.Isa.Directive s
          | Unsupported raw -> convert_error (Printf.sprintf "Unsupported instruction found during conversion: %s" raw)
          in
          (pc + 1, inst')
        ) bb_type.pc bb.insts in
        {
          label = bb.label;
          insts = inst';
          mnemonics = bb.mnemonics;
          orig_asm = bb.orig_asm;
        }
    ) tti.func tti.func_type
    in

    (* calculate precise dead_pc for basic blocks whose every instruction is alive *)
    let bbs, archs = List.map2 (
      fun (bb: ArchType.Isa.basic_block) (arch_type: ArchType.t) ->
        let last_executable_pc, _ = List.fold_left (
          fun (acc, pc) inst ->
            match inst with
            | ArchType.Isa.Directive _ -> (acc, pc + 1)
            | _ -> (pc, pc + 1)
        ) (arch_type.pc, arch_type.pc) bb.insts
        in
        bb, {arch_type with dead_pc = min arch_type.dead_pc (last_executable_pc + 1) }
    ) bbs archs |> List.split in

    (tti.func_name, bbs, archs, func_vsm)
  ) tti_list archs_of_func

let converted_to_file (filename: string) (cf_list: checker_func list) : unit =
  let open Sexplib in
  let channel = open_out filename in
  Sexp.output_hum channel (Std.sexp_of_list sexp_of_checker_func cf_list)

let converted_from_file (filename: string) : checker_func list =
  let open Sexplib in
  let channel = open_in filename in
  Std.list_of_sexp checker_func_of_sexp (Sexp.input_sexp channel)
