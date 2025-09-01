open Type
(* open Type.Single_exp_basic *)

let r = Isa_basic.IsaBasic.get_reg_idx

let get_default_info = Ptr_info.PtrInfo.get_default_info
let get_const_default_info = Ptr_info.PtrInfo.get_const_default_info

let get_reg_taint (reg_taint: (Isa_basic.IsaBasic.register * bool) list) : (bool option) list =
  List.fold_left (
    fun (acc: (bool option) list) (reg, taint) ->
      List.mapi (
        fun i x ->
          if i = r reg then Some taint else x
      ) acc
  ) (List.init Isa_basic.IsaBasic.total_reg_num (fun _ -> None)) reg_taint

let update_reg_taint 
    (reg_type: Taint_type_infer.TaintTypeInfer.ArchType.RegType.t) 
    (reg_taint: (bool option) list) :
    Taint_type_infer.TaintTypeInfer.ArchType.RegType.t =
  List.map2 (
    fun (valid, (single, taint)) (taint_opt: bool option) ->
      match taint_opt with
      | Some b -> valid, (single, Taint_exp.TaintExp.TaintConst b)
      | None -> valid, (single, taint)
  ) reg_type reg_taint

let get_change_var (non_change_list: int list) : Type.Set_sexp.IntSet.t =
  let all_var = List.init Type.Isa_basic.IsaBasic.total_reg_num (fun x -> x) |> Type.Set_sexp.IntSet.of_list in
  Type.Set_sexp.IntSet.diff all_var (Type.Set_sexp.IntSet.of_list non_change_list)

let memset_interface: Taint_type_infer.TaintTypeInfer.FuncInterface.t = 
  let start_var: Taint_entry_type.TaintEntryType.t = (SingleVar 0, TaintVar 0) in
  let _, default_reg_type = Taint_type_infer.TaintTypeInfer.ArchType.RegType.init_reg_type start_var in
  let in_reg : Taint_type_infer.TaintTypeInfer.ArchType.RegType.t =
    update_reg_taint default_reg_type
      (get_reg_taint [
        RDI, false;
        RDX, false;
      ])
    |> Taint_type_infer.TaintTypeInfer.ArchType.RegType.update_input_reg_range 3
  in
  let out_reg = Taint_type_infer.TaintTypeInfer.ArchType.RegType.mapi (
    fun (i: int) entry ->
      if Isa_basic.IsaBasic.is_reg_idx_callee_saved i then entry
      else Single_exp.SingleExp.SingleTop, Taint_exp.TaintExp.TaintConst true 
  ) in_reg
  |> Taint_type_infer.TaintTypeInfer.ArchType.RegType.update_ret_reg_range
  in
  let in_mem : Taint_type_infer.TaintTypeInfer.ArchType.MemType.t = Taint_type_infer.TaintTypeInfer.ArchType.MemType.add_base_to_offset [
    get_default_info (r RDI), [ 
      (Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX)), 
      RangeConst [], 
      (Single_exp.SingleExp.SingleTop, Taint_exp.TaintExp.TaintVar (Isa_basic.IsaBasic.total_reg_num)) 
    ];
    get_default_info (r RSP), [ (SingleConst 0L, SingleConst 8L), RangeConst [], (SingleTop, TaintConst true) ];
  ] in
  let mem_context = 
    Taint_type_infer.TaintTypeInfer.ArchType.MemType.get_all_mem_constraint
      (Taint_type_infer.TaintTypeInfer.ArchType.MemType.add_ret_addr_stack_slot in_mem)
  in
  {
    func_name = "memset";
    in_reg = in_reg;
    in_mem = in_mem;
    in_context = mem_context;
    in_taint_context = [];
    in_change_var = get_change_var [r RSI; r RDX];
    out_reg = out_reg;
    out_mem = Taint_type_infer.TaintTypeInfer.ArchType.MemType.add_base_to_offset [
      get_default_info (r RDI), [ 
        (Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX)), 
        RangeConst [(Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX))], 
        (Single_exp.SingleExp.SingleTop, Taint_exp.TaintExp.TaintVar (Isa_basic.IsaBasic.total_reg_num)) 
      ];
      get_default_info (r RSP), [ (SingleConst 0L, SingleConst 8L), RangeConst [], (SingleTop, TaintConst true) ];
    ];
    out_context = [];
    out_single_subtype_list = [];
    base_info = Taint_type_infer.TaintTypeInfer.ArchType.MemType.add_base_to_offset [
      get_default_info (r RDI), [ 
        (Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX)), 
        RangeConst [], 
        Call_anno.CallAnno.BaseAsReg RDI
      ];
      get_default_info (r RSP), [ (SingleConst 0L, SingleConst 8L), RangeConst [], BaseAsReg RSP ];
    ];
  }

let memcpy_interface: Taint_type_infer.TaintTypeInfer.FuncInterface.t = 
  let start_var: Taint_entry_type.TaintEntryType.t = (SingleVar 0, TaintVar 0) in
  let _, default_reg_type = Taint_type_infer.TaintTypeInfer.ArchType.RegType.init_reg_type start_var in
  let in_reg : Taint_type_infer.TaintTypeInfer.ArchType.RegType.t =
    update_reg_taint default_reg_type
      (get_reg_taint [
        RDI, false; (* dest ptr *)
        RSI, false; (* src ptr *)
        RDX, false;
      ])
    |> Taint_type_infer.TaintTypeInfer.ArchType.RegType.update_input_reg_range 3
  in
  let out_reg = Taint_type_infer.TaintTypeInfer.ArchType.RegType.mapi (
    fun (i: int) entry ->
      if Isa_basic.IsaBasic.is_reg_idx_callee_saved i then entry
      else Single_exp.SingleExp.SingleTop, Taint_exp.TaintExp.TaintConst true 
  ) in_reg
  |> Taint_type_infer.TaintTypeInfer.ArchType.RegType.update_ret_reg_range
  in
  let dest_taint = Taint_exp.TaintExp.TaintVar (Isa_basic.IsaBasic.total_reg_num) in
  let src_taint = Taint_exp.TaintExp.TaintVar (Isa_basic.IsaBasic.total_reg_num + 1) in
  let in_mem : Taint_type_infer.TaintTypeInfer.ArchType.MemType.t = Taint_type_infer.TaintTypeInfer.ArchType.MemType.add_base_to_offset [
    get_default_info (r RDI), [ 
      (Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX)), 
      RangeConst [], 
      (Single_exp.SingleExp.SingleTop, dest_taint) 
    ];
    get_const_default_info (r RSI), [ 
      (Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX)), 
      RangeConst [(Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX))], 
      (Single_exp.SingleExp.SingleTop, src_taint) 
    ];
    get_default_info (r RSP), [ (SingleConst 0L, SingleConst 8L), RangeConst [], (SingleTop, TaintConst true) ];
  ] in
  let mem_context = 
    Taint_type_infer.TaintTypeInfer.ArchType.MemType.get_all_mem_constraint
      (Taint_type_infer.TaintTypeInfer.ArchType.MemType.add_ret_addr_stack_slot in_mem)
  in
  {
    func_name = "memcpy";
    in_reg = in_reg;
    in_mem = in_mem;
    in_context = mem_context;
    in_taint_context = [ src_taint, dest_taint ];
    in_change_var = get_change_var [r RDX];
    out_reg = out_reg;
    out_mem = Taint_type_infer.TaintTypeInfer.ArchType.MemType.add_base_to_offset [
      get_default_info (r RDI), [ 
        (Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX)), 
        RangeConst [(Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX))], 
        (Single_exp.SingleExp.SingleTop, dest_taint) 
      ];
      get_const_default_info (r RSI), [ 
      (Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX)), 
      RangeConst [(Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX))], 
      (Single_exp.SingleExp.SingleTop, src_taint) 
      ];
      get_default_info (r RSP), [ (SingleConst 0L, SingleConst 8L), RangeConst [], (SingleTop, TaintConst true) ];
    ];
    out_context = [];
    out_single_subtype_list = [];
    base_info = Taint_type_infer.TaintTypeInfer.ArchType.MemType.add_base_to_offset [
      get_default_info (r RDI), [ 
        (Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX)), 
        RangeConst [], 
        Call_anno.CallAnno.BaseAsReg RDI
      ];
      get_const_default_info (r RSI), [ 
        (Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX)), 
        RangeConst [(Single_exp.SingleExp.SingleConst 0L, Single_exp.SingleExp.SingleVar (r RDX))], 
        Call_anno.CallAnno.BaseAsReg RSI 
      ];
      get_default_info (r RSP), [ (SingleConst 0L, SingleConst 8L), RangeConst [], BaseAsReg RSP ];
    ];
  }


let add_general_func_suffix
    (func_interface: Taint_type_infer.TaintTypeInfer.FuncInterface.t) :
    Taint_type_infer.TaintTypeInfer.FuncInterface.t list =
  [func_interface;
  {func_interface with func_name = func_interface.func_name ^ "@PLT"}]

let () = 
  (* External_layouts.StackLayout.test(); *)
  let open Sexplib in

  let channel = open_out "./interface/general_func_interface.func_interface" in
  Sexp.output_hum channel (Sexplib.Std.sexp_of_list Taint_type_infer.TaintTypeInfer.FuncInterface.sexp_of_t 
    (List.concat_map add_general_func_suffix [memset_interface; memcpy_interface]));
