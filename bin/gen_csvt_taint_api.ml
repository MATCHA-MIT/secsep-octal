open Read_file
open Type
open Single_exp.SingleExp
open Isa_basic.IsaBasic

let usage_msg = "gen_conservative_taint_api -name <program_name>"
let program_name = ref ""

let speclist = [
  ("-name", Arg.Set_string program_name, "Set program name")
]

let skip_func_list = [
  "memcpy"; "memcpy@PLT";
  "memset"; "memset@PLT";
  "OPENSSL_cleanse";
  "CRYPTO_memcpy";
]

let specified_reg_ed25519 = [
  "x25519_sc_reduce", RDX, true;
]

let specified_mem_ed25519 = [
  "SHA512_Init",
  (
    SingleBExp (SingleAdd, SingleVar 7, SingleConst 64L),
    SingleBExp (SingleAdd, SingleVar 7, SingleConst 72L)
  ),
  false;

  "SHA512_Init",
  (
    SingleBExp (SingleAdd, SingleVar 7, SingleConst 72L),
    SingleBExp (SingleAdd, SingleVar 7, SingleConst 80L)
  ),
  false;

  "SHA512_Init",
  (
    SingleBExp (SingleAdd, SingleVar 7, SingleConst 208L),
    SingleBExp (SingleAdd, SingleVar 7, SingleConst 212L)
  ),
  false;

  "SHA512_Init",
  (
    SingleBExp (SingleAdd, SingleVar 7, SingleConst 212L),
    SingleBExp (SingleAdd, SingleVar 7, SingleConst 216L)
  ),
  false;

  "SHA512_Final",
  (
    SingleBExp (SingleAdd, SingleVar 6, SingleConst 64L),
    SingleBExp (SingleAdd, SingleVar 6, SingleConst 72L)
  ),
  false;

  "SHA512_Final",
  (
    SingleBExp (SingleAdd, SingleVar 6, SingleConst 72L),
    SingleBExp (SingleAdd, SingleVar 6, SingleConst 80L)
  ),
  false;

  "SHA512_Final",
  (
    SingleBExp (SingleAdd, SingleVar 6, SingleConst 208L),
    SingleBExp (SingleAdd, SingleVar 6, SingleConst 212L)
  ),
  false;

  "SHA512_Final",
  (
    SingleBExp (SingleAdd, SingleVar 6, SingleConst 212L),
    SingleBExp (SingleAdd, SingleVar 6, SingleConst 216L)
  ),
  false;

  "SHA512_Update",
  (
    SingleBExp (SingleAdd, SingleVar 7, SingleConst 64L),
    SingleBExp (SingleAdd, SingleVar 7, SingleConst 72L)
  ),
  false;

  "SHA512_Update",
  (
    SingleBExp (SingleAdd, SingleVar 7, SingleConst 72L),
    SingleBExp (SingleAdd, SingleVar 7, SingleConst 80L)
  ),
  false;

  "SHA512_Update",
  (
    SingleBExp (SingleAdd, SingleVar 7, SingleConst 208L),
    SingleBExp (SingleAdd, SingleVar 7, SingleConst 212L)
  ),
  false;

  "SHA512_Update",
  (
    SingleBExp (SingleAdd, SingleVar 7, SingleConst 212L),
    SingleBExp (SingleAdd, SingleVar 7, SingleConst 216L)
  ),
  false;

]

let get_reg_taint (reg_taint: (label * register * bool) list) (func_name: label) : (bool option) list =
  List.fold_left (
    fun (acc: (bool option) list) (f, reg, taint) ->
      if not (String.equal f func_name) then acc else
      List.mapi (
        fun i x ->
          if i = (get_reg_idx reg) then Some taint else x
      ) acc
  ) (List.init Isa_basic.IsaBasic.total_reg_num (fun _ -> None)) reg_taint

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  let func_interface_list =
    Taint_type_infer.TaintTypeInfer.FuncInterface.interface_list_from_file (get_related_filename !program_name "out" "interface")
  in
  let _ =
    Taint_type_infer.TaintTypeInfer.state_list_from_file (get_related_filename !program_name "out" "taint_infer")
  in

  let find_manual_specify_mem l (func_name: label) (offset: Mem_offset_new.MemOffset.t) : (bool option) =
    List.find_map (
      fun (item: label * Mem_offset_new.MemOffset.t * bool) ->
        let f, o, b = item in
        if String.equal f func_name && Mem_offset_new.MemOffset.cmp offset o = 0 then
          Some b
        else
          None
    ) l
  in
  
  let taint_api_csvt: Taint_api.TaintApi.t = List.filter_map (
    fun (fi: Taint_type_infer.TaintTypeInfer.FuncInterface.t) : Taint_api.TaintApi.api_t option ->
      if List.mem fi.func_name skip_func_list then None else

      let reg_api = get_reg_taint specified_reg_ed25519 fi.func_name in
      List.iteri (fun idx (reg_spec, reg_interface) ->
        let _, te = reg_interface in
        if Option.is_some reg_spec && Taint_exp.TaintExp.cmp
            (Taint_exp.TaintExp.TaintConst (Option.get reg_spec)) te != 0 then
          Printf.printf "reg spec for %s %s takes effect\n" fi.func_name (string_of_reg (get_full_reg_by_idx idx));
      ) (List.combine reg_api fi.in_reg);

      let mem_api = Taint_type_infer.TaintTypeInfer.ArchType.MemType.map_full (
        fun (item: Mem_offset_new.MemOffset.t * Mem_offset_new.MemRange.t * Taint_entry_type.TaintEntryType.t) ->
          let off, init_range, taint_entry_type = item in
          let _, te = taint_entry_type in

          let mem_spec = find_manual_specify_mem specified_mem_ed25519 fi.func_name off in
          if Option.is_some mem_spec && Taint_exp.TaintExp.cmp 
              (Taint_exp.TaintExp.TaintConst (Option.get mem_spec)) te != 0 then
            Printf.printf "mem spec for %s %s takes effect\n" fi.func_name (Mem_offset_new.MemOffset.to_string off);

          match te with
          | Taint_exp.TaintExp.TaintConst _ -> off, init_range, mem_spec
          | Taint_exp.TaintExp.TaintVar _ -> off, init_range, (
              if Option.is_some mem_spec then
                mem_spec
              else
                Some true (* conservative *)
            )
          | _ -> off, init_range, mem_spec
      ) fi.in_mem in

      (* filter out global symbols, as their taint API will be provided separately *)
      let mem_api = List.filter (
        fun mem_part ->
          let base_id, _ = mem_part in
          base_id >= 0
      ) mem_api in

      Some (fi.func_name, reg_api, mem_api)
  ) func_interface_list in

  let channel = open_out (get_related_filename !program_name "interface" "taint_api_csvt") in
  Sexplib.Sexp.output_hum channel (Taint_api.TaintApi.sexp_of_t taint_api_csvt);
