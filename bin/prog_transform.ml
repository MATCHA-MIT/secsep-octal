open Read_file
open Type
open Asm_gen

let usage_msg = "transform -name <proram_name>"
let program_name = ref ""
let output_name = ref ""
let delta_string : string ref = ref ""
let disable_tf_push_pop = ref false
let disable_tf_call_preservation = ref false

let speclist = [
  ("-name", Arg.Set_string program_name, "Set program name");
  ("-out", Arg.Set_string output_name, "Set output asm filename");
  ("--delta", Arg.Set_string delta_string, "Absolute offset from public stack to secret stack");
  ("--no-push-pop", Arg.Set disable_tf_push_pop, "Disable push/pop transformation (security property harmed!!!)");
  ("--no-call-preservation", Arg.Set disable_tf_call_preservation, "Disable call untaint preservation transformation");
]

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;

  let delta = Int64.of_string_opt !delta_string |> Option.value ~default:0x100000L in
  if delta <= 0L then
    failwith "expecting positive delta value";

  let tf_config : Transform.Transform.tf_config_t = {
    delta = delta;
    disable_tf_push_pop = !disable_tf_push_pop;
    disable_tf_call_preservation = !disable_tf_call_preservation;
  } in

  let taint_infer_result_fi =
    Taint_type_infer.TaintTypeInfer.FuncInterface.fi_list_from_file (get_related_filename !program_name "out" "instantiate_interface")
  in
  let taint_infer_result_states =
    Taint_type_infer.TaintTypeInfer.state_list_from_file (get_related_filename !program_name "out" "instantiate_taint_infer")
  in
  let prog = Parser.Parser.prog_from_file (get_related_filename !program_name "out" "prog") in
  let tf_func_states = List.filter_map (fun (ti: Taint_type_infer.TaintTypeInfer.t) ->
    (* if ti.func_name <> "salsa20_words" && ti.func_name <> "salsa20_block" then None else *)
    if Option.get ti.alive then begin
      let init_mem_unity = Transform.Transform.get_func_init_mem_unity ti in
      Some (Transform.Transform.transform_one_function tf_config taint_infer_result_fi ti init_mem_unity)
    end else
      None
  ) taint_infer_result_states in

  let soft_faults, tf_func_states = List.fold_left_map (fun acc data ->
    let func_tf, sf = data in
    sf @ acc, func_tf
  ) [] tf_func_states in

  if List.length soft_faults > 0 then begin
    Printf.printf "Transformation failed, taint variables require instantiation:\n";
    List.iter (fun (soft_fault: Transform.Transform.tv_fault_t) ->
      let f, b, v, r = soft_fault in
      Printf.printf "\tF=%-30s B=%-30s TaintVar %-10d %s\n" f b v r;
    ) soft_faults
  end;

  (* print into output_name *)
  let oc = open_out !output_name in
  Printf.fprintf oc "%s" (AsmGen.gen_asm prog tf_func_states);

  (* in
  Taint_type_infer.TaintTypeInfer.state_list_to_file (get_related_filename !program_name "out" "taint_infer") range_infer_result *)
