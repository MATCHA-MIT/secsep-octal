open Read_file
open Type
open Asm_gen

let usage_msg = "transform -name <proram_name>"
let program_name = ref ""
let output_name = ref ""

let speclist = [
  ("-name", Arg.Set_string program_name, "Set program name");
  ("-out", Arg.Set_string output_name, "Set output asm filename")
]

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  let taint_infer_result_fi =
    Taint_type_infer.TaintTypeInfer.FuncInterface.fi_list_from_file (get_related_filename !program_name "out" "instantiate_interface")
  in
  let taint_infer_result_states =
    Taint_type_infer.TaintTypeInfer.state_list_from_file (get_related_filename !program_name "out" "instantiate_taint_infer")
  in
  let prog = Parser.Parser.prog_from_file (get_related_filename !program_name "out" "prog") in
  let tf_func_states = List.filter_map (fun (ti: Taint_type_infer.TaintTypeInfer.t) ->
    (* if ti.func_name <> "salsa20_words" && ti.func_name <> "salsa20_block" then None else *)
    let init_mem_unity = Transform.Transform.get_func_init_mem_unity ti in
    Some (Transform.Transform.transform_one_function taint_infer_result_fi ti init_mem_unity)
  ) taint_infer_result_states in

  (* print into output_name *)
  let oc = open_out !output_name in
  Printf.fprintf oc "%s" (AsmGen.gen_asm prog tf_func_states);
  ()

  (* in
  Taint_type_infer.TaintTypeInfer.state_list_to_file (get_related_filename !program_name "out" "taint_infer") range_infer_result *)
