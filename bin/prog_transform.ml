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
    Taint_type_infer.TaintTypeInfer.FuncInterface.fi_list_from_file (get_interface_list_filename !program_name) 
  in
  let taint_infer_result_states = 
    Taint_type_infer.TaintTypeInfer.state_list_from_file (get_taint_infer_filename !program_name) 
  in
  let prog = Parser.Parser.prog_from_file (get_parsed_program_filename !program_name) in
  let test_func_state = List.hd taint_infer_result_states in (* FIXME: only transforming the first function, for test purpose *)
  let init_mem_unity = Transform.Transform.get_func_init_mem_unity test_func_state in
  let tf_func_state =
    Transform.Transform.transform_one_function taint_infer_result_fi test_func_state init_mem_unity
  in

  (* print into output_name *)
  let oc = open_out !output_name in
  Printf.fprintf oc "%s" (AsmGen.gen_asm prog [tf_func_state]);
  ()

  (* in
  Taint_type_infer.TaintTypeInfer.state_list_to_file (get_taint_infer_filename !program_name) range_infer_result *)
