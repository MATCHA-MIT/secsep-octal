open Read_file
open Type

let usage_msg = "infer_single -name <proram_name>"
let program_name = ref ""

let speclist = [
  ("-name", Arg.Set_string program_name, "Set program name");
]

let () =  
  Arg.parse speclist (fun _ -> ()) usage_msg;

  (* 1. Parse func and func_input *)
  let p = Parser.Parser.prog_from_file (get_related_filename !program_name "out" "prog") in
  let func_input_list = 
    Func_input.FuncInput.parse_input (read_file (get_related_filename !program_name "out" "input")) in

  (* 2. Parse general interface *)
  let general_interface_list =
    Func_interface.FuncInterfaceConverter.TaintFuncInterface.interface_list_from_file
      "./interface/general_func_interface.func_interface"
  in

  (* 3. Infer *)
  let single_infer_result = Single_type_infer.SingleTypeInfer.infer p func_input_list general_interface_list 6 10 in
  Single_type_infer.SingleTypeInfer.state_list_to_file (get_related_filename !program_name "out" "single_infer") single_infer_result
  (* Main functionality here *)