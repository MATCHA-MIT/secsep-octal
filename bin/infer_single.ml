open Read_file
open Type

let usage_msg = "infer_single -name <proram_name>"
let program_name = ref ""

let use_cached_interface = ref false

let speclist = [
  ("-name", Arg.Set_string program_name, "Set program name");
  ("-use-cache", Arg.Set use_cached_interface, "Set whether use cache or not")
]

let () =  
  let start_time = Sys.time () in

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

  (* 3. Parse cached interface (if needed) *)
  let cached_func_interface_list =
    if !use_cached_interface then
      Single_type_infer.SingleTypeInfer.FuncInterface.interface_list_from_file (get_related_filename !program_name "out" "single_infer_interface")
    else []
  in

  (* 3. Infer *)
  let single_infer_func_interface, single_infer_result = Single_type_infer.SingleTypeInfer.infer p func_input_list general_interface_list cached_func_interface_list 6 20 in
  Single_type_infer.SingleTypeInfer.FuncInterface.interface_list_to_file (get_related_filename !program_name "out" "single_infer_interface") single_infer_func_interface;
  Single_type_infer.SingleTypeInfer.state_list_to_file (get_related_filename !program_name "out" "single_infer") single_infer_result;

  let end_time = Sys.time () in
  Stat.stat.time <- end_time -. start_time;
  Stat.statistics_to_file (get_related_filename !program_name "out" "single_infer.stat")
