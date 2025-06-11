open Read_file
open Type

let usage_msg = "infer_single -input-dir <input_dir> -name <proram_name>"
let program_name = ref ""
let input_dir = ref ""

let speclist = [
  ("-name", Arg.Set_string program_name, "Set program name");
  ("-input-dir", Arg.Set_string input_dir, "Set input dir name");
]

let get_path = get_related_filename !program_name !input_dir

let () =  Arg.parse speclist (fun _ -> ()) usage_msg;
  (* 1. Parse program*)
  let p = Parser.Parser.parse_program (read_file (get_path "s")) in
  Parser.Parser.prog_to_file (get_related_filename !program_name "out" "prog") p;

  (* 2. Parse func input*)
  let func_input = Func_input.FuncInput.parse (read_file (get_path "final.sexp")) in
  let func_input_list = Func_input.FuncInput.update_global_symbol_layout p func_input in

  (* 3. Parse general interface *)
  let general_interface_list =
    Func_interface.FuncInterfaceConverter.TaintFuncInterface.interface_list_from_file
      "./interface/general_func_interface.func_interface"
  in

  let single_infer_result = Single_type_infer.SingleTypeInfer.infer p func_input_list general_interface_list 6 10 in
  Single_type_infer.SingleTypeInfer.state_list_to_file (get_related_filename !program_name "out" "single_infer") single_infer_result
  (* Main functionality here *)