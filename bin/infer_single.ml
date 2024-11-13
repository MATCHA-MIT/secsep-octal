open Read_file
open Type

let usage_msg = "infer_single -asm <input_asm_file> -name <proram_name>"
let program_name = ref ""
let input_asm_file = ref ""

let speclist = [
  ("-name", Arg.Set_string program_name, "Set program name");
  ("-asm", Arg.Set_string input_asm_file, "Set input assembly file name");
]

let () =  Arg.parse speclist (fun _ -> ()) usage_msg;
  let p = Parser.Parser.parse_program (read_file !input_asm_file) in
  Parser.Parser.prog_to_file (get_related_filename !program_name "out" "prog") p;
  let interface_list = Base_func_interface.parse (read_file (get_related_filename !program_name "interface" "mem_interface")) in
  let general_interface_list =
    Func_interface.FuncInterfaceConverter.TaintFuncInterface.interface_list_from_file
      "./interface/general_func_interface.func_interface"
  in
  let single_infer_result = Single_type_infer.SingleTypeInfer.infer p interface_list general_interface_list 5 10 in
  Single_type_infer.SingleTypeInfer.state_list_to_file (get_related_filename !program_name "out" "single_infer") single_infer_result
  (* Main functionality here *)