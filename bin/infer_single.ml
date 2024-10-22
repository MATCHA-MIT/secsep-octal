open Read_file
open Type

let usage_msg = "infer_single -asm <input_asm_file> -interface <input_interface_file> -name <proram_name>"
let input_asm_file = ref ""
let input_interface_file = ref ""
let program_name = ref ""

let speclist = [
  ("-asm", Arg.Set_string input_asm_file, "Set input assembly file name");
  ("-interface", Arg.Set_string input_interface_file, "Set input function interface file name");
  ("-name", Arg.Set_string program_name, "Set program name")
]

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  let p = Parser.Parser.parse_program (read_file !input_asm_file) in
  let interface_list = Base_func_interface.parse (read_file !input_interface_file) in
  let single_infer_result = Single_type_infer.SingleTypeInfer.infer !program_name p interface_list 5 10 in
  Single_type_infer.SingleTypeInfer.state_list_to_file (get_single_infer_filename !program_name) single_infer_result
  (* Main functionality here *)