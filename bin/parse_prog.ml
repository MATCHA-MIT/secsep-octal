open Read_file
open Type

let usage_msg = "parse_prog -asm <input_asm_file> -name <proram_name>"
let input_asm_file = ref ""
let program_name = ref ""

let speclist = [
  ("-asm", Arg.Set_string input_asm_file, "Set input assembly file name");
  ("-name", Arg.Set_string program_name, "Set program name")
]

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  let p = Parser.Parser.parse_program (read_file !input_asm_file) in
  Parser.Parser.Isa.prog_to_file (get_output_filename !program_name "out" "s") p

