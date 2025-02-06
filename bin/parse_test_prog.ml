open Read_file
open Type

let usage_msg = "parse_test_prog -asm <input_asm_file>"
let input_asm_file = ref ""

let speclist = [
  ("-asm", Arg.Set_string input_asm_file, "Set input assembly file name");
]

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  let p = Parser.Parser.parse_program (read_file !input_asm_file) in
  let name = Filename.remove_extension (Filename.basename !input_asm_file) in
  Parser.Parser.Isa.prog_to_file (get_related_filename name "out" "prog") p

