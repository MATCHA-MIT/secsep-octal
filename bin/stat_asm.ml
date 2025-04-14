open Read_file
open Type

let usage_msg = "stat_asm -asm <input_asm_file>"
let input_asm_file = ref ""
let output_lines_file = ref ""

let speclist = [
  ("-asm", Arg.Set_string input_asm_file, "Set input assembly file name");
  ("-preview", Arg.Set_string output_lines_file, "Set output lines file name");
]

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  let lines = Parser.Parser.preprocess (read_file !input_asm_file) in
  let lines = List.filter (fun line -> not ((Parser.Parser.Isa.line_is_directive line) || (Parser.Parser.Isa.line_is_label line))) lines in
  Printf.printf "asm line count: %d\n" (List.length lines);
  if !output_lines_file <> "" then
    let oc = open_out !output_lines_file in
    List.iter (Printf.fprintf oc "%s\n") lines;
    close_out oc;
