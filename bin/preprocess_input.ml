open Read_file
open Type

let usage_msg = "preprocess_input -input-dir <input_dir> -name <proram_name>"
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
  let func_input_list = Func_input.FuncInput.update_func_input p func_input in
  Func_input.FuncInput.input_to_file (get_related_filename !program_name "out" "input") func_input_list
