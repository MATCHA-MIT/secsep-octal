(* open Type.Isa *)
open Type.Parser
(* open Type.Single_subtype *)
open Type.Single_type_infer
open Test_function_interface_new

let read_file (filename: string) : string =
  let channel = open_in filename in
  try
    let content = really_input_string channel (in_channel_length channel) in
    close_in channel;
    content
  with e ->
    close_in_noerr channel;
    raise e

let p = Parser.parse_program (read_file "./asm/standalone_salsa20.c.s")

(* let _ = Isa.pp_prog 0 p *)

(* let infer_state = SingleTypeInfer.init p "salsa20_block" salsa20_block_init_mem
let infer_state = SingleTypeInfer.type_prop_all_blocks infer_state 10 *)

(* let infer_func_name = "salsa20_block" *)
(* let infer_func_name = "salsa20_words"
let infer_state = 
  SingleTypeInfer.infer_one_func 
    p [] infer_func_name 
    (get_mem_interface standalone_salsa20 infer_func_name) 4 10

let _ = SingleSubtype.pp_single_subtype 0 infer_state.single_subtype *)

let _ = SingleTypeInfer.infer p standalone_salsa20 4 10
