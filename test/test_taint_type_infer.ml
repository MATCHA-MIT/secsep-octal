open Type.Parser
(* open Type.Taint_type_infer *)
open Test_function_single_infer_state

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

let _ = salsa20_words_single_infer_state
let _ = p
