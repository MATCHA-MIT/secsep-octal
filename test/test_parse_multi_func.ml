open Type.Isa
open Type.Parser


let read_file (filename: string) : string =
  let channel = open_in filename in
  try
    let content = really_input_string channel (in_channel_length channel) in
    close_in channel;
    content
  with e ->
    close_in_noerr channel;
    raise e


(* let p = Parser.parse_program (read_file "./asm/novec_ed25519_plain.c.s") *)
(* let p = Parser.parse_program (read_file "./asm/bench_ed25519_plain.c.s") *)
let p = Parser.parse_program (read_file "./asm/standalone_salsa20.c.s")

let _ = Isa.pp_prog 0 p
