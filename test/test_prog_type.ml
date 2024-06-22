open Type.Isa
open Type.Parser
open Type.State_type
open Type.Prog_type
open Test_function_interface

let read_file (filename: string) : string =
  let channel = open_in filename in
  try
    let content = really_input_string channel (in_channel_length channel) in
    close_in channel;
    content
  with e ->
    close_in_noerr channel;
    raise e

(* let p = Parser.parse_program (read_file "../asm/demo.s")  *)
let p = Parser.parse_program (read_file "./asm/sha512_block_data_order.s") 
(* let p = Parser.parse_program (read_file "./asm/sha512_final_impl.s")  *)
(* let p = Parser.parse_program (read_file "../asm/double_layer.s")  *)

let start_single_var_idx = StateType.stack_base_id + Isa.StrM.cardinal p.imm_var_map + 1

let _ = ProgType.gen 0 start_single_var_idx sha512_block_data_order_init_mem p 4
(* let _ = ProgType.gen 0 start_single_var_idx sha512_final_impl_init_mem p 4 *)
