open Read_file
open Type

let () =
  let state_list = 
    Single_type_infer.SingleTypeInfer.state_list_from_file (get_related_filename "bench_ed25519_plain" "out" "single_infer") 
  in
  let state = List.nth state_list 1 in
  let _ = Single_block_invariance.SingleBlockInvariance.solve state.smt_ctx state.input_var_set state.func_type state.block_subtype 5 in
  ()

