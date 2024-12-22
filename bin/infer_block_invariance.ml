open Read_file
open Type
(* open Sexplib.Std *)

let () =
  let state_list = 
    Single_type_infer.SingleTypeInfer.state_list_from_file (get_related_filename "bench_ed25519_plain_noinline" "out" "single_infer") 
  in
  let state = List.nth state_list 1 in
  (* let _ = 
    Single_block_invariance.SingleBlockInvariance.solve 
    state.smt_ctx true state.input_var_set state.func_type state.block_subtype state.single_subtype 5 
  in *)
  let sub_range_func = 
    Single_subtype.SingleSubtype.sub_sol_single_to_range 
    (fun x -> x)
    state.single_subtype state.input_var_set
  in
  let _, _ =
    Single_ret_invariance.SingleRetInvariance.gen_ret_invariance 
        state.smt_ctx sub_range_func state.input_var_set
        state.func_type state.block_subtype state.single_subtype
  in
  (* Printf.printf "func_type\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list Single_type_infer.SingleTypeInfer.ArchType.sexp_of_t func_type));
  Printf.printf "ret_subtype_list\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list Single_ret_invariance.SingleRetInvariance.sexp_of_subtype_t ret_subtype_list)); *)
  ()

