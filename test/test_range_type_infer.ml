open Type.Range_type_infer
open Test_function_single_infer_state

(* let _ = RangeTypeInfer.infer_one_func [] (List.nth standalone_salsa20_single_infer_state 0) *)
(* let _ = RangeTypeInfer.infer standalone_salsa20_single_infer_state *)
let _ = 
  RangeTypeInfer.infer [
    List.nth standalone_salsa20_single_infer_state 0;
    List.nth standalone_salsa20_single_infer_state 1;
    List.nth standalone_salsa20_single_infer_state 2;
  ] []
