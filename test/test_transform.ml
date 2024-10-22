open Type.Taint_type_infer
open Type.Transform
open Test_function_single_infer_state

let ti_state = TaintTypeInfer.infer_one_func [] (List.nth standalone_salsa20_single_infer_state 0) (* type not match! this code is to be abandoned *)
let _ = Transform.transform_one_function [] ti_state
