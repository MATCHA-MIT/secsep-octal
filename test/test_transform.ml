open Type.Taint_type_infer
open Type.Transform
open Test_function_single_infer_state

let ti_state = TaintTypeInfer.infer_one_func [] salsa20_words_single_infer_state
let _ = Transform.transform_one_function [] ti_state
