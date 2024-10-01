open Type.Taint_type_infer
open Test_function_single_infer_state

let _ = TaintTypeInfer.infer_one_func [] salsa20_words_single_infer_state
