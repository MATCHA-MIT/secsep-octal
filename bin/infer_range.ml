open Read_file
open Type

let usage_msg = "infer_range -name <proram_name>"
let program_name = ref ""

let speclist = [
  ("-name", Arg.Set_string program_name, "Set program name")
]

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  let single_infer_result = 
    Single_type_infer.SingleTypeInfer.state_list_from_file (get_output_filename !program_name "out" "single_infer") 
  in
  let range_infer_result =
    Range_type_infer.RangeTypeInfer.infer single_infer_result
  in
  Range_type_infer.RangeTypeInfer.state_list_to_file (get_output_filename !program_name "out" "range_infer") range_infer_result
