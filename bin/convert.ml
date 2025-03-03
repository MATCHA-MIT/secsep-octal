open Read_file

let usage_msg = "convert -name <proram_name>"
let program_name = ref ""


let speclist = [
  ("-name", Arg.Set_string program_name, "Set program name")
]

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;

  let fi_list =
    Type.Taint_type_infer.TaintTypeInfer.FuncInterface.interface_list_from_file
    (get_related_filename !program_name "out" "interface")
  in
  let tti_list = Type.Taint_type_infer.TaintTypeInfer.state_list_from_file
    (get_related_filename !program_name "out" "taint_infer")
  in

  let ctx = Z3.mk_context [] in
  let func_vsm, fi'_list = List.fold_left_map (fun func_vsm fi -> Checker.Conversion.convert_function_interface ctx fi func_vsm) [] fi_list in
  let converted_funcs = Checker.Conversion.convert_taint_type_infers ctx tti_list func_vsm in

  Checker.Func_interface.FuncInterface.interface_list_to_file
    (get_related_filename !program_name "out" "checker_interface") fi'_list;
  Checker.Conversion.converted_to_file
    (get_related_filename !program_name "out" "checker_func") converted_funcs;
  
  ()

