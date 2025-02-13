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
  let fi'_list = List.map (Checker.Conversion.convert_function_interface ctx) fi_list in
  let arch_of_tti = List.map (Checker.Conversion.convert_taint_type_infer ctx) tti_list in

  Checker.Func_interface.FuncInterface.interface_list_to_file
    (get_related_filename !program_name "out" "checker_interface") fi'_list;
  Checker.Arch_type_basic.ArchTypeBasic.arch_list_list_to_file
    (get_related_filename !program_name "out" "checker_arch") arch_of_tti;
  
  ()
