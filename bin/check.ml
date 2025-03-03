open Read_file
open Checker

let usage_msg = "check -name <proram_name>"
let program_name = ref ""


let speclist = [
  ("-name", Arg.Set_string program_name, "Set program name")
]

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;

  let fi_list =
    Func_interface.FuncInterface.interface_list_from_file
    (get_related_filename !program_name "out" "checker_interface")
  in
  let checker_func_list =
    Checker.Conversion.converted_from_file
    (get_related_filename !program_name "out" "checker_func")
  in

  List.iter (
    fun checker_func ->
      let func_name, func_body, func_type, _ = checker_func in
      let result = Arch_type.ArchType.type_prop_check_one_func
        (Type.Smt_emitter.SmtEmitter.init_smt_ctx ())
        fi_list
        func_name
        func_type
        func_body
      in
      Printf.printf "Function %s: %s\n" func_name (if result then "Passed" else "Failed")
  ) checker_func_list;
  
  ()

