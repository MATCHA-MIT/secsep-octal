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
    Type.Taint_type_infer.TaintTypeInfer.FuncInterface.interface_list_from_file
    (get_related_filename !program_name "out" "interface")
  in
  let tti_list = Type.Taint_type_infer.TaintTypeInfer.state_list_from_file
    (get_related_filename !program_name "out" "taint_infer")
  in

  let smt_ctx = Type.Smt_emitter.SmtEmitter.init_smt_ctx () in
  let ctx, _ = smt_ctx in

  let func_vsm, fi_list = List.fold_left_map (fun func_vsm fi -> Checker.Conversion.convert_function_interface ctx fi func_vsm) [] fi_list in
  let checker_func_list = Checker.Conversion.convert_taint_type_infers ctx tti_list func_vsm in

  let all_success = List.fold_left (
    fun acc_success checker_func ->
      let func_name, func_body, func_type, _ = checker_func in
      Printf.printf "=================================================================\n";
      Printf.printf "checking function %s\n" func_name;
      Printf.printf "=================================================================\n";
      let success = Arch_type.ArchType.type_prop_check_one_func
        smt_ctx
        fi_list
        func_name
        func_type
        func_body
      in
      Printf.printf "Function %s: %s\n" func_name (if success then "Passed" else "Failed");
      Printf.printf "=================================================================\n\n\n\n\n";
      acc_success && success
  ) true checker_func_list
  in

  if not all_success then
    exit 1
  else
    ()

