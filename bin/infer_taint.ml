open Read_file
open Type

let usage_msg = "infer_taint -name <proram_name>"
let program_name = ref ""
let use_taint_api = ref false

let speclist = [
  ("-name", Arg.Set_string program_name, "Set program name");
  ("-use-taint-api", Arg.Set use_taint_api, "Whether taint infer use taint api")
]

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  let range_infer_result = 
    Range_type_infer.RangeTypeInfer.state_list_from_file (get_related_filename !program_name "out" "range_infer") 
  in
  let general_interface_list = 
    Func_interface.FuncInterfaceConverter.TaintFuncInterface.interface_list_from_file "./interface/general_func_interface.func_interface" 
  in
  let taint_api =
    if !use_taint_api then
      Taint_api.TaintApi.api_from_file (get_related_filename !program_name "interface" "taint_api_csvt")
    else []
  in
  let p = Parser.Parser.prog_from_file (get_related_filename !program_name "out" "prog") in
  let global_symbol_layout = External_layouts.GlobalSymbolLayout.from_file (get_related_filename !program_name "interface" "symbol_layout") in
  let taint_api = Taint_api.TaintApi.add_global_symbol_taint p global_symbol_layout taint_api in
  let func_interface_list, taint_infer_result =
    Taint_type_infer.TaintTypeInfer.infer range_infer_result general_interface_list taint_api
  in
  Taint_type_infer.TaintTypeInfer.state_list_to_file (get_related_filename !program_name "out" "taint_infer") taint_infer_result;
  Taint_type_infer.TaintTypeInfer.FuncInterface.interface_list_to_file (get_related_filename !program_name "out" "interface") func_interface_list
