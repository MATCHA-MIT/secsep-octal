open Taint_dep_exp
open Reg_type
open Mem_type

module DataStateType = struct
  exception DataStateTypeError of string
  let data_state_type_error msg = raise (DataStateTypeError ("[Data State Type Error] " ^ msg))
  
  type t = {
    reg_type: RegTaintType.t;
    mem_type: MemTaintDepType.t
  }

  let init_state_type 
      (init_var: TaintDepExp.t) (mem_range_type: MemRangeType.t) :
      TaintDepExp.t * t =
    let init_taint_var, init_mem_dep_var = init_var in
    let next_taint_var, reg_type = RegTaintType.init_reg_type init_taint_var in
    let next_var, mem_type = 
      MemTaintDepType.init_mem_type_from_layout 
        (next_taint_var, init_mem_dep_var) 
        mem_range_type.ptr_list mem_range_type.mem_type in
    (next_var, { reg_type = reg_type; mem_type = mem_type })

end

module DataFlow = struct
  exception DataFlowError of string
  let data_flow_error msg = raise (DataFlowError ("[Data Flow Error] " ^ msg))



end
