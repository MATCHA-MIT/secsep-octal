open Isa
open Type_exp

module RegType = struct
  exception RegTypeError of string
  let reg_type_error msg = raise (RegTypeError ("[Reg Type Error] " ^ msg))

  type t = TypeExp.t list

  let get_reg_type (reg_type: t) (r: Isa.register) : TypeExp.t =
    let reg_idx = Isa.get_reg_idx r in
    List.nth reg_type reg_idx

  let set_reg_type (reg_type: t) (r: Isa.register) (new_type: TypeExp.t) : t =
    let reg_idx = Isa.get_reg_idx r in
    List.mapi (fun idx r -> if idx = reg_idx then new_type else r) reg_type

  let init_reg_type (start_var_idx: (TypeExp.type_var_id, Isa.imm_var_id) Either.t) : ((TypeExp.type_var_id, Isa.imm_var_id) Either.t) * t = 
    let rec helper (var_idx: (TypeExp.type_var_id, Isa.imm_var_id) Either.t) (r_type: t) (idx: int) : ((TypeExp.type_var_id, Isa.imm_var_id) Either.t) * t =
      if idx = Isa.total_reg_num
      then (var_idx, r_type)
      else begin match var_idx with
      | Left type_var_idx -> helper (Left (type_var_idx + 1)) (TypeVar type_var_idx :: r_type) (idx + 1)
      | Right imm_var_idx -> helper (Right (imm_var_idx + 1)) (TypeSingle (SingleVar imm_var_idx) :: r_type) (idx + 1)
      end
    in
    let var_idx, reg_type = helper start_var_idx [] 0 in
    (var_idx, List.rev reg_type)

  

end