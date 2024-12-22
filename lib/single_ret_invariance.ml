open Isa_basic
open Single_exp
open Single_entry_type
open Arch_type
open Range_exp
open Single_context
open Single_subtype
open Single_block_invariance
open Smt_emitter
open Sexplib.Std

module SingleRetInvariance = struct
  exception SingleRetInvarianceError of string

  let single_ret_invariance_error msg = raise (SingleRetInvarianceError ("[Single Ret Invaraince] " ^ msg))

  module ArchType = ArchType (SingleEntryType)

  type subtype_t = IsaBasic.imm_var_id * (SingleExp.t list)
  [@@deriving sexp]

  let find_and_bound (e_list: SingleExp.t list) : int64 option =
    let find_one_bound (e: SingleExp.t) : int64 option =
      let mask_value_list =
        List.init 32 (
          fun (x: int) ->
            Int64.sub (Int64.shift_left 1L (x + 1)) 1L
        )
      in
      match e with
      | SingleBExp (SingleAnd, _, SingleConst c)
      | SingleBExp (SingleAnd, SingleConst c, _) ->
        if List.find_opt (fun x -> c = x) mask_value_list <> None then
          Some c
        else None
      | _ -> None
    in
    let bound_list = List.filter_map find_one_bound e_list in
    List.fold_left (
      fun (acc: int64 option) (bound : int64) -> 
        match acc with
        | None -> Some bound
        | Some acc_bound -> Some (Int64.min acc_bound bound)
    ) None bound_list

  let get_ret_subtype_and_heuristic_range 
      (var_idx: IsaBasic.imm_var_id) (sol_list: SingleExp.t list) :
      SingleContext.t list =
    match find_and_bound sol_list with
    | Some bound -> [
        Cond (Le, SingleConst 0L, SingleVar var_idx);
        Cond (Le, SingleVar var_idx, SingleConst bound);
      ]
    | None -> []

  let process_one_ret_val
      (sub_sol_func: SingleExp.t -> RangeExp.t)
      (e: SingleExp.t) :
      SingleExp.t * (((IsaBasic.imm_var_id * (SingleExp.t list)) * (SingleContext.t list)) option) =
    match e with
    | SingleTop -> SingleTop, None
    | SingleVar var_idx ->
      begin match sub_sol_func e with
      | Single exp -> exp, None
      | Range _ | Top -> SingleTop, None
      | SingleSet e_list ->
        e,
        Some ((var_idx, e_list), get_ret_subtype_and_heuristic_range var_idx e_list)
      end
    | _ -> single_ret_invariance_error "process_ret_val: ret val should not be other expression"

  let process_ret_state
      (sub_sol_func: SingleExp.t -> RangeExp.t)
      (a_type: ArchType.t) :
      ArchType.t * ((IsaBasic.imm_var_id * (SingleExp.t list)) list) * (SingleContext.t list list) =
    let reg_type, reg_other_info = 
      List.map (process_one_ret_val sub_sol_func) a_type.reg_type |> List.split
    in
    let sub_list, tmp_ctx =
      List.filter_map (fun x -> x) reg_other_info |> List.split
    in
    let (sub_list, tmp_ctx), mem_type =
      ArchType.MemType.fold_left_map (
        fun (acc: ((IsaBasic.imm_var_id * (SingleExp.t list)) list) * (SingleContext.t list list)) (entry: SingleExp.t) ->
          match process_one_ret_val sub_sol_func entry with
          | new_entry, None -> acc, new_entry
          | new_entry, Some (new_sub, new_ctx) ->
            let acc_sub, acc_ctx = acc in
            (new_sub :: acc_sub, new_ctx :: acc_ctx), new_entry
      ) (sub_list, tmp_ctx) a_type.mem_type
    in
    { a_type with reg_type = reg_type; mem_type = mem_type },
    sub_list, tmp_ctx

  let update_ret_block
      (rev_func_type: ArchType.t list)
      (new_ret_type: ArchType.t) : ArchType.t list =
    match rev_func_type with
    | [] -> single_ret_invariance_error "update_ret_block: get empty func type"
    | old_ret_type :: other_type ->
      if old_ret_type.label = IsaBasic.ret_label && new_ret_type.label = IsaBasic.ret_label then
        new_ret_type :: other_type
      else single_ret_invariance_error "update_ret_block: block is not ret block"
  
  let gen_ret_invariance
      (smt_ctx: SmtEmitter.t)
      (sub_sol_func: (SingleExp.t * int) -> RangeExp.t)
      (input_var_set: SingleExp.SingleVarSet.t)
      (func_type: ArchType.t list)
      (block_subtype_list: ArchType.block_subtype_t list)
      (single_subtype: SingleSubtype.t) :
      ArchType.t list * ((IsaBasic.imm_var_id * (SingleExp.t list)) list) =
    let rev_func_type = List.rev func_type in
    let rev_block_subtype_list = List.rev block_subtype_list in
    let ret_block = List.hd rev_func_type in
    let sub_sol_func (e: SingleExp.t) : RangeExp.t =
      sub_sol_func (e, ret_block.pc)
    in

    let _, sub_list, tmp_ctx = process_ret_state sub_sol_func ret_block in
    (* let rev_func_type = update_ret_block rev_func_type ret_block in *)
    (* Printf.printf "rev_func_type\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list ArchType.sexp_of_t rev_func_type));
    Printf.printf "rev_block_subtype_list\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list ArchType.sexp_of_block_subtype_t rev_block_subtype_list)); *)
    Printf.printf "tmp_ctx\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list SingleContext.sexp_of_t (List.concat tmp_ctx)));
    let rev_func_type, remain_tmp_ctx_opt_list =
      List.fold_left_map (
        fun (rev_func_type: ArchType.t list) (tmp_ctx: SingleContext.t list) ->
          let ret_block = { (List.hd rev_func_type) with tmp_context = tmp_ctx } in
          let new_rev_func_type = update_ret_block rev_func_type ret_block in
          let new_rev_func_type, success = 
            SingleBlockInvariance.solve smt_ctx 
              true (* Note: By setting this option to be true, we allow aggressive guess on return value range that constrains input value range *)
              input_var_set new_rev_func_type rev_block_subtype_list single_subtype 100 
            in
          if success then
            new_rev_func_type, None
          else rev_func_type, Some tmp_ctx
      ) rev_func_type tmp_ctx
    in
    let remain_ctx = List.filter_map (fun x -> x) remain_tmp_ctx_opt_list in
    Printf.printf "SingleRetInvariance: %d tmp_ctx, %d remain_ctx\n" (List.length tmp_ctx) (List.length remain_ctx);
    List.rev rev_func_type, sub_list



end
