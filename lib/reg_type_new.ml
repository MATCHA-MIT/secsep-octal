open Isa_basic
open Entry_type
open Single_exp
open Pretty_print
open Sexplib.Std

module RegType (Entry: EntryType) = struct
  exception RegTypeError of string
  let reg_type_error msg = raise (RegTypeError ("[Reg Type Error] " ^ msg))

  type entry_t = Entry.t
  [@@deriving sexp]

  type t = entry_t list
  [@@deriving sexp]

  let pp_reg_type (lvl: int) (reg: t) =
    PP.print_lvl lvl "<RegType>\n";
    List.iteri (
      fun i entry -> 
        PP.print_lvl (lvl + 1) "<%s>\t%s\n" (IsaBasic.string_of_reg_idx i) (Entry.to_string entry)
    ) reg

  let pp_ocaml_reg_type (lvl: int) (buf: Buffer.t) (reg: t) : unit =
    PP.bprint_lvl lvl buf "[\n";
    List.iter (
      fun entry ->
        PP.bprint_lvl (lvl + 1) buf "%s;\n" (Entry.to_ocaml_string entry)
    ) reg;
    PP.bprint_lvl lvl buf "]\n"

  let get_reg_type (reg_type: t) (r: IsaBasic.register) : entry_t =
    let reg_idx = IsaBasic.get_reg_idx r in
    let entry_type = List.nth reg_type reg_idx in
    if IsaBasic.is_xmm r then
      match Entry.get_single_exp entry_type with
      | SingleConst 0L -> entry_type
      | _ -> Entry.set_taint_with_other (Entry.get_top_untaint_type ()) entry_type
    else
      let off, size = IsaBasic.get_reg_offset_size r in
      let full_size = IsaBasic.get_reg_full_size r in
      Entry.read_val (Some full_size) off size entry_type

  let set_reg_type_helper (allow_set_xmm: bool) (reg_type: t) (r: IsaBasic.register) (new_type: entry_t) : t =
    let reg_idx = IsaBasic.get_reg_idx r in
    if IsaBasic.is_xmm r then
      (* Note: taint of xmm reg should only depends on the incoming new val!!!
        e.g. movq rdx xmm, the new taint of xmm should be taint of rdx, xmm is overwrite by zero extended value of rdx. *)
      let new_type = 
        if allow_set_xmm && SingleExp.cmp (Entry.get_single_exp new_type) (SingleConst 0L) = 0 then
          new_type
        else
          Entry.set_taint_with_other (Entry.get_top_untaint_type ()) new_type 
      in
      List.mapi (fun idx r -> if idx = reg_idx then new_type else r) reg_type
    else
      let old_full_reg_type = List.nth reg_type reg_idx in
      let off, write_size = IsaBasic.get_reg_offset_size r in
      let full_size = IsaBasic.get_reg_full_size r in
      let new_type = Entry.read_val None 0L write_size new_type in
      let new_type = match write_size with
      | 1L | 2L -> Entry.write_gpr_partial off write_size old_full_reg_type new_type
      | 4L | 8L -> Entry.ext_val Entry.ZeroExt off full_size new_type
      | _ -> reg_type_error "set_reg_type_helper: expecting reg size of 1/2/4/8"
      in
      List.mapi (fun idx r -> if idx = reg_idx then new_type else r) reg_type

  let set_reg_type = set_reg_type_helper false

  let set_reg_type_allow_set_xmm = set_reg_type_helper true

  let init_reg_type (start_var: entry_t) : entry_t * t =
    let rec helper (var: entry_t) (r_type: t) (idx: int) : entry_t * t =
      if idx = IsaBasic.total_reg_num then (var, r_type)
      else helper (Entry.next_var var) (var :: r_type) (idx + 1)
    in
    let next_var, reg_type = helper start_var [] 0 in
    (next_var, List.rev reg_type)

  let get_callee_useful_var (reg_type: t) : SingleExp.SingleVarSet.t =
    let useful_var, _ = List.fold_left (
      fun (acc: SingleExp.SingleVarSet.t * int) (entry: entry_t) ->
        let acc_useful_var, acc_id = acc in
        if IsaBasic.is_reg_idx_callee_saved acc_id || IsaBasic.is_reg_idx_return_value acc_id then
          SingleExp.SingleVarSet.union acc_useful_var 
            (SingleExp.get_vars (Entry.get_single_exp entry)),
          acc_id + 1
        else
          acc_useful_var, acc_id + 1
    ) (SingleExp.SingleVarSet.empty, 0) reg_type
    in useful_var

end
