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
    let off, size = IsaBasic.get_reg_offset_size r in
    Entry.read_val off size (List.nth reg_type reg_idx)

  let set_reg_type (reg_type: t) (r: IsaBasic.register) (new_type: entry_t) : t =
    let reg_idx = IsaBasic.get_reg_idx r in
    let off, write_size = IsaBasic.get_reg_offset_size r in
    let full_size = IsaBasic.get_reg_full_size r in
    let new_type = Entry.read_val 0L write_size new_type in
    let new_type = Entry.ext_val Entry.ZeroExt off full_size new_type in
    List.mapi (fun idx r -> if idx = reg_idx then new_type else r) reg_type

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
        if IsaBasic.is_reg_idx_callee_saved acc_id then
          SingleExp.SingleVarSet.union acc_useful_var 
            (SingleExp.get_vars (Entry.get_single_exp entry)),
          acc_id + 1
        else
          acc_useful_var, acc_id + 1
    ) (SingleExp.SingleVarSet.empty, 0) reg_type
    in useful_var

end
