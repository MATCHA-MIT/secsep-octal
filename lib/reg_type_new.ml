open Isa_basic
open Reg_range
open Entry_type
open Single_exp
open Pretty_print
open Sexplib.Std

module RegType (Entry: EntryType) = struct
  exception RegTypeError of string
  let reg_type_error msg = raise (RegTypeError ("[Reg Type Error] " ^ msg))

  type entry_t = Entry.t
  [@@deriving sexp]

  type range_t = RegRange.t
  [@@deriving sexp]

  type 'a reg_content = (range_t * 'a) list
  [@@deriving sexp]

  type t = entry_t reg_content
  [@@deriving sexp]

  let pp_reg_type (lvl: int) (reg: t) =
    PP.print_lvl lvl "<RegType>\n";
    List.iteri (
      fun i (valid, entry) -> 
        PP.print_lvl (lvl + 1) "<%s>\t%s\t%s\n" 
          (IsaBasic.string_of_reg_idx i) 
          (Sexplib.Sexp.to_string_hum (RegRange.sexp_of_t valid)) 
          (Entry.to_string entry)
    ) reg

  let pp_ocaml_reg_type (lvl: int) (buf: Buffer.t) (reg: t) : unit =
    PP.bprint_lvl lvl buf "[\n";
    List.iter (
      fun (_, entry) ->
        PP.bprint_lvl (lvl + 1) buf "%s;\n" (Entry.to_ocaml_string entry)
    ) reg;
    PP.bprint_lvl lvl buf "]\n"

  let map (func: 'a -> 'b) (reg: 'a reg_content) : 'b reg_content =
    List.map (fun (valid, e) -> valid, func e) reg

  let map2entry (func: 'a -> 'b -> 'c) (reg1: 'a reg_content) (reg2: 'b reg_content) : 'c list =
    List.map2 (fun (_, e1) (_, e2) -> func e1 e2) reg1 reg2

  let filter_map (func: 'a -> 'b option) (reg: 'a reg_content) : 'b reg_content =
    List.filter_map (
      fun (valid, e) ->
        func e |>
        Option.map (fun x -> valid, x)
    ) reg

  let fold_left
      (func: 'acc -> 'a -> 'acc)
      (acc: 'acc) (reg1: 'a reg_content) : 'acc =
    List.fold_left (
      fun acc (_, e) -> func acc e
    ) acc reg1

  let fold_left_map
      (func: 'acc -> 'a -> ('acc * 'b))
      (acc: 'acc)
      (reg: 'a reg_content) : 'acc * ('b reg_content) =
    List.fold_left_map (
      fun acc (valid, e) ->
        let acc, e = func acc e in
        acc, (valid, e)
    ) acc reg

  let fold_left2
      (func: 'acc -> 'a -> 'b -> 'acc)
      (acc: 'acc) (reg1: 'a reg_content) (reg2: 'b reg_content) : 'acc =
    List.fold_left2 (
      fun acc (_, e1) (_, e2) -> func acc e1 e2
    ) acc reg1 reg2

  let find_index
      (func: 'a -> bool) (reg: 'a reg_content) : int option =
    List.find_index (
      fun (_, e) -> func e
    ) reg

  let get_reg_type (reg_type: t) (r: IsaBasic.register) : entry_t =
    let reg_idx = IsaBasic.get_reg_idx r in
    let (_, entry_type) = List.nth reg_type reg_idx in
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
      let new_range = RegRange.RangeConst IsaBasic.xmm_full_off_size in
      List.mapi (fun idx r -> if idx = reg_idx then (new_range, new_type) else r) reg_type
    else
      let old_range, old_full_reg_type = List.nth reg_type reg_idx in
      let off, write_size = IsaBasic.get_reg_offset_size r in
      let full_size = IsaBasic.get_reg_full_size r in
      let new_type = Entry.read_val None 0L write_size new_type in
      let new_type = match write_size with
      | 1L | 2L -> Entry.write_gpr_partial off write_size old_full_reg_type new_type
      | 4L | 8L -> Entry.ext_val Entry.ZeroExt off full_size new_type
      | _ -> reg_type_error "set_reg_type_helper: expecting reg size of 1/2/4/8"
      in
      let write_range, write_is_full = match write_size with
      | 1L | 2L -> (off, Int64.add off write_size), false
      | 4L | 8L -> IsaBasic.gpr_full_off_size, true
      | _ -> reg_type_error "set_reg_type_helper: expecting reg size of 1/2/4/8"
      in
      let new_range = RegRange.write_update old_range write_range write_is_full in
      List.mapi (fun idx r -> if idx = reg_idx then (new_range, new_type) else r) reg_type

  let set_reg_type = set_reg_type_helper false

  let set_reg_type_allow_set_xmm = set_reg_type_helper true

  let init_reg_type (start_var: entry_t) : entry_t * t =
    let rec helper (var: entry_t) (r_type: t) (idx: int) : entry_t * t =
      if idx = IsaBasic.total_reg_num then (var, r_type)
      else helper (Entry.next_var var) ((RegRange.get_empty_range(), var) :: r_type) (idx + 1)
    in
    let next_var, reg_type = helper start_var [] 0 in
    (next_var, List.rev reg_type)

  let get_callee_useful_var (reg_type: t) : SingleExp.SingleVarSet.t =
    let useful_var, _ = List.fold_left (
      fun (acc: SingleExp.SingleVarSet.t * int) (_, entry) ->
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
