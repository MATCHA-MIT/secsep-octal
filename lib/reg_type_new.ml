open Isa
open Entry_type

module RegType (Entry: EntryType) = struct
  exception RegTypeError of string
  let reg_type_error msg = raise (RegTypeError ("[Reg Type Error] " ^ msg))

  type entry_t = Entry.t
  type t = entry_t list

  let get_reg_type (reg_type: t) (r: Isa.register) : entry_t =
    let reg_idx = Isa.get_reg_idx r in
    let off, size = Isa.get_reg_offset_size r in
    Entry.read_val off size (List.nth reg_type reg_idx)

  let set_reg_type (reg_type: t) (r: Isa.register) (new_type: entry_t) : t =
    let reg_idx = Isa.get_reg_idx r in
    let off, write_size = Isa.get_reg_offset_size r in
    let full_size = Isa.get_reg_full_size r in
    let new_type = Entry.read_val 0L write_size new_type in
    let new_type = Entry.ext_val Entry.ZeroExt off full_size new_type in
    List.mapi (fun idx r -> if idx = reg_idx then new_type else r) reg_type

  let init_reg_type (start_var: entry_t) : entry_t * t =
    let rec helper (var: entry_t) (r_type: t) (idx: int) : entry_t * t =
      if idx = Isa.total_reg_num then (var, r_type)
      else helper (Entry.next_var var) (var :: r_type) (idx + 1)
    in
    let next_var, reg_type = helper start_var [] 0 in
    (next_var, List.rev reg_type)

end
