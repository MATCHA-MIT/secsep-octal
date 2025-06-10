open Isa_basic
open Single_entry_type
open Mem_type_new
open Sexplib.Std

module StackLayout = struct
  exception StackLayoutError of string
  let stack_layout_error msg = raise (StackLayoutError ("[Stack Layout Error] " ^ msg))

  module MemType = MemType (SingleEntryType)

  type layout_t = ((SingleEntryType.t * bool) MemTypeBasic.mem_slot) list
  [@@deriving sexp]

  type entry_t = IsaBasic.label * layout_t
  [@@deriving sexp]

  type t = entry_t list
  [@@deriving sexp]

  let from_file (filename: string) : t =
    let open Sexplib in
    let channel = open_in filename in
    let s_exp = Sexp.input_sexp channel in
    t_of_sexp s_exp

  let split_layout 
      (one_layout: ((SingleEntryType.t * bool) MemTypeBasic.mem_slot) list) :
      ((SingleEntryType.t MemTypeBasic.mem_slot) list) *
      ((bool MemTypeBasic.mem_slot) list) =
    List.map (
      fun (off, range, (entry, is_spill)) ->
        (off, range, entry), (off, range, is_spill)
    ) one_layout |> List.split

  let test () : unit = 
    let x : t = [
      "hh", [
        (SingleVar 0, SingleVar 0), RangeConst [], (SingleTop, true);
        (SingleVar 0, SingleVar 0), RangeConst [], (SingleTop, true);
      ];
      "bb", [
        (SingleVar 0, SingleVar 0), RangeConst [], (SingleTop, false);
      ]
    ] in
    Printf.printf "!!!\n%s%!\n" (Sexplib.Sexp.to_string_hum (sexp_of_t x))


end

module GlobalSymbolLayout = struct
  exception StackLayoutError of string
  let global_symbol_layout_error msg = raise (StackLayoutError ("[Global Symbol Layout Error] " ^ msg))

  (* contains layout information (offsets) and taint initialization (TaintExp.t in the value) *)
  module MemType = MemType (Taint_entry_type.TaintEntryType)

  type entry_t = IsaBasic.label * ((Taint_entry_type.TaintEntryType.t MemTypeBasic.mem_slot) list)
  [@@deriving sexp]

  type t = entry_t list
  [@@deriving sexp]

  let from_file (filename: string) : t =
    let open Sexplib in
    let channel = open_in filename in
    let s_exp = Sexp.input_sexp channel in
    t_of_sexp s_exp

end
