open Isa_basic
open Single_entry_type
open Mem_type_new
open Sexplib.Std

module StackLayout = struct
  exception StackLayoutError of string
  let stack_layout_error msg = raise (StackLayoutError ("[Stack Layout Error] " ^ msg))

  module MemType = MemType (SingleEntryType)

  type entry_t = IsaBasic.label * ((SingleEntryType.t MemTypeBasic.mem_slot) list)
  [@@deriving sexp]

  type t = entry_t list
  [@@deriving sexp]

  let from_file (filename: string) : t =
    let open Sexplib in
    let channel = open_in filename in
    let s_exp = Sexp.input_sexp channel in
    t_of_sexp s_exp

  let test () : unit = 
    let x : t = [
      "hh", [
        (SingleVar 0, SingleVar 0), RangeConst [], SingleTop;
        (SingleVar 0, SingleVar 0), RangeConst [], SingleTop;
      ];
      "bb", [
        (SingleVar 0, SingleVar 0), RangeConst [], SingleTop;
      ]
    ] in
    Printf.printf "%s" (Sexplib.Sexp.to_string_hum (sexp_of_t x))

end

module GlobalSymbolLayout = struct
  exception StackLayoutError of string
  let global_symbol_layout_error msg = raise (StackLayoutError ("[Global Symbol Layout Error] " ^ msg))

  module MemType = MemType (SingleEntryType)

  type entry_t = IsaBasic.label * ((SingleEntryType.t MemTypeBasic.mem_slot) list)
  [@@deriving sexp]

  type t = entry_t list
  [@@deriving sexp]

  let from_file (filename: string) : t =
    let open Sexplib in
    let channel = open_in filename in
    let s_exp = Sexp.input_sexp channel in
    t_of_sexp s_exp

end
