open Isa_basic
open Stack_layout
open Single_type_infer
open Sexplib.Std

exception BaseFuncInterfaceError of string
let base_func_interface_error msg = raise (BaseFuncInterfaceError ("[Base Func Interface Error] " ^ msg))

type entry_t = IsaBasic.label * SingleTypeInfer.ArchType.MemType.t
[@@deriving sexp]

type t = entry_t list
[@@deriving sexp]

let parse (source: string) : t =
  let open Sexplib in
  t_of_sexp (Sexp.of_string source)

let add_stack_layout (interface: t) (stack_layout: StackLayout.t) : t =
  List.map2 (
    fun (one_func: entry_t) (one_stack: StackLayout.entry_t) ->
      let label1, mem = one_func in
      let label2, stack = one_stack in
      if label1 = label2 then
        label1, (IsaBasic.get_reg_idx IsaBasic.RSP, stack) :: mem
      else
        base_func_interface_error (Printf.sprintf "Label %s and %s does not match" label1 label2)
  ) interface stack_layout
