open Single_entry_type
open Taint_exp
open Taint_entry_type
open Arch_type

module TaintSubtype = struct
  exception TaintSubtypeError of string

  let taint_subtype_error msg = raise (TaintSubtypeError ("[Taint Subtype Error] " ^ msg))

  type type_rel = {
    var_idx: TaintExp.taint_var_id;
    sol: TaintExp.t;
    subtype: TaintExp.t;
    suptype: TaintExp.t
  }

  type t = type_rel list

  module TaintEntryType = TaintEntryType (SingleEntryType)
  module ArchType = ArchType (TaintEntryType)

  type tt = TaintEntryType.t

  
end