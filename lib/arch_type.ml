open Pretty_print
open Entry_type
open Reg_type
open Mem_type
open Smt_emitter
open Isa
open Single_exp

module CondType (Entry: EntryType) = struct
  exception CondTypeError of string
  let cond_type_error msg = raise (CondTypeError ("[Cond Type Error] " ^ msg))

  type entry_t = Entry.t

  type cond = 
    | Eq | Ne
    | Le | Lt (* Signed comparison *)
    | Be | Bt (* Unsigned comparison *)

  type t = cond * entry_t * entry_t

  let not_cond_type (cond: t) : t =
    match cond with
    | (Eq, l, r) -> (Ne, l, r)
    | (Ne, l, r) -> (Eq, l, r)
    | (Le, l, r) -> (Lt, r, l)
    | (Lt, l, r) -> (Le, r, l)
    | (Be, l, r) -> (Bt, r, l)
    | (Bt, l, r) -> (Be, l, r)

  let to_string (cond: t) =
    let c, l, r = cond in
    let s = match c with
    | Eq -> "Eq" | Ne -> "Ne"
    | Le -> "Le" | Lt -> "Lt"
    | Be -> "Be" | Bt -> "Bt"
    in
    s ^ "(" ^ (Entry.to_string l) ^ "," ^ (Entry.to_string r) ^ ")"

  let pp_cond (lvl: int) (cond: t) =
    PP.print_lvl lvl "%s\n" (to_string cond)

  let pp_cond_list (lvl: int) (cond_list: t list) =
    PP.print_lvl lvl "<Cond list>\n";
    List.iter (fun x -> pp_cond (lvl + 1) x) (List.rev cond_list)

end

module ArchType (Entry: EntryType) = struct
  exception ArchTypeError of string
  let arch_type_error msg = raise (ArchTypeError ("[Arch Type Error] " ^ msg))

  type entry_t = Entry.t

  module RegType = RegType (Entry)
  module MemType = MemType (Entry)
  module CondType = CondType (Entry)

  type t = {
    pc: int;
    reg_type: RegType.t;
    mem_type: MemType.t;
    branch_hist: CondType.t;
    (* smt_ctx: SmtEmitter.t; *)
    local_var_map: (Isa.imm_var_id * SingleExp.t) list; (* TODO: change this to a better structure*)
    useful_var: SingleExp.SingleVarSet.t
    (* Maybe add constraint set here!!! *)
  }

  let type_prop_non_branch
      (smt_ctx: SmtEmitter.t)
      (curr_type: t) :
      t * ((SingleExp.t * int64) list)


end
