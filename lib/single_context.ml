open Smt_emitter
open Single_exp
open Cond_type_new
open Sexplib.Std

module SingleContext = struct
  exception SingleContextError of string
  let single_context_error msg = raise (SingleContextError ("[Single Context Error] " ^ msg))

  type t =
    | Cond of SingleCondType.t
    | Or of t list
    | And of t list
  [@@deriving sexp]

  let to_smt_expr (smt_ctx: SmtEmitter.t) (cond: t) : SmtEmitter.exp_t =
    let ctx, _ = smt_ctx in
    let rec helper (cond: t) : SmtEmitter.exp_t =
      match cond with
      | Cond c -> SingleCondType.to_smt_expr smt_ctx c
      | Or c_list -> Z3.Boolean.mk_or ctx (List.map helper c_list)
      | And c_list -> Z3.Boolean.mk_and ctx (List.map helper c_list)
    in
    helper cond

  let rec is_val (is_val_func: SingleExp.t -> bool) (cond: t) : bool =
    match cond with
    | Cond c -> SingleCondType.is_val is_val_func c
    | Or c_list | And c_list ->
      List.fold_left (fun acc x -> acc && (is_val is_val_func x)) true c_list

  let rec repl (repl_func: SingleExp.t -> SingleExp.t) (cond: t) : t =
    match cond with
    | Cond c -> Cond (SingleCondType.repl repl_func c)
    | Or c_list -> Or (List.map (repl repl_func) c_list)
    | And c_list -> And (List.map (repl repl_func) c_list)

end
