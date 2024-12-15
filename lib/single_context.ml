open Smt_emitter
open Single_exp
open Cond_type_new
open Sexplib.Std

module SingleContext = struct
  exception SingleContextError of string
  let single_context_error msg = raise (SingleContextError ("[Single Context Error] " ^ msg))

  type t =
    | Cond of SingleCondType.t
    | NoOverflow of SingleExp.t
    | Or of t list
    | And of t list
  [@@deriving sexp]

  (* let to_smt_expr (smt_ctx: SmtEmitter.t) (cond: t) : SmtEmitter.exp_t =
    let ctx, _ = smt_ctx in
    let rec helper (cond: t) : SmtEmitter.exp_t =
      match cond with
      | Cond c -> SingleCondType.to_smt_expr smt_ctx c
      | Or c_list -> Z3.Boolean.mk_or ctx (List.map helper c_list)
      | And c_list -> Z3.Boolean.mk_and ctx (List.map helper c_list)
    in
    helper cond *)

  let ctx_or (l: t) (r: t) : t =
    match l, r with
    | Or or_list, other
    | other, Or or_list -> Or (other :: or_list)
    | _ -> Or [l; r]

  let rec is_val (is_val_func: SingleExp.t -> bool) (cond: t) : bool =
    match cond with
    | Cond c -> SingleCondType.is_val is_val_func c
    | NoOverflow e -> is_val_func e
    | Or c_list | And c_list ->
      List.fold_left (fun acc x -> acc && (is_val is_val_func x)) true c_list

  let rec repl (repl_func: SingleExp.t -> SingleExp.t) (cond: t) : t =
    match cond with
    | Cond c -> Cond (SingleCondType.repl repl_func c)
    | NoOverflow e -> NoOverflow (repl_func e)
    | Or c_list -> Or (List.map (repl repl_func) c_list)
    | And c_list -> And (List.map (repl repl_func) c_list)

  let rec naive_check (cond: t) : (bool, t) Either.t =
    match cond with
    | Cond c -> 
      begin match SingleCondType.naive_check c with
      | Left b -> Left b
      | Right r -> Right (Cond r)
      end
    | NoOverflow _ -> Right cond
    | Or c_list ->
      List.fold_left (
        fun (acc: (bool, t) Either.t) (x: t) ->
          match acc with
          | Left true -> Left true
          | _ ->
            begin match naive_check x with
            | Left true -> Left true
            | Left false -> acc
            | Right _ -> Right cond
            end
      ) (Left false) c_list
    | And c_list ->
      List.fold_left (
        fun (acc: (bool, t) Either.t) (x: t) ->
          match acc with
          | Left false -> Left false
          | _ ->
            begin match naive_check x with
            | Left true -> acc
            | Left false -> Left false
            | Right _ -> Right cond
            end
      ) (Left true) c_list

  let rec to_smt_expr (smt_ctx: SmtEmitter.t) (cond: t) : SmtEmitter.exp_t =
    match cond with
    | Cond c -> SingleCondType.to_smt_expr smt_ctx c
    | NoOverflow e -> 
      let z3_ctx, _ = smt_ctx in
      Z3.Boolean.mk_and z3_ctx (SmtEmitter.get_exp_no_overflow_constraint smt_ctx e)
    | Or c_list -> 
      let z3_ctx, _ = smt_ctx in
      Z3.Boolean.mk_or z3_ctx (List.map (to_smt_expr smt_ctx) c_list)
    | And c_list -> 
      let z3_ctx, _ = smt_ctx in
      Z3.Boolean.mk_and z3_ctx (List.map (to_smt_expr smt_ctx) c_list)

  let add_assertions (smt_ctx: SmtEmitter.t) (cond_list: t list) : unit =
    SmtEmitter.add_assertions smt_ctx (List.map (to_smt_expr smt_ctx) cond_list)

  let check (is_quick: bool) (smt_ctx: SmtEmitter.t) (cond_list: t list) : SmtEmitter.sat_result_t =

    (* choose from one of two versions below *)

    let res: SmtEmitter.sat_result_t = begin
    (* if List.find_opt (fun x -> naive_check_impossible x) cond_list <> None then
      SatNo *)

    
    let known_list, unknown_list = List.partition_map naive_check cond_list in
    if List.find_opt (fun x -> not x) known_list <> None then
      SatNo (* If any no is found, then it is definitely not satisfied *)
    

    else begin

      (* choose from one of two versions below *)

      (* is_quick = true -> accept quick check, but may ignore overflow/underflow *)
      let exp_list = List.map (to_smt_expr smt_ctx) (if is_quick then unknown_list else cond_list) in

      (* let exp_list = List.map (to_smt_expr smt_ctx) unknown_list in *)

      if List.length exp_list = 0 then
        SatYes
      else begin
        (* Printf.printf "check call smt %s\n" (String.concat "; " (List.map to_string unknown_list)); *)
        SmtEmitter.check_compliance smt_ctx exp_list
      end
    end
    end in
    (* Printf.printf ">>>\n";
    Printf.printf "check\ninputs:\n";
    List.iter (fun x -> Printf.printf "  %s\n" (to_string x)) cond_list;
    Printf.printf "output: %s\n" (match res with SatYes -> "SatYes" | SatNo -> "SatNo" | SatUnknown -> "SatUnknown");
    Printf.printf "<<<\n"; *)
    res

  let check_or_assert
      (smt_ctx: SmtEmitter.t) (cond_list: t list) : (t list) option =
    (* If some cond in cond_list is SatNo, return None
      If all cond in cond_list is SatYes, return Some [], no constraint is added to smt_ctx 
      If some cond in cond_list is Unknown, assert it to be true in smt_ctx, return Some list contain it;
      We return the list of minimal extra assertions need to add to smt_ctx to make cond_list hold *)
    let helper (acc: (t list) option) (cond: t) : (t list) option =
      match acc with
      | None -> None
      | Some acc_cond_list ->
        begin match check false smt_ctx [cond] with
        | SatYes -> acc
        | SatNo -> 
          let check_ctx = SmtEmitter.check_context smt_ctx in
          if check_ctx = SatYes then begin
            Printf.printf "check_or_assert: unsat cond \n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_t cond));
            None
          end else begin
            SmtEmitter.pp_smt_ctx 0 smt_ctx;
            single_context_error "check_or_assert: smt ctx not sat\n"
          end
        | _ -> 
          SmtEmitter.add_assertions smt_ctx [to_smt_expr smt_ctx cond];
          Some (cond :: acc_cond_list)
        end
    in
    List.fold_left helper (Some []) cond_list

  let rec try_sub_sol
      (sub_sol_func: (SingleExp.t) -> (SingleExp.t * SingleExp.t) option)
      (cond: t) : t option =
    match cond with
    | Cond c ->
      begin match SingleCondType.try_sub_sol sub_sol_func c with
      | Some c_sub -> Some (Cond c_sub)
      | None -> None
      end
    | NoOverflow e ->
      begin match sub_sol_func e with
      | Some _ -> Some (NoOverflow e) (* We do not try sub sol for this *)
      | None -> None (* Just use this to decide whether sol are resolved *)
      end
    | Or c_list ->
      let sub_c_list = List.filter_map (try_sub_sol sub_sol_func) c_list in
      if List.length sub_c_list < List.length c_list then None
      else Some (Or sub_c_list)
    | And c_list ->
      let sub_c_list = List.filter_map (try_sub_sol sub_sol_func) c_list in
      if List.length sub_c_list < List.length c_list then None
      else Some (And sub_c_list)

  let sub_check_or_filter
      (is_quick: bool)
      (smt_ctx: SmtEmitter.t) 
      (cond_list: (t * t) list) : (t list, t) Either.t =
    (* If cond_list might be sat, return Left (list of unknown cond)
      else, return Right (unsat cond)*)
    let helper (acc: (t list, t) Either.t) (cond_pair: t * t) : (t list, t) Either.t =
      let cond, simp_cond = cond_pair in
      match acc with
      | Left acc_cond_list ->
        begin match check is_quick smt_ctx [cond] with
        | SatYes -> acc
        | SatNo -> Right cond
        | _ -> Left (simp_cond :: acc_cond_list)
        end
      | Right _ -> acc
    in
    List.fold_left helper (Left []) cond_list

end
