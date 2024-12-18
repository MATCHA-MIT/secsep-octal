open Isa_basic
open Single_exp
open Single_entry_type
open Mem_offset_new
open Range_exp
open Single_context
open Arch_type
open Smt_emitter
open Pretty_print
open Sexplib.Std

module SingleSol = struct
  exception SingleSolError of string

  let single_sol_error msg = raise (SingleSolError ("[Single Sol Error] " ^ msg))

  type t =
    | SolNone
    | SolSimple of RangeExp.t
    | SolCond of int * RangeExp.t * RangeExp.t * RangeExp.t
      (* cond_pc * (sol of pc < cond_pc) * (sol of pc = cond_pc (taken)) * (sol of pc > cond_pc (not taken)) *)
      (* This is used to simplify inference process, not for type check!!! *)
  [@@deriving sexp]

  let to_string (e: t) : string =
    match e with
    | SolNone -> "SolNone"
    | SolSimple e -> Printf.sprintf "SolSimple(%s)" (RangeExp.to_string e)
    | SolCond (pc, e1, e2, e3) -> Printf.sprintf "SolCond(%d)(%s, %s, %s)" pc (RangeExp.to_string e1) (RangeExp.to_string e2) (RangeExp.to_string e3)

  let to_ocaml_string (e: t) : string =
    match e with
    | SolNone -> "SolNone"
    | SolSimple e -> Printf.sprintf "SolSimple (%s)" (RangeExp.to_ocaml_string e)
    | SolCond (pc, e1, e2, e3) -> 
      Printf.sprintf "SolCond (%d, %s, %s, %s)" pc (RangeExp.to_ocaml_string e1) (RangeExp.to_ocaml_string e2) (RangeExp.to_ocaml_string e3)

  let to_smt_expr (smt_ctx: SmtEmitter.t) (v_idx: int) (s: t) : SmtEmitter.exp_t =
    match s with
    | SolNone | SolSimple Top | SolSimple (Single SingleTop) -> SmtEmitter.mk_true smt_ctx
      (* RangeExp.to_smt_expr smt_ctx v_idx RangeExp.Top *)
    | SolSimple e
    | SolCond (_, e, _, _) -> RangeExp.to_smt_expr smt_ctx v_idx e

  let to_context (v_idx: int) (s: t) : SingleContext.t option =
    match s with
    | SolNone -> None
    | SolSimple e
    | SolCond (_, e, _, _) -> RangeExp.to_context v_idx e

end

module SingleSubtype = struct
  exception SingleSubtypeError of string

  let single_subtype_error msg = raise (SingleSubtypeError ("[Single Subtype Error] " ^ msg))

  type var_pc_t = IsaBasic.imm_var_id * int
  [@@deriving sexp]
  type var_pc_list_t = IsaBasic.imm_var_id * (int list) (* supertype var, path pc list *)
  [@@deriving sexp]
  type type_pc_t = SingleEntryType.t * int
  type type_pc_list_t = SingleEntryType.t * (int list) (* subtype, path pc list *)
  [@@deriving sexp]

  let rec list_tl (x: 'a list) : 'a =
    match x with
    | [] -> single_subtype_error "list_tl cannot get tl of empty list"
    | hd :: [] -> hd
    | _ :: tl -> list_tl tl

  type type_rel = {
    var_idx: var_pc_t;
    sol: SingleSol.t;
    other_constraint: SingleContext.t list;
    subtype_list: type_pc_list_t list; (* subtype, pc of the branch that jumps to var_idx's block *)
    supertype_list: var_pc_list_t list; (* NOTE: I omit br hist here since I noticed that current implementation cannot use it anyway *)
    set_sol: type_pc_list_t list; (* Dirty trick to record information to resolve relation between vars whose solution is a set of values; is empty list if not used *)
  }
  [@@deriving sexp]

  type t = type_rel list
  [@@deriving sexp]

  module ArchType = ArchType (SingleEntryType)

  let pp_type_rel (lvl: int) (x: type_rel) =
    let var_idx, var_pc = x.var_idx in
    PP.print_lvl lvl "<SymImm %d> at pc %d\n" var_idx var_pc;
    PP.print_lvl (lvl + 1) "Sol: %s\n" (SingleSol.to_string x.sol);
    PP.print_lvl (lvl + 1) "Subtype: [\n";
    List.iter (fun (sub, pc) -> PP.print_lvl (lvl + 2) "%s, (%s);\n" (SingleEntryType.to_string sub) (Sexplib.Sexp.to_string (sexp_of_list sexp_of_int pc))) x.subtype_list;
    PP.print_lvl (lvl + 1) "]\n";
    PP.print_lvl (lvl + 1) "Supertype: [%s]\n" 
      (String.concat "; " (List.map (fun (v, pc) -> Printf.sprintf "%d,(%s)" v (Sexplib.Sexp.to_string (sexp_of_list sexp_of_int pc))) x.supertype_list));
    PP.print_lvl (lvl + 1) "Sol set: %s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list sexp_of_type_pc_list_t x.set_sol))

  let pp_single_subtype (lvl: int) (tv_rels: t) =
    List.iter (fun x -> pp_type_rel lvl x) tv_rels

  let to_ocaml_string (x: type_rel) : string =
    let v_idx, v_pc = x.var_idx in
    Printf.sprintf "{ var_idx = (%d, %d); sol = %s; subtype_list = []; supertype_list = [] }"
      v_idx v_pc (SingleSol.to_ocaml_string x.sol)

  let pp_ocaml_single_subtype (lvl: int) (buf: Buffer.t) (x: t) =
    PP.bprint_lvl lvl buf "[\n";
    List.iter (
      fun x -> PP.bprint_lvl (lvl + 1) buf "%s;\n" (to_ocaml_string x)
    ) x;
    PP.bprint_lvl lvl buf "]\n"

  let find_or_add_entry (var_pc_map: var_pc_t list) (tv_rel: t) (var_idx: int) : t * type_rel =
    let find_entry = List.find_opt (fun x -> let v_idx, _ = x.var_idx in v_idx = var_idx) tv_rel in
    match find_entry with
    | Some entry -> tv_rel, entry
    | None ->
      let entry = 
        { var_idx = List.find (fun (v_idx, _) -> v_idx = var_idx) var_pc_map; 
        sol = SingleSol.SolNone; 
        other_constraint = [];
        subtype_list = []; supertype_list = [];
        set_sol = [] } 
      in
      entry :: tv_rel, entry

  let filter_entry (tv_rel: t) (var_set: SingleExp.SingleVarSet.t) : t =
    List.filter (
      fun (x: type_rel) ->
        let v_idx, _ =  x.var_idx in
        SingleExp.SingleVarSet.mem v_idx var_set
    ) tv_rel

  let type_list_insert 
      (type_list: type_pc_list_t list) (ty: type_pc_list_t) : type_pc_list_t list =
    let t_exp, t_pc = ty in
    let find_type = 
      List.find_opt (
        fun (exp, pc) -> SingleEntryType.cmp t_exp exp = 0 && t_pc = pc
      ) type_list
    in
    if find_type <> None then type_list else ty :: type_list

  let type_var_list_insert
      (type_var_list: var_pc_list_t list) (var_idx: var_pc_list_t) : var_pc_list_t list =
    if List.find_opt (fun x -> x = var_idx) type_var_list <> None then type_var_list else var_idx :: type_var_list

  let add_one_sub_type (tv_rel: type_rel) (ty: type_pc_list_t) : type_rel =
    { tv_rel with subtype_list = type_list_insert tv_rel.subtype_list ty }

  let add_one_super_type (tv_rel: type_rel) (t_idx: var_pc_list_t) : type_rel =
    { tv_rel with supertype_list = type_var_list_insert tv_rel.supertype_list t_idx }

  (* Connect a->b *)
  let add_one_sub_super (tv_rel: t) (a_exp_pc: type_pc_list_t) (b_idx: int) : t =
    match a_exp_pc with
    | SingleVar a_idx, a_pc ->
      if a_idx = b_idx then tv_rel
      else
        List.map (
          fun x ->
            let x_idx, _ = x.var_idx in
            if x_idx = a_idx then add_one_super_type x (b_idx, a_pc)
            else if x_idx = b_idx then add_one_sub_type x a_exp_pc
            else x
        ) tv_rel
    | _ ->
      List.map (
        fun x ->
          let x_idx, _ = x.var_idx in
          if x_idx = b_idx then add_one_sub_type x a_exp_pc
          else x
      ) tv_rel


  let add_sub_sub_super
      (var_pc_map: var_pc_t list)
      (tv_rel: t) (a_exp_pc: type_pc_list_t) (b_idx: int) : t =
    match a_exp_pc with
    | SingleVar a_idx, a_pc_list ->
    (* | SingleVar a_idx, _ -> *)
      let tv_rel, a_entry = find_or_add_entry var_pc_map tv_rel a_idx in
      let tv_rel =
        List.fold_left (
          fun acc_tv_rel (sub_a_exp, sub_a_pc_list) ->
            add_one_sub_super acc_tv_rel (sub_a_exp, sub_a_pc_list @ a_pc_list) b_idx
            (* add_one_sub_super acc_tv_rel (sub_a_exp, sub_a_pc_list) b_idx *)
        ) tv_rel a_entry.subtype_list in
      add_one_sub_super tv_rel a_exp_pc b_idx
    | _ -> add_one_sub_super tv_rel a_exp_pc b_idx

  let add_sub_sub_super_super
      (var_pc_map: var_pc_t list)
      (tv_rel: t) (a_exp_pc: type_pc_list_t) (b_idx: int) : t =
    let tv_rel, b_entry = find_or_add_entry var_pc_map tv_rel b_idx in
    let tv_rel =
      List.fold_left (
        fun acc_tv_rel (sup_b, b_pc_list) ->
        (* fun acc_tv_rel (sup_b, _) -> *)
          let a_exp, a_pc_list = a_exp_pc in
          add_sub_sub_super var_pc_map acc_tv_rel (a_exp, a_pc_list @ b_pc_list) sup_b
          (* add_sub_sub_super var_pc_map acc_tv_rel (a_exp, a_pc_list) sup_b *)
      ) tv_rel b_entry.supertype_list
    in
    add_sub_sub_super var_pc_map tv_rel a_exp_pc b_idx


  type useful_var_t = IsaBasic.label * SingleExp.SingleVarSet.t
  [@@deriving sexp]

  let pp_useful_var (lvl: int) (useful_var: useful_var_t) =
    let label, vars = useful_var in
    let var_s = String.concat "," (List.map string_of_int (SingleExp.SingleVarSet.to_list vars)) in
    PP.print_lvl lvl "%s\t%s\n" label var_s

  let pp_useful_var_list (lvl: int) (useful_var_list: useful_var_t list) =
    List.iter (pp_useful_var (lvl + 1)) useful_var_list

  let add_one_useful_var_block_subtype_helper
      (var_pc_map: var_pc_t list)
      (block_subtype: ArchType.block_subtype_t)
      (useful_var: SingleExp.SingleVarSet.t)
      (tv_rel: t) : t * (useful_var_t list) =
    let sup_block, sub_block_list = block_subtype in
    let helper (acc_tv_rel: t) (sub_block: ArchType.t) : t * useful_var_t =
      let sub_local_var_map = sub_block.local_var_map in
      let sub_pc = sub_block.pc in
      let add_one_helper 
          (acc: t * SingleEntryType.SingleVarSet.t) 
          (sub: SingleEntryType.t) (sup: SingleEntryType.t) : 
          t * SingleEntryType.SingleVarSet.t =
        let acc_tv_rel, acc_useful = acc in
        match sup with
        | SingleVar sup_idx ->
          if SingleEntryType.SingleVarSet.mem sup_idx useful_var then
            let sub = SingleEntryType.repl_local_var sub_local_var_map sub in
            add_sub_sub_super_super var_pc_map acc_tv_rel (sub, [sub_pc]) sup_idx,
            SingleEntryType.SingleVarSet.union acc_useful (SingleEntryType.get_vars sub)
          else acc
        | SingleTop -> acc
        | _ -> single_subtype_error 
          (Printf.sprintf "add_one_helper: incorrect sub/super types (%s,%d)->%s"
            (SingleEntryType.to_string sub) sub_pc (SingleEntryType.to_string sup))
      in
      let acc_tv_rel, reg_useful_var = 
        List.fold_left2 add_one_helper 
          (acc_tv_rel, SingleEntryType.SingleVarSet.empty)
          sub_block.reg_type sup_block.reg_type
      in
      let acc_tv_rel, mem_useful_var =
        ArchType.MemType.fold_left2 add_one_helper
          (acc_tv_rel, SingleEntryType.SingleVarSet.empty)
          sub_block.mem_type sup_block.mem_type
      in
      acc_tv_rel, (sub_block.label, SingleEntryType.SingleVarSet.union reg_useful_var mem_useful_var)
    in
    List.fold_left_map helper tv_rel sub_block_list

  let update_block_subtype_useful_var
      (block_subtype: ArchType.block_subtype_t list)
      (useful_var_list: useful_var_t list) :
      ArchType.block_subtype_t list * useful_var_t list =
    let helper 
        (acc: ArchType.block_subtype_t list) 
        (useful_var: useful_var_t) : 
        (ArchType.block_subtype_t list) * useful_var_t =
      let label, var_set = useful_var in
      let acc_lookup = 
        List.find_mapi (
          fun i (x: ArchType.block_subtype_t) -> 
            let x, _ = x in
            if x.label = label then Some (i, x.useful_var) else None
        ) acc
      in
      match acc_lookup with
      | Some (idx, known_useful_var) ->
        List.mapi (
          fun i (b: ArchType.block_subtype_t) ->
            let x, y = b in
            if i = idx then 
              (* x, y *)
              { x with useful_var = SingleEntryType.SingleVarSet.union known_useful_var var_set }, y 
            else b
        ) acc,
        (label, SingleEntryType.SingleVarSet.diff var_set known_useful_var)
      | None -> single_subtype_error "block not found"
    in
    let block_subtype, useful_var_list = List.fold_left_map helper block_subtype useful_var_list in
    block_subtype,
    List.filter (fun (_, var_set) -> Bool.not (SingleExp.SingleVarSet.is_empty var_set)) useful_var_list
    

  let add_one_useful_var_block_subtype
      (var_pc_map: var_pc_t list)
      (block_subtype_list: ArchType.block_subtype_t list)
      (useful_var: useful_var_t)
      (tv_rel: t) :
      t * (ArchType.block_subtype_t list) * (useful_var_t list) =
    let sup_label, sup_useful_var = useful_var in
    let block_subtype = 
      List.find (fun (x: ArchType.block_subtype_t) -> let x, _ = x in x.label = sup_label) block_subtype_list 
    in
    let tv_rel, new_useful_var_list = add_one_useful_var_block_subtype_helper var_pc_map block_subtype sup_useful_var tv_rel in
    let block_subtype_list, new_useful_var_list = update_block_subtype_useful_var block_subtype_list new_useful_var_list in
    tv_rel, block_subtype_list, new_useful_var_list

  let merge_useful_var
      (ul1: useful_var_t list) (ul2: useful_var_t list) : useful_var_t list =
    let rec insert (ul1: useful_var_t list) (u2: useful_var_t) : useful_var_t list =
      match ul1 with
      | [] -> [ u2 ]
      | (hd_lb, hd_var) :: tl ->
        let u2_lb, u2_var = u2 in
        if hd_lb = u2_lb then (hd_lb, SingleEntryType.SingleVarSet.union hd_var u2_var) :: tl
        else (hd_lb, hd_var) :: (insert tl u2)
    in
    List.fold_left insert ul1 ul2

  let rec add_all_useful_var_block_subtype
      (var_pc_map: var_pc_t list)
      (block_subtype_list: ArchType.block_subtype_t list)
      (useful_var_list: useful_var_t list)
      (* (tv_rel: t) : t * (ArchType.block_subtype_t list) = *)
      (tv_rel: t) (count: int) : t * (ArchType.block_subtype_t list) =
    (* Printf.printf "add_all_useful_var_block_subtype %d\n" count;
    Printf.printf "before\n";
    pp_useful_var_list 0 useful_var_list; *)
    match useful_var_list with
    | [] -> tv_rel, block_subtype_list
    | hd :: tl ->
      let tv_rel, block_subtype_list, new_useful_var_list =
        add_one_useful_var_block_subtype var_pc_map block_subtype_list hd tv_rel
      in
      (* Printf.printf "after\n";
      pp_useful_var_list 0 useful_var_list; *)
      let useful_var_list = merge_useful_var tl new_useful_var_list in
      (* add_all_useful_var_block_subtype var_pc_map block_subtype_list useful_var_list tv_rel *)
      (* if count > 0 then *)
        add_all_useful_var_block_subtype var_pc_map block_subtype_list useful_var_list tv_rel (count - 1)
      (* else if List.length useful_var_list > 0 then begin
        Printf.printf "Warning: useful vars not handled due to limited layers of recursive calls";
        pp_useful_var_list 0 useful_var_list;
        tv_rel, block_subtype_list
      end
      else
        tv_rel, block_subtype_list *)

  let init_useful_var_from_block_subtype
      (block_subtype_list: ArchType.block_subtype_t list) : useful_var_t list =
    List.map (
      fun (x: ArchType.block_subtype_t) ->
        let x, _ = x in
        x.label, x.useful_var
    ) block_subtype_list
    
  let init
      (func_name: string)
      (block_subtype_list: ArchType.block_subtype_t list) : t * (ArchType.block_subtype_t list) =
    let useful_var_list = init_useful_var_from_block_subtype block_subtype_list in
    let useful_var_map_list =
      List.concat_map (
        fun (block_subtype: ArchType.block_subtype_t) ->
          let a_type, _ = block_subtype in
          List.map (fun x -> (x, a_type.pc)) (SingleExp.SingleVarSet.to_list (ArchType.get_local_var_set a_type)) 
          @
          (
            if a_type.label = func_name then
              List.map (fun x -> (x, a_type.pc)) (SingleExp.SingleVarSet.to_list (a_type.global_var))
            else
              []
          )
      ) block_subtype_list
    in
    add_all_useful_var_block_subtype useful_var_map_list block_subtype_list useful_var_list [] 10

  let to_smt_expr (smt_ctx: SmtEmitter.t) (sol: type_rel) : SmtEmitter.exp_t =
    let var_idx, _ = sol.var_idx in
    (* Printf.printf "single_to_smt_expr SymImm %d %s\n" var_idx (SingleSol.to_string sol.sol); *)
    SingleSol.to_smt_expr smt_ctx var_idx sol.sol

  let update_block_smt_ctx
      (smt_ctx: SmtEmitter.t)
      (sol: t) (useful_single_var: SingleExp.SingleVarSet.t) : unit =
    List.iter (
      fun (x: type_rel) ->
        let var_idx, _ = x.var_idx in
        if SingleExp.SingleVarSet.mem var_idx useful_single_var then begin
          SmtEmitter.add_assertions smt_ctx [ to_smt_expr smt_ctx x ];
          SingleContext.add_assertions smt_ctx x.other_constraint
        end else ()
    ) sol

  let to_context (sol: type_rel) : SingleContext.t option =
    let var_idx, _ = sol.var_idx in
    SingleSol.to_context var_idx sol.sol

  let get_block_context
      (sol: t) (useful_single_var: SingleExp.SingleVarSet.t) : SingleContext.t list =
    List.concat_map (
      fun (x: type_rel) ->
        let var_idx, _ = x.var_idx in
        if SingleExp.SingleVarSet.mem var_idx useful_single_var then
          (* to_context x *)
          match to_context x with
          | Some sol_context -> sol_context :: x.other_constraint
          | None -> x.other_constraint
        else []
    ) sol

  let find_var_sol (tv_rel_list: t) (v: IsaBasic.imm_var_id) (v_pc: int) : RangeExp.t =
    (* Printf.printf "find_var_sol: %d at %d\n" v v_pc; *)
    match List.find_opt (fun (x: type_rel) -> let id, _ =  x.var_idx in v = id) tv_rel_list with
    | Some tv_rel ->
      let _, block_pc = tv_rel.var_idx in
      begin match tv_rel.sol with
      | SolNone -> Top
      | SolSimple s -> s
      | SolCond (cond_pc, exp, exp_taken, exp_not_taken) ->
        (* Printf.printf "v_pc = %d, block_pc = %d, cond_pc = %d, exp_taken = %s, exp_not_taken = %s\n" v_pc block_pc cond_pc (RangeExp.to_string exp_taken) (RangeExp.to_string exp_not_taken); *)
        if v_pc >= block_pc && v_pc < cond_pc then
          begin
            (* Printf.printf "Using %s\n" (RangeExp.to_string exp); *)
            exp
          end
        else if v_pc == cond_pc then
          begin
            (* Printf.printf "Using %s\n" (RangeExp.to_string exp_taken); *)
            Printf.printf "find_var_sol: WARNING, v_pc = cond_pc = %d\n" v_pc;
            exp_taken
          end
        else if v_pc > cond_pc then
          begin
            (* Printf.printf "Using %s\n" (RangeExp.to_string exp_not_taken); *)
            exp_not_taken
          end
        else single_subtype_error (Printf.sprintf "find_var_sol wrong exp pc var %d %d smaller than %d" v v_pc cond_pc)
      end
    | None -> Top

  let is_sol_resolved
      (is_val_helper: SingleExp.SingleVarSet.t -> 'a -> bool)
      (tv_rel_list: t)
      (input_var_set: SingleExp.SingleVarSet.t)
      (e: 'a) : bool =
    let resolved_vars = 
      List.filter_map (
        fun (x: type_rel) -> 
          (* match x.sol with
          | SolNone | SolSimple Top -> None
          | _ -> let idx, _ = x.var_idx in Some idx *)
          if x.sol <> SolNone then let idx, _ = x.var_idx in Some idx else None
      ) tv_rel_list
    in
    is_val_helper (
      SingleExp.SingleVarSet.union 
        (SingleExp.SingleVarSet.of_list resolved_vars) 
        input_var_set
    ) e

  let sub_sol_single_var
      (eval_helper: SingleExp.t -> SingleExp.t)
      (tv_rel_list: t)
      (input_var_set: SingleEntryType.SingleVarSet.t)
      (e: SingleExp.t) : SingleExp.t option =
    (* No need to use is_sol_resolved here since it already did that implicitly *)
    if SingleExp.is_val input_var_set e then Some e
    else
      let e_vars = SingleExp.get_vars e in
      let helper (acc: SingleExp.t) (tv_rel: type_rel) : SingleExp.t =
        let v, _ = tv_rel.var_idx in
        if SingleExp.SingleVarSet.mem v e_vars then
          match tv_rel.sol with
          | SolSimple (Single v_sol) ->
            SingleExp.repl_var_exp e (v, v_sol)
          | _ -> acc
        else acc
      in
      let simp_e = eval_helper (List.fold_left helper e tv_rel_list) in
      if SingleExp.is_val input_var_set simp_e then Some simp_e else None

  let sub_sol_single_set_var
      (* NOTE: this is a internal sub helper func, and eval_helper would be called by its caller,
        so we do not have eval_helper for it *)
      (tv_rel_list: t)
      (get_vars: 'a -> SingleEntryType.SingleVarSet.t)
      (repl_var_exp: 'a -> IsaBasic.imm_var_id * SingleExp.t -> 'a)
      (e: 'a) : 'a list =
    let e_vars = get_vars e in
    let helper (acc: 'a list) (tv_rel: type_rel) : 'a list =
      let v, _ = tv_rel.var_idx in
      if SingleExp.SingleVarSet.mem v e_vars then
        match tv_rel.sol with
        | SolSimple (SingleSet v_sol_list) ->
          List.concat_map (
            fun v_sol ->
              List.map (fun e -> repl_var_exp e (v, v_sol)) acc
          ) v_sol_list
        | _ -> acc
      else acc
    in
    List.fold_left helper [e] tv_rel_list

  let sub_sol_single_to_range_naive_repl
      (* NOTE: this is a internal sub helper func, and eval_helper would be called by its caller,
        so we do not have eval_helper for it *)
      (repl_set_sol: bool)
      (tv_rel_list: t)
      (input_var_set: SingleEntryType.SingleVarSet.t)
      (e_pc: int)
      (e: SingleExp.t) : RangeExp.t =
    let rec helper (e: SingleEntryType.t) : RangeExp.t =
      match e with
      | SingleTop -> Top
      | SingleConst c -> Single (SingleConst c)
      | SingleVar v ->
        if SingleEntryType.SingleVarSet.mem v input_var_set then Single e
        else begin match find_var_sol tv_rel_list v e_pc with
        | Single e_sub -> helper e_sub
        | Range (l, r, step) ->
          if SingleExp.is_val input_var_set l && SingleExp.is_val input_var_set r then
            Range (l, r, step)
          else single_subtype_error (Printf.sprintf "sub_sol_single_to_range range %s contain local var\n" (RangeExp.to_string (Range (l, r, step))))
        | SingleSet e_list ->
          if List.find_opt (fun x -> not (SingleExp.is_val input_var_set x)) e_list = None then
            if List.length e_list = 1 then Single (List.hd e_list)
            else if repl_set_sol then SingleSet e_list
            else Single (SingleVar v)
          else single_subtype_error (Printf.sprintf "sub_sol_single_to_range range %s contain local var\n" (RangeExp.to_string (SingleSet e_list)))
        | Top -> Top
        end          
      | SingleBExp (bop, e1, e2) ->
        begin match bop, helper e1, helper e2 with
        | op, Single e1, Single e2 -> Single (SingleEntryType.eval (SingleBExp (op, e1, e2)))
        | SingleAdd, Single e, Range (e1, e2, s)
        | SingleAdd, Range (e1, e2, s), Single e ->
          Range (SingleEntryType.eval (SingleBExp (SingleAdd, e, e1)), SingleEntryType.eval (SingleBExp (SingleAdd, e, e2)), s)
        | SingleAdd, Range (e11, e12, s1), Range (e21, e22, s2) ->
          (* TODO: Double check this!!! *)
          let new_s = Z.to_int64 (Z.gcd (Z.of_int64 s1) (Z.of_int64 s2)) in
          Range (SingleEntryType.eval (SingleBExp (SingleAdd, e11, e21)), SingleEntryType.eval (SingleBExp (SingleAdd, e12, e22)), new_s)
        (* | SingleAdd, Single e, SingleSet e_list
        | SingleAdd, SingleSet e_list, Single e ->
          SingleSet (List.map (fun x -> SingleEntryType.eval (SingleBExp (SingleAdd, e, x))) e_list) *)
        | SingleSub, Single e, Range (e1, e2, s) ->
          Range (SingleEntryType.eval (SingleBExp (SingleSub, e, e2)), SingleEntryType.eval (SingleBExp (SingleSub, e, e1)), s)
        | SingleSub, Range (e1, e2, s), Single e ->
          Range (SingleEntryType.eval (SingleBExp (SingleSub, e, e1)), SingleEntryType.eval (SingleBExp (SingleSub, e, e2)), s)
        (* | SingleSub, Single e, SingleSet e_list ->
          SingleSet (List.map (fun x -> SingleEntryType.eval (SingleBExp (SingleSub, e, x))) e_list)
        | SingleSub, SingleSet e_list, Single e ->
          SingleSet (List.map (fun x -> SingleEntryType.eval (SingleBExp (SingleSub, x, e))) e_list) *)
        | SingleMul, Single (SingleConst 0L), _
        | SingleMul, _, Single (SingleConst 0L) -> Single (SingleConst 0L)
        | SingleMul, Single (SingleConst c), Range (e1, e2, s)
        | SingleMul, Range (e1, e2, s), Single (SingleConst c) ->
          if c = 0L then Single (SingleConst 0L)
          else if c > 0L then
            Range (SingleEntryType.eval (SingleBExp (SingleMul, SingleConst c, e1)),
            SingleEntryType.eval (SingleBExp (SingleMul, SingleConst c, e2)),
            Int64.mul c s)
          else
            Range (SingleEntryType.eval (SingleBExp (SingleMul, SingleConst c, e2)),
            SingleEntryType.eval (SingleBExp (SingleMul, SingleConst c, e1)),
            Int64.mul c s)
        (* | SingleMul, Single e, SingleSet e_list
        | SingleMul, SingleSet e_list, Single e ->
          SingleSet (List.map (fun x -> SingleEntryType.eval (SingleBExp (SingleMul, e, x))) e_list) *)
        | SingleSar, Range (e1, e2, s), Single (SingleConst c) ->
          (* Printf.printf "Cal sar %s %s\n" (RangeExp.to_string (Range (e1, e2, s))) (RangeExp.to_string (Single (SingleConst c))); *)
          let s_sar = Int64.shift_right s (Int64.to_int c) in
          if Int64.shift_left s_sar (Int64.to_int c) = s then
            Range (SingleEntryType.eval (SingleBExp (SingleSar, e1, SingleConst c)),
              SingleEntryType.eval (SingleBExp (SingleSar, e2, SingleConst c)),
              Int64.shift_right s (Int64.to_int c))
          else 
            Range (SingleEntryType.eval (SingleBExp (SingleSar, e1, SingleConst c)),
              SingleEntryType.eval (SingleBExp (SingleSar, e2, SingleConst c)),
              1L)
        (* | SingleSar, SingleSet e_list, Single (SingleConst c) ->
          SingleSet (List.map (fun x -> SingleEntryType.eval (SingleBExp (SingleSar, x, SingleConst c))) e_list) *)
        | SingleSal, Range (e1, e2, s), Single (SingleConst c) ->
          let s_sal = Int64.shift_left s (Int64.to_int c) in
          if Int64.shift_right s_sal (Int64.to_int c) = s then
            Range (SingleEntryType.eval (SingleBExp (SingleSal, e1, SingleConst c)),
              SingleEntryType.eval (SingleBExp (SingleSal, e2, SingleConst c)),
              Int64.shift_left s (Int64.to_int c))
          else Top
        (* | SingleSal, SingleSet e_list, Single (SingleConst c) ->
          SingleSet (List.map (fun x -> SingleEntryType.eval (SingleBExp (SingleSal, x, SingleConst c))) e_list) *)
        | _, Single e, SingleSet e_list ->
          if repl_set_sol then
            SingleSet (List.map (fun x -> SingleEntryType.eval (SingleBExp (bop, e, x))) e_list)
          else begin
            Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum (SingleEntryType.sexp_of_t e));
            single_subtype_error "sub_sol_single_to_range_naive_repl: Do not expect get set operand"
          end
        | _, SingleSet e_list, Single e ->
          if repl_set_sol then
            SingleSet (List.map (fun x -> SingleEntryType.eval (SingleBExp (bop, x, e))) e_list)
          else begin
            Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum (SingleEntryType.sexp_of_t e));
            single_subtype_error "sub_sol_single_to_range_naive_repl: Do not expect get set operand"
          end
        | _, r1, r2 -> 
          Printf.printf "sub_sol_single_to_range: WARNING! %s not handled l %s r %s\n" 
            (SingleExp.to_string e) (RangeExp.to_string r1) (RangeExp.to_string r2); 
          Top (* TODO: maybe need to handle more cases here *)
        end
      | SingleUExp _ -> Top
    in
    helper e

  let sub_sol_single_to_offset_opt
      (eval_helper: SingleExp.t -> SingleExp.t)
      (tv_rel_list: t)
      (input_var_set: SingleEntryType.SingleVarSet.t)
      (e: type_pc_t) : MemOffset.t option =
    let e, e_pc = e in
    (* let resolved_vars = 
      List.filter_map (fun (x: type_rel) -> if x.sol <> SolNone then let idx, _ = x.var_idx in Some idx else None) tv_rel_list
    in
    if SingleExp.is_val (SingleExp.SingleVarSet.union (SingleExp.SingleVarSet.of_list resolved_vars) input_var_set) e then *)
    if is_sol_resolved SingleExp.is_val tv_rel_list input_var_set e then
      match sub_sol_single_to_range_naive_repl false tv_rel_list input_var_set e_pc e with
      | Single e -> let e = eval_helper e in Some (e, e)
      | Range (l, r, _) -> Some (eval_helper l, eval_helper r)
      | _ -> let e = eval_helper e in Some (e, e)
      (* Some (sub_sol_single_to_range tv_rel_list input_var_set e) *)
    else None

  let sub_sol_to_offset_list
      (* NOTE: this is a wrapper func, and eval_helper would be called by its callee sub_to_off_opt_helper,
        so we do not have eval_helper for it *)
      (sub_to_list_helper: 'a -> 'a list)
      (sub_to_off_opt_helper: 'a * int -> MemOffset.t option)
      (e_and_pc: 'a * int) : (MemOffset.t list) option =
    let e, e_pc = e_and_pc in
    let e_list = sub_to_list_helper e in
    let simp_helper (e: 'a) : MemOffset.t option =
      sub_to_off_opt_helper (e, e_pc)
    in
    match e_list with
    | [] -> single_subtype_error "sub_sol_to_offset_list: get empty list"
    | e :: [] ->
      let off_opt = simp_helper e in
      begin match off_opt with
      | Some out_off -> Some [out_off]
      | None -> None
      end
    | _ -> 
      let simp_off_list = List.filter_map simp_helper e_list in
      if List.length simp_off_list = List.length e_list then
        Some simp_off_list
      else
        let off_opt = simp_helper e in
        begin match off_opt with
        | Some out_off -> Some [out_off]
        | None -> None
        end

  let sub_sol_single_to_offset_list
      (eval_helper: SingleExp.t -> SingleExp.t)
      (tv_rel_list: t)
      (input_var_set: SingleEntryType.SingleVarSet.t)
      (e: type_pc_t) : (MemOffset.t list) option =
    let sub_to_list_helper = sub_sol_single_set_var tv_rel_list SingleExp.get_vars SingleExp.repl_var_exp in
    let sub_to_off_opt_helper = sub_sol_single_to_offset_opt eval_helper tv_rel_list input_var_set in
    sub_sol_to_offset_list sub_to_list_helper sub_to_off_opt_helper e

  let sub_sol_single_to_range
      (eval_helper: SingleExp.t -> SingleExp.t)
      (tv_rel_list: t)
      (input_var_set: SingleEntryType.SingleVarSet.t)
      (e: type_pc_t) : RangeExp.t =
    let e, e_pc = e in
    let e_list = sub_sol_single_set_var tv_rel_list SingleExp.get_vars SingleExp.repl_var_exp e in
    match e_list with
    | [] -> single_subtype_error "sub_sol_single_to_range: get empty list"
    | e :: [] -> 
      sub_sol_single_to_range_naive_repl true tv_rel_list input_var_set e_pc e
      |> (RangeExp.eval_helper eval_helper)
    | _ ->
      let simp_e_list = List.map (sub_sol_single_to_range_naive_repl true tv_rel_list input_var_set e_pc) e_list in
      let single_e_list = List.filter_map (
        fun (x: RangeExp.t) ->
          match x with
          | Single e -> Some e
          | _ -> None
      ) simp_e_list
      in
      if List.length simp_e_list = List.length single_e_list then
        SingleSet single_e_list
        |> (RangeExp.eval_helper eval_helper)
      else
        sub_sol_single_to_range_naive_repl true tv_rel_list input_var_set e_pc e
        |> (RangeExp.eval_helper eval_helper)

  let sub_sol_offset_to_offset_list
      (* eval_helper and check sol resolved are all done by calling sub_sol_single_to_offset_opt *)
      (eval_helper: SingleExp.t -> SingleExp.t)
      (tv_rel_list: t)
      (input_var_set: SingleEntryType.SingleVarSet.t)
      (off_and_pc: MemOffset.t * int) : (MemOffset.t list) option =
    let sub_to_list_helper = sub_sol_single_set_var tv_rel_list MemOffset.get_vars MemOffset.repl_var_exp in
    let sub_to_off_opt_helper (off_and_pc: MemOffset.t * int) : MemOffset.t option =
      let (l, r), off_pc = off_and_pc in
      MemOffset.from_off_opt (
        sub_sol_single_to_offset_opt eval_helper tv_rel_list input_var_set (l, off_pc),
        sub_sol_single_to_offset_opt eval_helper tv_rel_list input_var_set (r, off_pc)
      ) 
    in
    (* = sub_sol_single_to_offset_opt eval_helper tv_rel_list input_var_set in *)
    sub_sol_to_offset_list sub_to_list_helper sub_to_off_opt_helper off_and_pc
    (* let off, off_pc = off_and_pc in
    let off_list = sub_sol_single_set_var tv_rel_list MemOffset.get_vars MemOffset.repl_var_exp off in
    let simp_off_helper (off: MemOffset.t) : MemOffset.t option =
      let l, r = off in
      MemOffset.from_off_opt (
        sub_sol_single_to_offset_opt eval_helper tv_rel_list input_var_set (l, off_pc),
        sub_sol_single_to_offset_opt eval_helper tv_rel_list input_var_set (r, off_pc)
      ) 
    in
    match off_list with
    | [] -> single_subtype_error "sub_sol_offset_to_offset_list: get empty list"
    | off :: [] ->
      let off_opt = simp_off_helper off in
      begin match off_opt with
      | Some out_off -> Some [out_off]
      | None -> None
      end
    | _ ->
      let simp_off_list = List.filter_map simp_off_helper off_list in
      if List.length simp_off_list = List.length off_list then
        Some simp_off_list
      else
        let off_opt = simp_off_helper off in
        begin match off_opt with
        | Some out_off -> Some [out_off]
        | None -> None
        end *)

  let sub_sol_single_to_range_opt
      (eval_helper: SingleExp.t -> SingleExp.t)
      (tv_rel_list: t)
      (input_var_set: SingleEntryType.SingleVarSet.t)
      (e: type_pc_t) : RangeExp.t option =
    let exp, _ = e in
    (* let resolved_vars = 
      List.filter_map (fun (x: type_rel) -> if x.sol <> SolNone then let idx, _ = x.var_idx in Some idx else None) tv_rel_list
    in
    if SingleExp.is_val (SingleExp.SingleVarSet.union (SingleExp.SingleVarSet.of_list resolved_vars) input_var_set) exp then *)
    if is_sol_resolved SingleExp.is_val tv_rel_list input_var_set exp then
      Some (sub_sol_single_to_range eval_helper tv_rel_list input_var_set e)
    else None

  let sub_sol_single_to_single_func_interface
      (eval_helper: SingleExp.t -> SingleExp.t)
      (tv_rel_list: t)
      (input_var_set: SingleEntryType.SingleVarSet.t)
      (pc: int) (e: SingleEntryType.t) : SingleEntryType.t =
    let r = 
      sub_sol_single_to_range eval_helper tv_rel_list input_var_set (e, pc) 
    in
    match r with
    | Single exp -> exp
    | _ -> SingleTop

  let find_list_correlation
      (input_var_set: SingleEntryType.SingleVarSet.t)
      (op: SingleExp.single_bop)
      (sol1: type_pc_list_t list) (sol2: type_pc_list_t list) : SingleExp.t option =
    let helper (acc: (SingleExp.t option) * bool) (entry1: type_pc_list_t) (entry2: type_pc_list_t) : (SingleExp.t option) * bool =
      let e1, _ = entry1 in
      let e2, _ = entry2 in
      match acc with
      | _, false -> acc
      | Some invariance, true -> 
        if SingleExp.cmp invariance (SingleExp.eval (SingleBExp (op, e1, e2))) = 0 then 
          Some invariance, true 
        else None, false
      | None, true -> 
        let invariance = SingleExp.eval (SingleBExp (op, e1, e2)) in
        if SingleExp.is_val input_var_set invariance then
          Some invariance, true
        else None, false
    in
    match List.fold_left2 helper (None, true) sol1 sol2 with
    | _, false -> None
    | invariance_opt, true -> invariance_opt

  let update_sol_set_correlation
      (tv_rel_list: t)
      (input_var_set: SingleEntryType.SingleVarSet.t) : t =
    let rec helper_outer (tv_rel_list: t) : t =
      match tv_rel_list with
      | [] -> []
      | hd :: tl ->
        let hd_sol_len = List.length hd.set_sol in
        if hd_sol_len > 0 then
          let hd_idx, hd_pc = hd.var_idx in
          let helper_inner (tv_rel: type_rel) : type_rel =
            let other_idx, other_pc = tv_rel.var_idx in
            (* Printf.printf "!!! SymImm %d pc %d SymImm %d pc %d\n" hd_idx hd_pc other_idx other_pc; *)
            if other_idx <> hd_idx && other_pc = hd_pc && List.length tv_rel.set_sol = hd_sol_len then begin
              match find_list_correlation input_var_set SingleExp.SingleAdd hd.set_sol tv_rel.set_sol with
              | Some invariance ->
                (* Printf.printf "!!! find_list_correlation SymImm %d and SymImm %d with invariance %s\n" other_idx hd_idx (SingleEntryType.to_string invariance); *)
                { tv_rel with 
                  sol = SolSimple (Single (SingleExp.eval (SingleBExp (SingleSub, invariance, SingleVar hd_idx))));
                  set_sol = [] }
              | None -> tv_rel
            end else tv_rel
          in
          hd :: helper_outer (List.map helper_inner tl)
        else hd :: helper_outer tl
    in
    helper_outer tv_rel_list

  let try_solve_one_var
      (* (smt_ctx: SmtEmitter.t) *) (* Maybe I need add_no_overflow later *)
      (tv_rel: type_rel)
      (tv_rel_list: t)
      (block_subtype: ArchType.block_subtype_t list)
      (input_var_set: SingleEntryType.SingleVarSet.t) : 
      type_rel =
    (* let target_idx, target_pc = tv_rel.var_idx in *)
    (* let try_solve_top (tv_rel: type_rel) : type_rel =
      match List.find_opt (fun (x, _) -> x = SingleEntryType.SingleTop) tv_rel.subtype_list with
      | Some _ -> { tv_rel with sol = SolSimple Top }
      | None -> tv_rel
    in *)
    let try_solve_single_sub_val (tv_rel: type_rel) : type_rel =
      let subtype_list = (* remove non-input SingleVar *)
        List.filter (
          fun (e, _) ->
            match e with
            | SingleEntryType.SingleVar _ -> SingleEntryType.is_val input_var_set e
            | _ -> true
        ) tv_rel.subtype_list
      in
      if List.find_opt (fun (x, _) -> x = SingleEntryType.SingleTop) subtype_list <> None then
        tv_rel
      else if List.find_opt (fun (x, _) -> not (SingleEntryType.is_val input_var_set x)) subtype_list = None then
        match List.map (fun (x, pc_list) -> x, List.rev pc_list) subtype_list with
        | [] -> tv_rel
        | (hd, _) :: [] -> { tv_rel with sol = SolSimple (Single hd) }
        | single_pc_list -> (* SolSimple (SingleSet single_list) *)
          begin match List.sort_uniq (fun (x1, _) (x2, _) -> SingleExp.cmp x1 x2) single_pc_list with
          | [] -> single_subtype_error "sub range list is empty"
          | (hd, _) :: [] -> { tv_rel with sol = SolSimple (Single hd)}
          | _ ->
            let single_pc_list = 
              List.sort (
                fun (x1, pc_list1) (x2, pc_list2) ->
                  let pc_cmp = compare pc_list1 pc_list2 in
                  if pc_cmp = 0 then SingleExp.cmp x1 x2 else pc_cmp
              ) single_pc_list
            in
            let sol, _ = List.split single_pc_list in
            { tv_rel with sol = SolSimple (SingleSet sol); set_sol = single_pc_list }
            (* Printf.printf "Set sol for SymImm %d pc %d %s\n" target_idx target_pc (Sexplib.Sexp.to_string (sexp_of_list sexp_of_type_pc_list_t single_pc_list));
            find_other_sol_set_correlation target_idx target_pc single_pc_list tv_rel_list input_var_set *)
            (* SolSimple (SingleSet single_list) *)
          end
      else begin 
        (* To handle the case where exp contains resolved block vars *)
        let sub_range_opt_list = List.map (fun (x, pc_list) -> sub_sol_single_to_range_opt (fun x -> x) tv_rel_list input_var_set (x, List.hd pc_list)) subtype_list in
        let sub_range_list = List.filter_map (fun x -> x) sub_range_opt_list in
        (
          let var_idx, _ = tv_rel.var_idx in
          if var_idx = 114 then
            Printf.printf "sub_range_list: \n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list RangeExp.sexp_of_t sub_range_list));
        );
        if List.length sub_range_opt_list <> List.length sub_range_list then tv_rel
        else if List.find_opt (fun x -> x = RangeExp.Top) sub_range_list <> None then { tv_rel with sol = SolSimple (Top) }
        else begin
          let rec helper 
              (range_list: RangeExp.t list) : 
              (SingleEntryType.t list) * 
              ((SingleEntryType.t * SingleEntryType.t * int64) list) =
            match range_list with
            | [] -> [], []
            | hd :: tl -> let list1, list2 = helper tl in
              begin match hd with
              | Single e -> e :: list1, list2
              | Range (e1, e2, s) -> list1, (e1, e2, s) :: list2
              | SingleSet set -> set @ list1, list2
              | Top -> single_subtype_error "range partition should not find top"
              end
          in
          match helper sub_range_list with
          | single_list, [] -> { tv_rel with sol = SolSimple (SingleSet single_list) }
            (* TODO: I cannot resolve correlation between vars with this solution format!!! *)
            (* Printf.printf "!!! single list %s\n" (Sexplib.Sexp.to_string (sexp_of_list SingleEntryType.sexp_of_t single_list)); *)
            (* begin match List.sort_uniq SingleExp.cmp single_list with
            | [] -> single_subtype_error "sub range list is empty"
            | hd :: [] -> SolSimple (Single hd)
            | _ -> SolSimple (SingleSet single_list)
            end *)
          | s :: [], (l, r, step) :: [] ->
            if SingleEntryType.cmp s (SingleEntryType.eval (SingleBExp (SingleAdd, r, SingleConst (Int64.abs step)))) = 0 then
              { tv_rel with sol = SolSimple (Range (l, s, step)) }
            else if SingleEntryType.cmp s (SingleEntryType.eval (SingleBExp (SingleSub, l, SingleConst (Int64.abs step)))) = 0 then
              { tv_rel with sol = SolSimple (Range (s, r, step))}
            else tv_rel
          | _ -> tv_rel (* TODO: Maybe need to improve this!!! *)
        end
      end
    in
    let find_base_step 
        (supertype_list: var_pc_list_t list)
        (subtype_list: type_pc_list_t list) (v_idx: IsaBasic.imm_var_id) : 
        (SingleEntryType.t * type_pc_list_t * int * int64) option =
      let find_base = 
        List.find_opt (
          fun (x, _) -> SingleEntryType.is_val input_var_set x
        ) subtype_list
      in
      let find_step =
        List.find_map (
          fun (x_pc: type_pc_list_t) ->
            let x, _ = x_pc in
            match x with
            | SingleBExp (SingleAdd, SingleVar v, SingleConst s)
            | SingleBExp (SingleAdd, SingleConst s, SingleVar v) ->
              if v = v_idx then
                if s = 0L then single_subtype_error "try_solve_loop_cond: step should be non-zero"
                else Some (x_pc, v, s)
              else if List.find_opt (fun (sup_idx, _) -> v = sup_idx) supertype_list <> None then
                if s = 0L then single_subtype_error "try_solve_loop_cond: step should be non-zero"
                else Some (x_pc, v, s)
              else None
            | _ -> None
        ) subtype_list
      in
      match find_base, find_step with
      | Some (base, _), Some ((var_step, branch_pc), var_idx, step) ->
        Some (base, (var_step, branch_pc), var_idx, step)
      | _ -> None
    in
    let get_range_helper (begin_val: SingleEntryType.t) (end_val: SingleEntryType.t) (step: int64) : RangeExp.t =
      if step > 0L then
        Range (begin_val, end_val, step)
      else if step < 0L then
        Range (end_val, begin_val, step)
      else
        single_subtype_error "get_range_helper step = 0L"
    in
    let bound_match 
        (on_left: bool) (cond: ArchType.CondType.cond)
        (begin_val: SingleEntryType.t) (end_val: SingleEntryType.t) (step: int64) : (RangeExp.t * RangeExp.t * RangeExp.t) option =
      let inc = step > 0L in
      let len_val = SingleEntryType.eval (SingleBExp (SingleSub, end_val, begin_val)) in
      let len_div_step = SingleEntryType.is_div len_val step in
      (* Check whether cmp opcode, operand, and step value are compatible *)
      let match_success =
        match on_left, inc, cond with
        | _, _, Ne ->
          if not len_div_step then Printf.printf "Warning: for jne, step %Ld does not div len %s\n" step (SingleEntryType.to_string len_val);
          true
        | _, _, Eq -> false
        | _ -> on_left = inc
      in
      if not match_success then None else
      let in_loop_bound_off, resume_loop_bound_off =
        match cond with
        | Ne -> 0L, Int64.neg step
        | Lt | Bt ->
          if len_div_step then 0L, Int64.neg step
          else if inc then Int64.sub step 1L, -1L
          else Int64.add step 1L, 1L
        | Le | Be -> step, 0L
        | Eq -> single_subtype_error "this should not be reachable"
      in
      let end_val_in = SingleEntryType.eval (SingleBExp (SingleAdd, end_val, SingleConst in_loop_bound_off)) in
      let end_val_resume = SingleEntryType.eval (SingleBExp (SingleAdd, end_val, SingleConst resume_loop_bound_off)) in
      let range_in = get_range_helper begin_val end_val_in step in
      let range_resume = get_range_helper begin_val end_val_resume step in
      let range_out : RangeExp.t = 
        if len_div_step then Single end_val_in
        else get_range_helper end_val_resume end_val_in (if step > 0L then 1L else -1L)
      in
      Some (range_in, range_resume, range_out)
    in
    let gen_self_loop_sol
        (begin_val: SingleEntryType.t) (end_val: SingleEntryType.t)
        (step: int64) (* signed, which could be positive or negative *)
        (step_before_cmp: bool) 
        (cmp_pc: int) (* or cond_pc: note this may not be the branch pc*)
        (resume_on_taken: bool)
        (cmp_operand: ArchType.CondType.cond) (cmp_var_on_left: bool)
        (var_cmp_in_same_block: bool) : SingleSol.t =
      let end_val =
        if step_before_cmp then SingleEntryType.eval (SingleBExp (SingleSub, end_val, SingleConst step))
        else end_val
      in
      match bound_match cmp_var_on_left cmp_operand begin_val end_val step with
      | Some (range_in, range_resume, range_out) ->
        if var_cmp_in_same_block then
          if resume_on_taken then
            SolCond (cmp_pc, range_in, range_resume, range_out)
          else SolCond (cmp_pc, range_in, range_out, range_resume)
        else SolSimple range_in
      | None -> 
        Printf.printf "Bound match cannot find sol begin %s end %s\n" (SingleEntryType.to_string begin_val) (SingleEntryType.to_string end_val);
        SolNone
      (* let match_cmp_op_step, (in_loop_bound_off, resume_loop_bound_off) = 
        bound_match cmp_var_on_left inc cmp_operand begin_val end_val_var_cmp step
      in
      if match_cmp_op_step then 
        let end_val = SingleEntryType.eval (SingleBExp (SingleAdd, end_val_var_cmp, SingleConst in_loop_bound_off)) in
        let end_val_resume = SingleEntryType.eval (SingleBExp (SingleAdd, end_val_var_cmp, SingleConst resume_loop_bound_off)) in
        let range, range_resume =
          if inc then
            RangeExp.Range (begin_val, end_val, step), RangeExp.Range (begin_val, end_val_resume, step)
          else
            RangeExp.Range (end_val, begin_val, Int64.neg step), RangeExp.Range (end_val_resume, begin_val, Int64.neg step)
        in
        if var_cmp_in_same_block then
          if resume_on_taken then
            SolCond (cmp_pc, range, range_resume, Single end_val)
          else SolCond (cmp_pc, range, Single end_val, range_resume)
        else SolSimple range
      else begin
        Printf.printf "Bound match cannot find sol begin %s end %s\n" (SingleEntryType.to_string begin_val) (SingleEntryType.to_string end_val_var_cmp);
        SolNone
      end *)
    in
    let gen_other_loop_sol
        (self_info: int * SingleExp.t * int64)
        (other_info: int * SingleExp.t * int64) :
        SingleContext.t =
      let self_idx, self_base, self_sign_step = self_info in
      let other_idx, other_base, other_sign_step = other_info in
      Cond (
        Eq,
        SingleExp.eval (SingleBExp (SingleMul, (SingleBExp (SingleSub, SingleVar self_idx, self_base)), SingleConst other_sign_step)),
        SingleExp.eval (SingleBExp (SingleMul, (SingleBExp (SingleSub, SingleVar other_idx, other_base)), SingleConst self_sign_step))
      )
    in
    let try_solve_loop_cond (tv_rel: type_rel) : type_rel =
      let target_idx, target_pc = tv_rel.var_idx in
      match find_base_step tv_rel.supertype_list tv_rel.subtype_list target_idx with
      | Some (_, _, _, 0L) -> single_subtype_error "try_solve_loop_cond: find step 0" 
      | Some (base, (var_step, branch_pc), var_idx, step) ->
        begin match ArchType.get_branch_cond block_subtype (List.hd branch_pc) with
        | None -> 
          (* Note uncond jump now can use the last not taken cond! *)
          (* TODO: Maybe we should make loop inference use more conditions. *)
          Printf.printf "Warning: try_solve_loop_cond found a uncond jump\n";
          tv_rel
        | Some ((cond, l, r), cond_pc) ->
          let find_cond_var_step = (* on_left, bound, add step before cmp *)
            if SingleEntryType.cmp l var_step = 0 then Some (true, r, true)
            else if SingleEntryType.cmp r var_step = 0 then Some (false, l, true)
            else if SingleEntryType.cmp l (SingleVar var_idx) = 0 then Some (true, r, false)
            else if SingleEntryType.cmp r (SingleVar var_idx) = 0 then Some (false, l, false)
            else None
          in
          Printf.printf "Find cond var step %s\n" (ArchType.CondType.to_string (cond, l, r));
          match find_cond_var_step with
          | Some (on_left, bound, step_before_cmp) -> 
            (* let inc = (step > 0L) in *)
            let resume_loop_on_taken = cond_pc = List.hd branch_pc in
            let target_idx_branch_pc_same_block = target_idx = var_idx in
            (* TODO: Need to repl bound here!!! *)
            let bound = (
              (* Very imporant: here bound comes from the branch condition, which should be evaluated in the context of branch_pc - 1!!! *)
              begin match sub_sol_single_to_range_opt (fun x -> x) tv_rel_list input_var_set (bound, (List.hd branch_pc) - 1) with
              | Some (Single e) -> e
              | Some _ -> bound
              | None -> SingleTop
              end
            ) in
            if bound <> SingleTop && SingleEntryType.is_val input_var_set bound then
              let sol = gen_self_loop_sol base bound step step_before_cmp cond_pc resume_loop_on_taken cond on_left target_idx_branch_pc_same_block in
              { tv_rel with sol = sol }
              (* let bound_1 = SingleEntryType.eval (SingleBExp (SingleSub, bound, SingleConst step)) in
              let bound_2 = SingleEntryType.eval (SingleBExp (SingleSub, bound_1, SingleConst step)) in
              begin match bound_match on_left inc cond, inc with
              | (true, true), true -> 
                { tv_rel with sol = SolCond (List.hd branch_pc, Range (base, bound, step), Range (base, bound_1, step), Single bound) }
              | (true, true), false -> 
                { tv_rel with sol = SolCond (List.hd branch_pc, Range (bound, base, Int64.neg step), Range (bound_1, base, Int64.neg step), Single bound) }
              | (true, false), true ->
                { tv_rel with sol = SolCond (List.hd branch_pc, Range (base, bound_1, step), Range (base, bound_2, step), Single bound_1) }
              | (true, false), false ->
                { tv_rel with sol = SolCond (List.hd branch_pc, Range (bound_1, base, Int64.neg step), Range (bound_2, base, Int64.neg step), Single bound_1) }
              | _ -> 
                Printf.printf "Bound match cannot find sol %s bound %s\n" (ArchType.CondType.to_string (cond, l, r)) (SingleExp.to_string bound);
                tv_rel
              end *)
            else begin
              (* Printf.printf "!!!\n";
              pp_single_subtype 0 tv_rel_list;
              Printf.printf "!!!\n"; *)
              Printf.printf "Bound is not val %s bound %s\n" (ArchType.CondType.to_string (cond, l, r)) (SingleExp.to_string bound);
              tv_rel
            end
          | None ->
            (* Represent sol with the actual loop counter *)
            let find_other =
              List.find_map (
                fun (tv: type_rel) ->
                  let v_idx, v_pc = tv.var_idx in
                  if v_pc <> target_pc then None
                  else begin
                    match find_base_step tv.supertype_list tv.subtype_list v_idx with
                    | Some (_, _, _, 0L) -> None
                    | Some (_, (other_step, other_branch_pc), other_cmp_var_idx, v_step) ->
                      (* NOTE: Must ensure the other_var is indeed in the cond!!! *)
                      (* IMPORTANT: We need to use v_step from find_base_step instead of the solution*)
                      if SingleEntryType.cmp l other_step = 0 || SingleEntryType.cmp r other_step = 0
                        || SingleEntryType.cmp l (SingleVar other_cmp_var_idx) = 0
                        || SingleEntryType.cmp r (SingleVar other_cmp_var_idx) = 0 then begin
                        match tv.sol with
                        | SolCond (_, Range (v_l, v_r, _), _, _) -> (* Only real counter that used in cmp can have this format of solution *)
                          if List.hd other_branch_pc = List.hd branch_pc then (* We check branch pc, which might not be the pc of the cond branch that do the cmp *)
                            let v_base = if v_step > 0L then v_l else v_r in
                            if Int64.rem step v_step = 0L then
                              Some (SingleEntryType.eval (
                                SingleBExp (
                                  SingleAdd,
                                  SingleBExp (
                                    SingleMul, 
                                    SingleBExp (SingleSub, SingleVar v_idx, v_base),
                                    SingleConst (Int64.div step v_step)
                                  ),
                                  base
                                )
                              ), [])
                            else 
                              let _ = 
                                Some (
                                SingleExp.SingleVar target_idx,
                                [ gen_other_loop_sol (target_idx, base, step) (v_idx, v_base, v_step) ]
                              )
                              in None
                              (* single_subtype_error (Printf.sprintf "cannot represent range of symimm %d with symimm %d" target_idx v_idx) *)
                          else None
                        | _ -> None
                      end else None
                    | _ -> None
                  end
              ) tv_rel_list
            in
            begin match find_other with
            | Some (sol, other_constraint) -> { tv_rel with sol = SolSimple (Single sol); other_constraint = other_constraint }
            | None -> 
              Printf.printf "Warning: cannot find sol for var %d with find loop cond\n" target_idx;
              tv_rel
            end
          end
      | None -> tv_rel
    in
    let solve_rules = [
      (* try_solve_top; *)
      try_solve_loop_cond;
      try_solve_single_sub_val;
    ] in
    let tv_rel_opt = 
      List.find_map (
        fun rule ->
          let tv_rel = rule tv_rel in
          if tv_rel.sol = SingleSol.SolNone then None
          else Some tv_rel
      ) solve_rules
    in
    match tv_rel_opt with
    | Some tv_rel -> tv_rel
    | None -> tv_rel

  let subsititue_one_exp_single_sol
      (exp_pc: type_pc_list_t)
      (idx_sol: var_pc_t * SingleSol.t) : type_pc_list_t =
    let exp, e_pc = exp_pc in
    let (idx, _), sol = idx_sol in
    (* TODO: We should check var and exp are in the block instead of having the same pc *)
    (* But I think if a exp has a var, then they must belong to the same block, so no need to check (maybe) *)
    (* if e_pc <> i_pc then exp_pc
    else *)
      match sol with
      | SolSimple (Single e) -> 
        (* Printf.printf "subsititue_one_exp_single_sol %s %d %s" (SingleExp.to_string exp) idx (SingleExp.to_string e); *)
        SingleEntryType.eval (SingleEntryType.repl_var_exp exp (idx, e)), e_pc
      | SolCond (pc, r_before_branch, r_taken, r_not_taken) ->
        (* TODO: Double check this!!! *)
        let r = if (List.hd e_pc) < pc then r_before_branch else if (List.hd e_pc) = pc then r_taken else r_not_taken in
        begin match r with
        | Single e -> SingleEntryType.eval (SingleEntryType.repl_var_exp exp (idx, e)), e_pc
        | _ -> exp_pc
        end
      | _ -> exp_pc

  let subsititue_one_exp_single_sol_list
      (idx_sol_list: (var_pc_t * SingleSol.t) list)
      (exp_pc: type_pc_list_t) : type_pc_list_t =
    List.fold_left subsititue_one_exp_single_sol exp_pc idx_sol_list

  let subsititue_one_exp_subtype_list
      (tv_rel_list: t) (exp_pc: type_pc_t) : SingleEntryType.t =
    let idx_sol_list =
      List.map (fun (x: type_rel) -> (x.var_idx, x.sol)) tv_rel_list
    in
    let exp, pc = exp_pc in
    let exp, _ = subsititue_one_exp_single_sol_list idx_sol_list (exp, [pc]) in
    exp

  let update_subtype_single_sol
      (tv_rel_list: t) (idx_sol_list: (var_pc_t * SingleSol.t) list) : t =
    List.map (
      fun (tv_rel: type_rel) ->
        if tv_rel.sol <> SolNone then tv_rel
        else
          (* let v_idx, _ = tv_rel.var_idx in
          Printf.printf "Sub for %d sol list len %d\n" v_idx (List.length idx_sol_list); *)
          { tv_rel with subtype_list = List.map (subsititue_one_exp_single_sol_list idx_sol_list) tv_rel.subtype_list }
    ) tv_rel_list

  let try_solve_vars
      (* (smt_ctx: SmtEmitter.t) *)
      (tv_rel_list: t)
      (block_subtype: ArchType.block_subtype_t list)
      (input_var_set: SingleEntryType.SingleVarSet.t) : t * bool =
    let new_sol_list, new_subtype = List.fold_left_map (
      fun acc tv_rel ->
        if tv_rel.sol <> SolNone then acc, tv_rel
        else
          (* Merge same subtype exp, keep the smaller pc which means fewer constraints from cond branch *)
          let rec merge (last_entry: (SingleEntryType.t * int) option) (sub_list: type_pc_list_t list) : type_pc_list_t list =
            match last_entry, sub_list with
            | _, [] -> []
            | None, (hd_exp, hd_pc) :: tl -> (hd_exp, hd_pc) :: (merge (Some (hd_exp, list_tl hd_pc)) tl)
            | Some (last_exp, last_pc), (hd_exp, hd_pc) :: tl ->
              if SingleEntryType.cmp hd_exp last_exp = 0 && last_pc = list_tl hd_pc then merge last_entry tl
              else (hd_exp, hd_pc) :: (merge (Some (hd_exp, list_tl hd_pc)) tl)
          in
          let subtype_list =
            merge None (List.sort (
              fun (e1, pc1) (e2, pc2) ->
                let cmp_e = SingleEntryType.cmp e1 e2 in
                if cmp_e = 0 then compare pc1 pc2
                else cmp_e
            ) tv_rel.subtype_list)
          in
          let tv_rel = { tv_rel with subtype_list = subtype_list } in
          let tv_rel = try_solve_one_var tv_rel tv_rel_list block_subtype input_var_set in
          if tv_rel.sol = SolNone then acc, tv_rel
          else (tv_rel.var_idx, tv_rel.sol) :: acc, tv_rel
    ) [] tv_rel_list
    in
    (* Printf.printf "Update subtype_single_sol with new sol\n";
    List.iter (
      fun ((x, _), sol) -> Printf.printf "SymImm %d = %s\n" x (SingleSol.to_string sol)
    ) new_sol_list; *)
    let new_subtype = update_subtype_single_sol new_subtype new_sol_list in
    (* TODO: handle/utilize other forms of solutions like range *)
    new_subtype, List.length new_sol_list > 0

  let solve_vars
      (* (smt_ctx: SmtEmitter.t) *)
      (tv_rel_list: t)
      (block_subtype: ArchType.block_subtype_t list)
      (input_var_set: SingleEntryType.SingleVarSet.t)
      (num_iter: int) : t =
    let rec helper
        (tv_rel_list: t)
        (num_iter: int) : t =
      if num_iter = 0 then update_sol_set_correlation tv_rel_list input_var_set
      else begin
        let new_subtype, found_new_sol = try_solve_vars tv_rel_list block_subtype input_var_set in
        if found_new_sol then helper new_subtype (num_iter - 1)
        else update_sol_set_correlation new_subtype input_var_set
      end
    in
    helper tv_rel_list num_iter

  (* TODO:
    1. Handle calculation of range of a exp when var in the exp is not a single value.
    2. Solve by merging several ranges.
  *)

end
