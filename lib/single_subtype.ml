open Isa
open Single_exp
open Single_entry_type
open Arch_type
open Smt_emitter
open Pretty_print

module RangeExp = struct
  exception RangeExpError of string

  let range_exp_error msg = raise (RangeExpError ("[Range Exp Error] " ^ msg))

  type t = (* TODO: Maybe need more... *)
    | Single of SingleEntryType.t
    | Range of SingleEntryType.t * SingleEntryType.t * int64 (* begin, end, step *)
    | SingleSet of SingleEntryType.t list
    | Top

  let to_string (e: t) : string =
    match e with
    | Single e -> Printf.sprintf "{%s}" (SingleEntryType.to_string e)
    | Range (a, b, step) -> Printf.sprintf "[%s, %s]%Ld" (SingleEntryType.to_string a) (SingleEntryType.to_string b) step
    | SingleSet e_list ->
      let str_list = List.map SingleEntryType.to_string e_list in
      Printf.sprintf "{%s}" (String.concat ", " str_list)
    | Top -> "Top"

  let to_smt_expr (smt_ctx: SmtEmitter.t) (v_idx: int) (r: t) : SmtEmitter.exp_t =
    let ctx, _ = smt_ctx in
    let v_exp = SingleEntryType.to_smt_expr smt_ctx (SingleVar v_idx) in
    match r with
    | Single e -> Z3.Boolean.mk_eq ctx v_exp (SingleEntryType.to_smt_expr smt_ctx e)
    | Range (a, b, step) ->
      let a_exp = SingleEntryType.to_smt_expr smt_ctx a in
      let b_exp = SingleEntryType.to_smt_expr smt_ctx b in
      let step = SingleEntryType.to_smt_expr smt_ctx (SingleConst step) in
      let a_le_v = Z3.BitVector.mk_sle ctx a_exp v_exp in
      let v_le_b = Z3.BitVector.mk_sle ctx v_exp b_exp in
      let mod_step_eq_0 = 
        Z3.Boolean.mk_eq ctx 
          (Z3.BitVector.mk_smod ctx (Z3.BitVector.mk_sub ctx v_exp a_exp) step) 
          (SingleEntryType.to_smt_expr smt_ctx (SingleConst 0L)) 
      in
      Z3.Boolean.mk_and ctx [a_le_v; v_le_b; mod_step_eq_0]
    | SingleSet e_list ->
      let eq_exp_list =
        List.map (
          fun x -> Z3.Boolean.mk_eq ctx v_exp (SingleEntryType.to_smt_expr smt_ctx x)
        ) e_list
      in
      Z3.Boolean.mk_or ctx eq_exp_list
    | Top -> Z3.Boolean.mk_true ctx

end

module SingleSol = struct
  exception SingleSolError of string

  let single_sol_error msg = raise (SingleSolError ("[Single Sol Error] " ^ msg))

  type t =
    | SolNone
    | SolSimple of RangeExp.t
    | SolCond of int * RangeExp.t * RangeExp.t * RangeExp.t
      (* cond_pc * (sol of pc < cond_pc) * (sol of pc = cond_pc (taken)) * (sol of pc > cond_pc (not taken)) *)
      (* This is used to simplify inference process, not for type check!!! *)

  let to_string (e: t) : string =
    match e with
    | SolNone -> "SolNone"
    | SolSimple e -> Printf.sprintf "SolSimple(%s)" (RangeExp.to_string e)
    | SolCond (pc, e1, e2, e3) -> Printf.sprintf "SolCond(%d)(%s, %s, %s)" pc (RangeExp.to_string e1) (RangeExp.to_string e2) (RangeExp.to_string e3)

  let to_smt_expr (smt_ctx: SmtEmitter.t) (v_idx: int) (s: t) : SmtEmitter.exp_t =
    match s with
    | SolNone -> RangeExp.to_smt_expr smt_ctx v_idx RangeExp.Top
    | SolSimple e
    | SolCond (_, e, _, _) -> RangeExp.to_smt_expr smt_ctx v_idx e

end

module SingleSubtype = struct
  exception SingleSubtypeError of string

  let single_subtype_error msg = raise (SingleSubtypeError ("[Single Subtype Error] " ^ msg))

  type var_idx_t = Isa.imm_var_id * int
  type type_exp_t = SingleEntryType.t * int

  type type_rel = {
    var_idx: var_idx_t;
    sol: SingleSol.t;
    subtype_list: type_exp_t list; (* subtype, pc of the branch that jumps to var_idx's block *)
    supertype_list: var_idx_t list (* NOTE: I omit br hist here since I noticed that current implementation cannot use it anyway *)
  }

  type t = type_rel list

  module ArchType = ArchType (SingleEntryType)

  let pp_type_rel (lvl: int) (x: type_rel) =
    let var_idx, _ = x.var_idx in
    PP.print_lvl lvl "<SymImm %d>\n" var_idx;
    PP.print_lvl (lvl + 1) "Sol: %s\n" (SingleSol.to_string x.sol);
    PP.print_lvl (lvl + 1) "Subtype: [\n";
    List.iter (fun (sub, pc) -> PP.print_lvl (lvl + 2) "%s, (%d);\n" (SingleEntryType.to_string sub) pc) x.subtype_list;
    PP.print_lvl (lvl + 1) "]\n";
    PP.print_lvl (lvl + 1) "Supertype: [%s]\n" 
      (String.concat "; " (List.map (fun (v, _) -> string_of_int v) x.supertype_list))

  let pp_single_subtype (lvl: int) (tv_rels: t) =
    List.iter (fun x -> pp_type_rel lvl x) tv_rels

  let find_or_add_entry (tv_rel: t) (var_idx: var_idx_t) : t * type_rel =
    let find_entry = List.find_opt (fun x -> x.var_idx = var_idx) tv_rel in
    match find_entry with
    | Some entry -> tv_rel, entry
    | None ->
      let entry = 
        { var_idx = var_idx; sol = SingleSol.SolNone; subtype_list = []; supertype_list = [] } 
      in
      entry :: tv_rel, entry


  let type_list_insert 
      (type_list: type_exp_t list) (ty: type_exp_t) : type_exp_t list =
    let t_exp, t_pc = ty in
    let find_type = 
      List.find_opt (
        fun (exp, pc) -> SingleEntryType.cmp t_exp exp = 0 && t_pc = pc
      ) type_list
    in
    if find_type != None then type_list else ty :: type_list

  let type_var_list_insert
      (type_var_list: var_idx_t list) (var_idx: var_idx_t) : var_idx_t list =
    if List.find_opt (fun x -> x = var_idx) type_var_list != None then type_var_list else var_idx :: type_var_list

  let add_one_sub_type (tv_rel: type_rel) (ty: type_exp_t) : type_rel =
    { tv_rel with subtype_list = type_list_insert tv_rel.subtype_list ty }

  let add_one_super_type (tv_rel: type_rel) (t_idx: var_idx_t) : type_rel =
    { tv_rel with supertype_list = type_var_list_insert tv_rel.supertype_list t_idx }

  (* Connect a->b *)
  let add_one_sub_super (tv_rel: t) (a_exp_pc: type_exp_t) (b_idx: var_idx_t) : t =
    match a_exp_pc with
    | SingleVar a_idx, a_pc ->
      if (a_idx, a_pc) = b_idx then tv_rel
      else
        List.map (
          fun x ->
            if x.var_idx = (a_idx, a_pc) then add_one_super_type x b_idx
            else if x.var_idx = b_idx then add_one_sub_type x a_exp_pc
            else x
        ) tv_rel
    | _ ->
      List.map (
        fun x ->
          if x.var_idx = b_idx then add_one_sub_type x a_exp_pc
          else x
      ) tv_rel


  let add_sub_sub_super
      (tv_rel: t) (a_exp_pc: type_exp_t) (b_idx: var_idx_t) : t =
    match a_exp_pc with
    | SingleVar a_idx, a_pc ->
      let tv_rel, a_entry = find_or_add_entry tv_rel (a_idx, a_pc) in
      let tv_rel =
        List.fold_left (
          fun acc_tv_rel sub_a ->
            add_one_sub_super acc_tv_rel sub_a b_idx
        ) tv_rel a_entry.subtype_list in
      add_one_sub_super tv_rel a_exp_pc b_idx
    | _ -> add_one_sub_super tv_rel a_exp_pc b_idx

  let add_sub_sub_super_super
      (tv_rel: t) (a_exp_pc: type_exp_t) (b_idx: var_idx_t) : t =
    let tv_rel, b_entry = find_or_add_entry tv_rel b_idx in
    let tv_rel =
      List.fold_left (
        fun acc_tv_rel sup_b ->
          add_sub_sub_super acc_tv_rel a_exp_pc sup_b
      ) tv_rel b_entry.supertype_list
    in
    add_sub_sub_super tv_rel a_exp_pc b_idx
    
  let add_full_subtype 
      (tv_rel: t) 
      (a_exp_pc: type_exp_t) 
      (b: type_exp_t) : t =
    (* a_exp_pc -> b *)
    match b with
    | SingleVar b_idx, b_pc -> add_sub_sub_super_super tv_rel a_exp_pc (b_idx, b_pc)
    | SingleTop, _ -> tv_rel
    | b_exp, b_pc -> let a, pc = a_exp_pc in
      single_subtype_error 
        (Printf.sprintf "add_full_subtype: incorrect sub/super types (%s,%d)->(%s,%d)"
          (SingleEntryType.to_string a) pc (SingleEntryType.to_string b_exp) b_pc)

  type useful_var_t = Isa.label * SingleExp.SingleVarSet.t

  let pp_useful_var (lvl: int) (useful_var: useful_var_t) =
    let label, vars = useful_var in
    let var_s = String.concat "," (List.map string_of_int (SingleExp.SingleVarSet.to_list vars)) in
    PP.print_lvl lvl "%s\t%s\n" label var_s

  let pp_useful_var_list (lvl: int) (useful_var_list: useful_var_t list) =
    List.iter (pp_useful_var (lvl + 1)) useful_var_list

  let add_one_useful_var_block_subtype_helper
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
            add_sub_sub_super_super acc_tv_rel (sub, sub_pc) (sup_idx, sup_block.pc),
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
      (block_subtype_list: ArchType.block_subtype_t list)
      (useful_var: useful_var_t)
      (tv_rel: t) :
      t * (ArchType.block_subtype_t list) * (useful_var_t list) =
    let sup_label, sup_useful_var = useful_var in
    let block_subtype = 
      List.find (fun (x: ArchType.block_subtype_t) -> let x, _ = x in x.label = sup_label) block_subtype_list 
    in
    let tv_rel, new_useful_var_list = add_one_useful_var_block_subtype_helper block_subtype sup_useful_var tv_rel in
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
      (block_subtype_list: ArchType.block_subtype_t list)
      (useful_var_list: useful_var_t list)
      (tv_rel: t) : t * (ArchType.block_subtype_t list) =
      (* (count: int) : t * (ArchType.block_subtype_t list) = *)
    match useful_var_list with
    | [] -> tv_rel, block_subtype_list
    | hd :: tl ->
      let tv_rel, block_subtype_list, new_useful_var_list =
        add_one_useful_var_block_subtype block_subtype_list hd tv_rel
      in
      let useful_var_list = merge_useful_var tl new_useful_var_list in
      add_all_useful_var_block_subtype block_subtype_list useful_var_list tv_rel
      (* if count > 0 then
        add_all_useful_var_block_subtype block_subtype_list useful_var_list tv_rel (count - 1)
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
      (block_subtype_list: ArchType.block_subtype_t list) : t * (ArchType.block_subtype_t list) =
    let useful_var_list = init_useful_var_from_block_subtype block_subtype_list in
    Printf.printf "after init useful var\n";
    add_all_useful_var_block_subtype block_subtype_list useful_var_list []

  let to_smt_expr (smt_ctx: SmtEmitter.t) (sol: type_rel) : SmtEmitter.exp_t =
    let var_idx, _ = sol.var_idx in
    SingleSol.to_smt_expr smt_ctx var_idx sol.sol

  let update_block_smt_ctx
      (smt_ctx: SmtEmitter.t)
      (sol: t) (curr_type: ArchType.t) : unit =
    List.iter (
      fun (x: type_rel) ->
        let var_idx, _ = x.var_idx in
        if SingleExp.SingleVarSet.mem var_idx curr_type.useful_var then
          SmtEmitter.add_assertions smt_ctx [ to_smt_expr smt_ctx x ]
        else ()
    ) sol

  let try_solve_one_var
      (* (smt_ctx: SmtEmitter.t) *) (* Maybe I need add_no_overflow later *)
      (tv_rel: type_rel)
      (tv_rel_list: t)
      (block_subtype: ArchType.block_subtype_t list)
      (input_var_set: SingleEntryType.SingleVarSet.t) : SingleSol.t =
    let target_idx, target_pc = tv_rel.var_idx in
    let try_solve_top (subtype_list: type_exp_t list) : SingleSol.t =
      match List.find_opt (fun (x, _) -> x = SingleEntryType.SingleTop) subtype_list with
      | Some _ -> SolSimple Top
      | None -> SolNone
    in
    let try_solve_single_sub_val (subtype_list: type_exp_t list) : SingleSol.t =
      if List.find_opt (fun (x, _) -> not (SingleEntryType.is_val input_var_set x)) subtype_list = None then
        match List.map (fun (x, _ ) -> x) subtype_list with
        | [] -> SolNone
        | hd :: [] -> SolSimple (Single hd)
        | subtype_list -> SolSimple (SingleSet subtype_list)
      else
        SolNone
    in
    let find_base_step 
        (subtype_list: type_exp_t list) (v_idx: Isa.imm_var_id) : 
        (SingleEntryType.t * type_exp_t * int64) option =
      let find_base = 
        List.find_opt (
          fun (x, _) -> SingleEntryType.is_val input_var_set x
        ) subtype_list
      in
      let find_step =
        List.find_map (
          fun (x_pc: type_exp_t) ->
            let x, _ = x_pc in
            match x with
            | SingleBExp (SingleAdd, SingleVar v, SingleConst s)
            | SingleBExp (SingleAdd, SingleConst s, SingleVar v) ->
              if v = v_idx then
                if s = 0L then single_subtype_error "try_solve_loop_cond: step should be non-zero"
                else Some (x_pc, s)
              else None
            | _ -> None
        ) subtype_list
      in
      match find_base, find_step with
      | Some (base, _), Some ((var_step, branch_pc), step) ->
        Some (base, (var_step, branch_pc), step)
      | _ -> None
    in
    let bound_match (on_left: bool) (inc: bool) (cond: ArchType.CondType.cond) : bool * bool =
      (* match, if match include bound or not *)
      match on_left, inc, cond with
      (* x+step != / < / <= bound *)
      | true, true, Ne
      | true, true, Lt -> true, false
      | true, true, Le -> true, true
      (* bound != / < / <= x+step *)
      | false, false, Ne
      | false, false, Lt -> true, false
      | false, false, Le -> true, true
      (* does not match *)
      | _ -> false, false
    in
    let try_solve_loop_cond (subtype_list: type_exp_t list) : SingleSol.t =
      match find_base_step subtype_list target_idx with
      | Some (_, _, 0L) -> single_subtype_error "try_solve_loop_cond: find step 0" 
      | Some (base, (var_step, branch_pc), step) ->
        begin match ArchType.get_branch_cond block_subtype target_pc branch_pc with
        | None -> 
          Printf.printf "Warning: try_solve_loop_cond found a uncond jump\n";
          SolNone
        | Some (cond, l, r) ->
          let find_cond_var_step = 
            if SingleEntryType.cmp l var_step = 0 then Some (true, r)
            else if SingleEntryType.cmp r var_step = 0 then Some (false, l)
            else None
          in
          match find_cond_var_step with
          | Some (on_left, bound) -> let inc = (step > 0L) in
            if SingleEntryType.is_val input_var_set bound then
              let bound_1 = SingleEntryType.eval (SingleBExp (SingleSub, bound, SingleConst step)) in
              let bound_2 = SingleEntryType.eval (SingleBExp (SingleSub, bound_1, SingleConst step)) in
              begin match bound_match on_left inc cond, inc with
              | (true, true), true -> 
                SolCond (branch_pc, Range (base, bound, step), Range (base, bound_1, step), Single bound)
              | (true, true), false -> 
                SolCond (branch_pc, Range (bound, base, Int64.neg step), Range (bound_1, base, Int64.neg step), Single bound)
              | (true, false), true ->
                SolCond (branch_pc, Range (base, bound_1, step), Range (base, bound_2, step), Single bound_1)
              | (true, false), false ->
                SolCond (branch_pc, Range (bound_1, base, Int64.neg step), Range (bound_2, base, Int64.neg step), Single bound_1)
              | _ -> SolNone
              end
            else SolNone
          | None ->
            (* Represent sol with the actual loop counter *)
            let find_other =
              List.find_map (
                fun (tv: type_rel) ->
                  let v_idx, v_pc = tv.var_idx in
                  if v_pc != target_pc then None
                  else begin
                    match tv.sol with
                    | SolCond (v_br_pc, Range (v_base, _, v_step), _, _) ->
                      if v_br_pc = target_pc then
                        if Int64.rem step v_step = 0L then
                          Some (SingleEntryType.eval (
                            SingleBExp (
                              SingleMul,
                              SingleBExp (SingleAdd, SingleBExp (SingleSub, SingleVar v_idx, v_base), base),
                              SingleConst (Int64.div step v_step)
                            )
                          ))
                        else single_subtype_error (Printf.sprintf "cannot represent range of symimm %d with symimm %d" target_idx v_idx)
                      else None
                    | _ -> None
                  end
              ) tv_rel_list
            in
            begin match find_other with
            | Some sol -> SolSimple (Single sol)
            | None -> 
              Printf.printf "Warning: cannot find sol for var %d with find loop cond\n" target_idx;
              SolNone
            end
          end
      | None -> SolNone
    in
    let solve_rules = [
      try_solve_top;
      try_solve_single_sub_val;
      try_solve_loop_cond
    ] in
    List.fold_left 
      (fun acc rule -> if acc = SingleSol.SolNone then rule tv_rel.subtype_list else acc) 
      tv_rel.sol solve_rules

  let subsititue_one_exp_single_sol
      (exp_pc: type_exp_t)
      (idx_sol: var_idx_t * SingleSol.t) : type_exp_t =
    let exp, e_pc = exp_pc in
    let (idx, i_pc), sol = idx_sol in
    if e_pc != i_pc then exp_pc
    else
      match sol with
      | SolSimple (Single e) -> SingleEntryType.repl_var_exp exp (idx, e), e_pc
      | _ -> exp_pc

  let subsititue_one_exp_single_sol_list
      (idx_sol_list: (var_idx_t * SingleSol.t) list)
      (exp_pc: type_exp_t) : type_exp_t =
    List.fold_left subsititue_one_exp_single_sol exp_pc idx_sol_list

  let update_subtype_single_sol
      (tv_rel_list: t) (idx_sol_list: (var_idx_t * SingleSol.t) list) : t =
    List.map (
      fun (tv_rel: type_rel) ->
        if tv_rel.sol != SolNone then tv_rel
        else
          { tv_rel with subtype_list = List.map (subsititue_one_exp_single_sol_list idx_sol_list) tv_rel.subtype_list }
    ) tv_rel_list

  let try_solve_vars
      (* (smt_ctx: SmtEmitter.t) *)
      (tv_rel_list: t)
      (block_subtype: ArchType.block_subtype_t list)
      (input_var_set: SingleEntryType.SingleVarSet.t) : t * bool =
    let new_sol_list, new_subtype = List.fold_left_map (
      fun acc tv_rel ->
        if tv_rel.sol != SolNone then acc, tv_rel
        else
          let sol = try_solve_one_var tv_rel tv_rel_list block_subtype input_var_set in
          if sol = SolNone then acc, tv_rel
          else (tv_rel.var_idx, sol) :: acc, { tv_rel with sol = sol}
    ) [] tv_rel_list
    in
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
      if num_iter = 0 then tv_rel_list
      else begin
        let new_subtype, found_new_sol = try_solve_vars tv_rel_list block_subtype input_var_set in
        if found_new_sol then helper new_subtype (num_iter - 1)
        else new_subtype
      end
    in
    helper tv_rel_list num_iter

  (* TODO:
    1. Handle calculation of range of a exp when var in the exp is not a single value.
    2. Solve by merging several ranges.
  *)

end
