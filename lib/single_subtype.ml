open Isa_basic
open Single_exp
open Single_entry_type
open Range_exp
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
    | SolNone -> RangeExp.to_smt_expr smt_ctx v_idx RangeExp.Top
    | SolSimple e
    | SolCond (_, e, _, _) -> RangeExp.to_smt_expr smt_ctx v_idx e

end

module SingleSubtype = struct
  exception SingleSubtypeError of string

  let single_subtype_error msg = raise (SingleSubtypeError ("[Single Subtype Error] " ^ msg))

  type var_pc_t = IsaBasic.imm_var_id * int
  [@@deriving sexp]
  type var_pc_list_t = IsaBasic.imm_var_id * (int * int) (* supertype var, (root pc, direct branch pc) *)
  [@@deriving sexp]
  type type_pc_t = SingleEntryType.t * int
  type type_pc_list_t = SingleEntryType.t * (int * int) (* subtype, (root pc, direct branch pc) *)
  [@@deriving sexp]

  type type_rel = {
    var_idx: var_pc_t;
    sol: SingleSol.t;
    subtype_list: type_pc_list_t list; (* subtype, pc of the branch that jumps to var_idx's block *)
    supertype_list: var_pc_list_t list (* NOTE: I omit br hist here since I noticed that current implementation cannot use it anyway *)
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
    List.iter (fun (sub, (pc1, pc2)) -> PP.print_lvl (lvl + 2) "%s, (%d %d);\n" (SingleEntryType.to_string sub) pc1 pc2) x.subtype_list;
    PP.print_lvl (lvl + 1) "]\n";
    PP.print_lvl (lvl + 1) "Supertype: [%s]\n" 
      (String.concat "; " (List.map (fun (v, (pc1, pc2)) -> Printf.sprintf "%d,(%d %d)" v pc1 pc2) x.supertype_list))

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
        sol = SingleSol.SolNone; subtype_list = []; supertype_list = [] } 
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
    | SingleVar a_idx, (_, a_direct_pc_list) ->
    (* | SingleVar a_idx, _ -> *)
      let tv_rel, a_entry = find_or_add_entry var_pc_map tv_rel a_idx in
      let tv_rel =
        List.fold_left (
          fun acc_tv_rel (sub_a_exp, (sub_a_root_pc, _)) ->
            add_one_sub_super acc_tv_rel (sub_a_exp, (sub_a_root_pc, a_direct_pc_list)) b_idx
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
        fun acc_tv_rel (sup_b, (_, b_direct_pc)) ->
        (* fun acc_tv_rel (sup_b, _) -> *)
          let a_exp, (a_root_pc, _) = a_exp_pc in
          add_sub_sub_super var_pc_map acc_tv_rel (a_exp, (a_root_pc, b_direct_pc)) sup_b
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
            add_sub_sub_super_super var_pc_map acc_tv_rel (sub, (sub_pc, sub_pc)) sup_idx,
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
    Printf.printf "after init useful var\n";
    pp_useful_var_list 0 useful_var_list;
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
        if SingleExp.SingleVarSet.mem var_idx useful_single_var then
          SmtEmitter.add_assertions smt_ctx [ to_smt_expr smt_ctx x ]
        else ()
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

  let sub_sol_single_to_range
      (tv_rel_list: t)
      (input_var_set: SingleEntryType.SingleVarSet.t)
      (e: type_pc_t) : RangeExp.t =
    let e, e_pc = e in
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
            SingleSet e_list
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
        | SingleAdd, Single e, SingleSet e_list
        | SingleAdd, SingleSet e_list, Single e ->
          SingleSet (List.map (fun x -> SingleEntryType.eval (SingleBExp (SingleAdd, e, x))) e_list)
        | SingleSub, Single e, Range (e1, e2, s) ->
          Range (SingleEntryType.eval (SingleBExp (SingleSub, e, e2)), SingleEntryType.eval (SingleBExp (SingleSub, e, e1)), s)
        | SingleSub, Range (e1, e2, s), Single e ->
          Range (SingleEntryType.eval (SingleBExp (SingleSub, e, e1)), SingleEntryType.eval (SingleBExp (SingleSub, e, e2)), s)
        | SingleSub, Single e, SingleSet e_list ->
          SingleSet (List.map (fun x -> SingleEntryType.eval (SingleBExp (SingleAdd, e, x))) e_list)
        | SingleSub, SingleSet e_list, Single e ->
          SingleSet (List.map (fun x -> SingleEntryType.eval (SingleBExp (SingleAdd, x, e))) e_list)
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
            Int64.neg (Int64.mul c s))
        | SingleMul, Single e, SingleSet e_list
        | SingleMul, SingleSet e_list, Single e ->
          SingleSet (List.map (fun x -> SingleEntryType.eval (SingleBExp (SingleMul, e, x))) e_list)
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
        | SingleSar, SingleSet e_list, Single (SingleConst c) ->
          SingleSet (List.map (fun x -> SingleEntryType.eval (SingleBExp (SingleSar, x, SingleConst c))) e_list)
        | SingleSal, Range (e1, e2, s), Single (SingleConst c) ->
          let s_sal = Int64.shift_left s (Int64.to_int c) in
          if Int64.shift_right s_sal (Int64.to_int c) = s then
            Range (SingleEntryType.eval (SingleBExp (SingleSal, e1, SingleConst c)),
              SingleEntryType.eval (SingleBExp (SingleSal, e2, SingleConst c)),
              Int64.shift_left s (Int64.to_int c))
          else Top
        | SingleSal, SingleSet e_list, Single (SingleConst c) ->
          SingleSet (List.map (fun x -> SingleEntryType.eval (SingleBExp (SingleSal, x, SingleConst c))) e_list)
        | _, r1, r2 -> 
          Printf.printf "sub_sol_single_to_range: WARNING! %s not handled l %s r %s\n" 
            (SingleExp.to_string e) (RangeExp.to_string r1) (RangeExp.to_string r2); 
          Top (* TODO: maybe need to handle more cases here *)
        end
      | SingleUExp _ -> Top
    in
    helper e

  let sub_sol_single_to_range_opt
      (tv_rel_list: t)
      (input_var_set: SingleEntryType.SingleVarSet.t)
      (e: type_pc_t) : RangeExp.t option =
    let exp, _ = e in
    let resolved_vars = 
      List.filter_map (fun (x: type_rel) -> if x.sol <> SolNone then let idx, _ = x.var_idx in Some idx else None) tv_rel_list
    in
    if SingleExp.is_val (SingleExp.SingleVarSet.union (SingleExp.SingleVarSet.of_list resolved_vars) input_var_set) exp then
      Some (sub_sol_single_to_range tv_rel_list input_var_set e)
    else None

  let sub_sol_single_to_single_func_interface
      (tv_rel_list: t)
      (input_var_set: SingleEntryType.SingleVarSet.t)
      (pc: int) (e: SingleEntryType.t) : SingleEntryType.t =
    let r = 
      sub_sol_single_to_range tv_rel_list input_var_set (e, pc) 
    in
    match r with
    | Single exp -> exp
    | _ -> SingleTop

  let try_solve_one_var
      (* (smt_ctx: SmtEmitter.t) *) (* Maybe I need add_no_overflow later *)
      (tv_rel: type_rel)
      (tv_rel_list: t)
      (block_subtype: ArchType.block_subtype_t list)
      (input_var_set: SingleEntryType.SingleVarSet.t) : SingleSol.t =
    let target_idx, target_pc = tv_rel.var_idx in
    let try_solve_top (subtype_list: type_pc_list_t list) : SingleSol.t =
      match List.find_opt (fun (x, _) -> x = SingleEntryType.SingleTop) subtype_list with
      | Some _ -> SolSimple Top
      | None -> SolNone
    in
    let try_solve_single_sub_val (subtype_list: type_pc_list_t list) : SingleSol.t =
      let subtype_list = (* remove non-input SingleVar *)
        List.filter (
          fun (e, _) ->
            match e with
            | SingleEntryType.SingleVar _ -> SingleEntryType.is_val input_var_set e
            | _ -> true
        ) subtype_list
      in
      if List.find_opt (fun (x, _) -> not (SingleEntryType.is_val input_var_set x)) subtype_list = None then
        match List.map (fun (x, _ ) -> x) subtype_list with
        | [] -> SolNone
        | hd :: [] -> SolSimple (Single hd)
        | subtype_list -> SolSimple (SingleSet subtype_list)
      else begin 
        (* To handle the case where exp contains resolved block vars *)
        let sub_range_opt_list = List.map (fun (x, pc_list) -> sub_sol_single_to_range_opt tv_rel_list input_var_set (x, fst pc_list)) subtype_list in
        let sub_range_list = List.filter_map (fun x -> x) sub_range_opt_list in
        if List.length sub_range_opt_list <> List.length sub_range_list then SolNone
        else if List.find_opt (fun x -> x = RangeExp.Top) sub_range_list <> None then SolSimple (Top)
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
          | single_list, [] -> SolSimple (SingleSet single_list)
          | s :: [], (l, r, step) :: [] ->
            if SingleEntryType.cmp s (SingleEntryType.eval (SingleBExp (SingleAdd, r, SingleConst step))) = 0 then
              SolSimple (Range (l, s, step))
            else if SingleEntryType.cmp s (SingleEntryType.eval (SingleBExp (SingleSub, l, SingleConst step))) = 0 then
              SolSimple (Range (s, r, step))
            else SolNone
          | _ -> SolNone (* TODO: Maybe need to improve this!!! *)
        end
      end
    in
    let find_base_step 
        (supertype_list: var_pc_list_t list)
        (subtype_list: type_pc_list_t list) (v_idx: IsaBasic.imm_var_id) : 
        (SingleEntryType.t * type_pc_list_t * int64) option =
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
                else Some (x_pc, s)
              else if List.find_opt (fun (sup_idx, _) -> v = sup_idx) supertype_list <> None then
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
      (* x+step <> / < / <= bound *)
      | true, true, Ne
      | true, true, Lt | true, true, Bt -> true, false
      | true, true, Le | true, true, Be -> true, true
      (* bound <> / < / <= x-step *)
      | false, false, Ne
      | false, false, Lt | false, false, Bt -> true, false
      | false, false, Le | false, false, Be -> true, true
      (* x-step <> bound *)
      | true, false, Ne -> true, false
      (* bound <> x+step *)
      | false, true, Ne -> true, false
      (* does not match *)
      | _ -> false, false
    in
    let try_solve_loop_cond (supertype_list: var_pc_list_t list) (subtype_list: type_pc_list_t list) : SingleSol.t =
      match find_base_step supertype_list subtype_list target_idx with
      | Some (_, _, 0L) -> single_subtype_error "try_solve_loop_cond: find step 0" 
      | Some (base, (var_step, branch_pc), step) ->
        begin match ArchType.get_branch_cond block_subtype (fst branch_pc) with
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
            (* TODO: Need to repl bound here!!! *)
            let bound = (
              (* Very imporant: here bound comes from the branch condition, which should be evaluated in the context of branch_pc - 1!!! *)
              begin match sub_sol_single_to_range_opt tv_rel_list input_var_set (bound, (fst branch_pc) - 1) with
              | Some (Single e) -> e
              | Some _ -> bound
              | None -> SingleTop
              end
            ) in
            if bound <> SingleTop && SingleEntryType.is_val input_var_set bound then
              let bound_1 = SingleEntryType.eval (SingleBExp (SingleSub, bound, SingleConst step)) in
              let bound_2 = SingleEntryType.eval (SingleBExp (SingleSub, bound_1, SingleConst step)) in
              begin match bound_match on_left inc cond, inc with
              | (true, true), true -> 
                SolCond (fst branch_pc, Range (base, bound, step), Range (base, bound_1, step), Single bound)
              | (true, true), false -> 
                SolCond (fst branch_pc, Range (bound, base, Int64.neg step), Range (bound_1, base, Int64.neg step), Single bound)
              | (true, false), true ->
                SolCond (fst branch_pc, Range (base, bound_1, step), Range (base, bound_2, step), Single bound_1)
              | (true, false), false ->
                SolCond (fst branch_pc, Range (bound_1, base, Int64.neg step), Range (bound_2, base, Int64.neg step), Single bound_1)
              | _ -> 
                Printf.printf "Bound match cannot find sol %s bound %s\n" (ArchType.CondType.to_string (cond, l, r)) (SingleExp.to_string bound);
                SolNone
              end
            else begin
              (* Printf.printf "!!!\n";
              pp_single_subtype 0 tv_rel_list;
              Printf.printf "!!!\n"; *)
              Printf.printf "Bound is not val %s bound %s\n" (ArchType.CondType.to_string (cond, l, r)) (SingleExp.to_string bound);
              SolNone
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
                    | Some (_, _, 0L) -> None
                    | Some (_, (other_step, _), _) ->
                      (* NOTE: Must ensure the other_var is indeed in the cond!!! *)
                      if SingleEntryType.cmp l other_step = 0 || SingleEntryType.cmp r other_step = 0 then begin
                        match tv.sol with
                        | SolCond (v_br_pc, Range (v_base, _, v_step), _, _) ->
                          if v_br_pc = fst branch_pc then
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
                                (* SingleBExp (
                                  SingleMul,
                                  SingleBExp (SingleAdd, SingleBExp (SingleSub, SingleVar v_idx, v_base), base),
                                  SingleConst (Int64.div step v_step)
                                ) *)
                              ))
                            else single_subtype_error (Printf.sprintf "cannot represent range of symimm %d with symimm %d" target_idx v_idx)
                          else None
                        | _ -> None
                      end else None
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
      try_solve_loop_cond tv_rel.supertype_list;
      try_solve_single_sub_val;
    ] in
    List.fold_left 
      (fun acc rule -> if acc = SingleSol.SolNone then rule tv_rel.subtype_list else acc) 
      tv_rel.sol solve_rules

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
        let r = if (fst e_pc) < pc then r_before_branch else if (fst e_pc) = pc then r_taken else r_not_taken in
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
    let exp, _ = subsititue_one_exp_single_sol_list idx_sol_list (exp, (pc, pc)) in
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
            | None, (hd_exp, (hd_root_pc, hd_direct_pc)) :: tl -> (hd_exp, (hd_root_pc, hd_direct_pc)) :: (merge (Some (hd_exp, hd_direct_pc)) tl)
            | Some (last_exp, last_pc), (hd_exp, (hd_root_pc, hd_direct_pc)) :: tl ->
              if SingleEntryType.cmp hd_exp last_exp = 0 && last_pc = hd_direct_pc then merge last_entry tl
              else (hd_exp, (hd_root_pc, hd_direct_pc)) :: (merge (Some (hd_exp, hd_direct_pc)) tl)
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
          let sol = try_solve_one_var tv_rel tv_rel_list block_subtype input_var_set in
          if sol = SolNone then acc, tv_rel
          else (tv_rel.var_idx, sol) :: acc, { tv_rel with sol = sol}
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
