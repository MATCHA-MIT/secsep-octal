open Isa_basic
open Single_exp
open Single_entry_type
open Mem_offset_new
open Range_exp
open Cond_type_new
open Single_context
open Arch_type
open Arch_context_map
open Single_ite_eval
open Set_sexp
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

  let cmp (s1: t) (s2: t) : int =
    match s1, s2 with
    | SolNone, SolNone -> 0
    | SolNone, _ -> -1
    | SolSimple _, SolNone -> 1
    | SolSimple r1, SolSimple r2 -> RangeExp.cmp r1 r2
    | SolSimple _, SolCond _ -> -1
    | SolCond (pc1, a1, b1, c1), SolCond (pc2, a2, b2, c2) ->
      let cmp_pc = compare pc1 pc2 in
      if cmp_pc = 0 then
        let cmp_a = RangeExp.cmp a1 a2 in
        if cmp_a = 0 then
          let cmp_b = RangeExp.cmp b1 b2 in
          if cmp_b = 0 then RangeExp.cmp c1 c2
          else cmp_b
        else cmp_a
      else cmp_pc
    | SolCond _, _ -> 1

  let get_vars (s: t) : IntSet.t =
    match s with
    | SolNone -> IntSet.empty
    | SolSimple r -> RangeExp.get_var r
    | SolCond (_, r1, r2, r3) ->
      RangeExp.get_var r1 |> (IntSet.union (RangeExp.get_var r2)) |> (IntSet.union (RangeExp.get_var r3))

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

  let has_top (s: t) : bool =
    match s with
    | SolNone | SolSimple Top -> true
    | SolSimple r -> RangeExp.has_top r
    | SolCond (_, r1, r2, r3) -> RangeExp.has_top r1 || RangeExp.has_top r2 || RangeExp.has_top r3

  let to_smt_expr (smt_ctx: SmtEmitter.t) (v_idx: int) (s: t) : SmtEmitter.exp_t =
    if has_top s then SmtEmitter.mk_true smt_ctx else
    match s with
    (* | SolNone | SolSimple Top | SolSimple (Single SingleTop) -> SmtEmitter.mk_true smt_ctx *)
      (* RangeExp.to_smt_expr smt_ctx v_idx RangeExp.Top *)
    | SolSimple e
    | SolCond (_, e, _, _) -> RangeExp.to_smt_expr smt_ctx v_idx e
    | _ -> single_sol_error "should not be reachable"

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
  [@@deriving sexp]
  type type_pc_list_t = SingleEntryType.t * (int list) (* subtype, path pc list *)
  [@@deriving sexp]

  let rec list_tl (x: 'a list) : 'a =
    match x with
    | [] -> single_subtype_error "list_tl cannot get tl of empty list"
    | hd :: [] -> hd
    | _ :: tl -> list_tl tl

  let merge_pc_list (pc_list1: 'a list) (pc_list2: 'a list) : 'a list =
    match pc_list1, pc_list2 with
    | [], _ | _, [] -> single_subtype_error "cannot merge empty pc_list"
    (* | l1, [] -> l1
    | [], l2 -> l2 *)
    | _, _ -> [ List.hd pc_list1; list_tl pc_list2 ]

  type subtype_info_t = {
    sub_exp: SingleExp.t;
    path_pc_list: int list;
    is_leaf: bool; 
    (* During init, if the subtype (not simplified version) is a block var, 
      then is_leaf=false since we will add the var's subtype too.*)
  }
  [@@deriving sexp]

  type loop_info_t = {
    base: SingleExp.t;
    step_exp: SingleExp.t;
    step_pc_list: int list;
    step_var_idx: int;
    step_val: int64;
  }
  [@@deriving sexp]

  type type_rel = {
    var_idx: var_pc_t;
    sol: SingleSol.t;
    other_constraint: SingleContext.t list;
    subtype_list: subtype_info_t list; (* subtype, pc of the branch that jumps to var_idx's block *)
    supertype_list: var_pc_list_t list; (* NOTE: I omit br hist here since I noticed that current implementation cannot use it anyway *)
    set_sol: type_pc_list_t list; (* Dirty trick to record information to resolve relation between vars whose solution is a set of values; is empty list if not used *)
    loop_info: loop_info_t option;
  }
  [@@deriving sexp]

  type t = type_rel list
  [@@deriving sexp]

  module ArchType = ArchType (SingleEntryType)

  module SingleExpPcSet = Set.Make (
    struct
      type t = type_pc_t
      let compare (a: t) (b: t) : int =
        let a_exp, a_pc = a in
        let b_exp, b_pc = b in
        let cmp_exp = SingleExp.cmp a_exp b_exp in
        if cmp_exp = 0 then compare a_pc b_pc
        else cmp_exp
    end
  )

  let pp_type_rel (lvl: int) (x: type_rel) =
    let var_idx, var_pc = x.var_idx in
    PP.print_lvl lvl "<SymImm %d> at pc %d\n" var_idx var_pc;
    PP.print_lvl (lvl + 1) "Sol: %s\n" (SingleSol.to_string x.sol);
    PP.print_lvl (lvl + 1) "Subtype: [\n";
    List.iter (fun (sub: subtype_info_t) -> PP.print_lvl (lvl + 2) "%s, (%s), %b;\n" (SingleEntryType.to_string sub.sub_exp) (Sexplib.Sexp.to_string (sexp_of_list sexp_of_int sub.path_pc_list)) sub.is_leaf) x.subtype_list;
    PP.print_lvl (lvl + 1) "]\n";
    PP.print_lvl (lvl + 1) "Supertype: [%s]\n" 
      (String.concat "; " (List.map (fun (v, pc) -> Printf.sprintf "%d,(%s)" v (Sexplib.Sexp.to_string (sexp_of_list sexp_of_int pc))) x.supertype_list));
    PP.print_lvl (lvl + 1) "Sol set: %s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list sexp_of_type_pc_list_t x.set_sol));
    PP.print_lvl (lvl + 1) "Loop info: %s\n" 
      (Sexplib.Sexp.to_string_hum (sexp_of_option sexp_of_loop_info_t x.loop_info))

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
      let var_idx_pc =
        match List.find_opt (fun (v_idx, _) -> v_idx = var_idx) var_pc_map with
        | Some var_idx_pc -> var_idx_pc
        | None -> single_subtype_error (Printf.sprintf "cannot find var %d" var_idx)
      in
      let entry = 
        { var_idx = var_idx_pc; 
        sol = SingleSol.SolNone; 
        other_constraint = [];
        subtype_list = []; supertype_list = [];
        set_sol = [];
        loop_info = None } 
      in
      entry :: tv_rel, entry

  let filter_entry (tv_rel: t) (var_set: SingleExp.SingleVarSet.t) : t =
    List.filter (
      fun (x: type_rel) ->
        let v_idx, _ =  x.var_idx in
        SingleExp.SingleVarSet.mem v_idx var_set
    ) tv_rel

  let get_subtype_info (input_var_set: IntSet.t) (sub: SingleExp.t) (sub_pc: int) : subtype_info_t =
    let is_leaf =
      match sub with
      | SingleVar v -> IntSet.mem v input_var_set
      | _ -> true
    in
    { sub_exp = sub; path_pc_list = [sub_pc]; is_leaf = is_leaf }

  let type_list_insert 
      (type_list: subtype_info_t list) (ty: subtype_info_t) : subtype_info_t list =
    let find_type = 
      List.find_opt (
        fun sub -> 
          SingleEntryType.cmp ty.sub_exp sub.sub_exp = 0 
          && ty.path_pc_list = sub.path_pc_list
          && ty.is_leaf = sub.is_leaf
      ) type_list
    in
    if find_type <> None then type_list else ty :: type_list

  let type_var_list_insert
      (type_var_list: var_pc_list_t list) (var_idx: var_pc_list_t) : var_pc_list_t list =
    if List.find_opt (fun x -> x = var_idx) type_var_list <> None then type_var_list else var_idx :: type_var_list

  let add_one_sub_type (tv_rel: type_rel) (ty: subtype_info_t) : type_rel =
    { tv_rel with subtype_list = type_list_insert tv_rel.subtype_list ty }

  let add_one_super_type (tv_rel: type_rel) (t_idx: var_pc_list_t) : type_rel =
    { tv_rel with supertype_list = type_var_list_insert tv_rel.supertype_list t_idx }

  (* Connect a->b *)
  let add_one_sub_super (tv_rel: t) (a_exp_pc: subtype_info_t) (b_idx: int) : t =
    match a_exp_pc.sub_exp with
    | SingleVar a_idx ->
      List.map (
        fun x ->
          let x_idx, _ = x.var_idx in
          (* for x_idx = b_idx = a_idx, we do both add_one_sub_type and add_one_super_type*)
          let x = if x_idx = b_idx then add_one_sub_type x a_exp_pc else x in
          if x_idx = a_idx then add_one_super_type x (b_idx, a_exp_pc.path_pc_list) else x
          (* if x_idx = b_idx then add_one_sub_type x a_exp_pc
          else if x_idx = a_idx then add_one_super_type x (b_idx, a_pc)
          else x *)
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
      (tv_rel: t) (a_exp_pc: subtype_info_t) (b_idx: int) : t =
    match a_exp_pc.sub_exp with
    | SingleVar a_idx ->
    (* | SingleVar a_idx, _ -> *)
      let tv_rel =
        if a_idx = b_idx then tv_rel
        else
          let tv_rel, a_entry = find_or_add_entry var_pc_map tv_rel a_idx in
          List.fold_left (
            fun acc_tv_rel sub_a -> (* (sub_a_exp, sub_a_pc_list) -> *)
              add_one_sub_super acc_tv_rel { sub_a with path_pc_list = merge_pc_list sub_a.path_pc_list a_exp_pc.path_pc_list } b_idx
              (* add_one_sub_super acc_tv_rel (sub_a_exp, sub_a_pc_list) b_idx *)
          ) tv_rel a_entry.subtype_list 
        in
      add_one_sub_super tv_rel a_exp_pc b_idx
    | _ -> add_one_sub_super tv_rel a_exp_pc b_idx

  let add_sub_sub_super_super
      (var_pc_map: var_pc_t list)
      (tv_rel: t) (a_exp_pc: subtype_info_t) (b_idx: int) : t =
    let tv_rel, b_entry = find_or_add_entry var_pc_map tv_rel b_idx in
    let tv_rel =
      List.fold_left (
        fun acc_tv_rel (sup_b, b_pc_list) ->
        (* fun acc_tv_rel (sup_b, _) -> *)
          if sup_b = b_idx then acc_tv_rel else
          (* let a_exp, a_pc_list = a_exp_pc in *)
          (* add_sub_sub_super var_pc_map acc_tv_rel (a_exp, merge_pc_list a_pc_list b_pc_list) sup_b *)
          add_sub_sub_super var_pc_map acc_tv_rel { a_exp_pc with path_pc_list = merge_pc_list a_exp_pc.path_pc_list b_pc_list } sup_b
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
      (input_var_set: IntSet.t)
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
            add_sub_sub_super_super var_pc_map acc_tv_rel (get_subtype_info input_var_set sub sub_pc) sup_idx,
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
    let tv_rel, sub_useful_list = List.fold_left_map helper tv_rel sub_block_list in
    let tv_rel, sup_useful =
      List.fold_left (
        fun (acc: t * SingleExp.SingleVarSet.t) (subtype_entry: IsaBasic.imm_var_id * (SingleExp.t list)) ->
          let sup_idx, sub_list = subtype_entry in
          List.fold_left (
            fun (acc: t * SingleExp.SingleVarSet.t) (sub: SingleExp.t) ->
              let acc_tv_rel, acc_useful = acc in
              if SingleEntryType.SingleVarSet.mem sup_idx useful_var then
                let sub = SingleEntryType.repl_local_var sup_block.local_var_map sub in
                add_sub_sub_super_super var_pc_map acc_tv_rel (get_subtype_info input_var_set sub sup_block.pc) sup_idx,
                SingleEntryType.SingleVarSet.union acc_useful (SingleEntryType.get_vars sub)
              else acc
          ) acc sub_list
      ) (tv_rel, SingleExp.SingleVarSet.empty) sup_block.extra_call_subtype_list
    in
    tv_rel, (sup_block.label, sup_useful) :: sub_useful_list

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
      (input_var_set: IntSet.t)
      (var_pc_map: var_pc_t list)
      (block_subtype_list: ArchType.block_subtype_t list)
      (useful_var: useful_var_t)
      (tv_rel: t) :
      t * (ArchType.block_subtype_t list) * (useful_var_t list) =
    let sup_label, sup_useful_var = useful_var in
    let block_subtype = 
      List.find (fun (x: ArchType.block_subtype_t) -> let x, _ = x in x.label = sup_label) block_subtype_list 
    in
    let tv_rel, new_useful_var_list = add_one_useful_var_block_subtype_helper input_var_set var_pc_map block_subtype sup_useful_var tv_rel in
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

  let add_useful_var_block_subtype_one_iter
      (input_var_set: IntSet.t)
      (var_pc_map: var_pc_t list)
      (block_subtype_list: ArchType.block_subtype_t list)
      (useful_var_list: useful_var_t list)
      (tv_rel: t) : t * (ArchType.block_subtype_t list) * useful_var_t list =
    List.fold_left (
      fun (acc: t * (ArchType.block_subtype_t list) * useful_var_t list) (entry: useful_var_t) ->
        let acc_tv_rel, acc_block_subtype_list, acc_new_useful_var_list = acc in
        let acc_tv_rel, acc_block_subtype_list, new_useful_var_list =
          add_one_useful_var_block_subtype input_var_set var_pc_map acc_block_subtype_list entry acc_tv_rel
        in
        acc_tv_rel, acc_block_subtype_list, merge_useful_var acc_new_useful_var_list new_useful_var_list
    ) (tv_rel, block_subtype_list, []) useful_var_list

  let rec add_all_useful_var_block_subtype
      (input_var_set: IntSet.t)
      (var_pc_map: var_pc_t list)
      (block_subtype_list: ArchType.block_subtype_t list)
      (useful_var_list: useful_var_t list)
      (* (tv_rel: t) : t * (ArchType.block_subtype_t list) = *)
      (tv_rel: t) (count: int) : t * (ArchType.block_subtype_t list) =
    (* Printf.printf "add_all_useful_var_block_subtype %d\n" count;
    Printf.printf "before\n";
    pp_useful_var_list 0 useful_var_list;
    Printf.printf "%!\n"; *)
    match useful_var_list with
    | [] -> tv_rel, block_subtype_list
    | _ ->
      let tv_rel, block_subtype_list, useful_var_list = 
        add_useful_var_block_subtype_one_iter input_var_set var_pc_map block_subtype_list useful_var_list tv_rel
      in
      add_all_useful_var_block_subtype input_var_set var_pc_map block_subtype_list useful_var_list tv_rel (count - 1)

  let get_all_useful_local_var (all_useful_var: IntSet.t) (block_subtype_list: ArchType.block_subtype_t list) : IntSet.t =
    (* Get all useful var and local var derived from useful var *)
    let add_uesful_local_var (useful_var: IntSet.t) (local_var_map: SingleExp.local_var_map_t) : IntSet.t =
      List.fold_left (
        fun (acc: IntSet.t) (var, exp) ->
          if IntSet.mem var useful_var then
            acc
          else if SingleExp.is_val useful_var exp then
            IntSet.add var acc
          else acc
      ) useful_var local_var_map
    in
    List.fold_left (
      fun (acc: IntSet.t) (entry: ArchType.block_subtype_t) ->
        let block, _ = entry in
        (* let acc = IntSet.union acc block.useful_var in *)
        add_uesful_local_var acc block.local_var_map
    ) all_useful_var block_subtype_list

  module VarSubMap = IntMapSexp (
    struct
      type t = (SingleEntryType.t * IsaBasic.label * int) list
      [@@deriving sexp]
    end
  )
  
  let update_var_sub_map_one_block_subtype
      (input_var_set: IntSet.t)
      (all_local_var_map: SingleExp.local_var_map_t StrMap.t)
      (var_pc_map: var_pc_t list) 
      (all_useful_var: IntSet.t)
      (tv_rel_list: t)
      (block_subtype: ArchType.block_subtype_t) :
      IntSet.t * t * ArchType.block_subtype_t =
    let target_block, sub_block_list = block_subtype in
    let update_one_pair
        (all_useful_var: IntSet.t) (sub_label: IsaBasic.label) (sub_pc: int)
        (acc: VarSubMap.t) (sup: SingleExp.t) (sub: SingleExp.t) : VarSubMap.t =
      match sup with
      | SingleVar sup_v ->
        if IntSet.mem sup_v all_useful_var then acc else
        let new_sub = sub, sub_label, sub_pc in
        VarSubMap.update sup_v (
          fun (entry_opt: ((SingleEntryType.t * IsaBasic.label * int) list) option) ->
            match entry_opt with
            | None -> Some [ new_sub ]
            | Some sub_list -> Some (new_sub :: sub_list)
        ) acc
      | SingleTop -> acc
      | _ -> single_subtype_error 
          (Printf.sprintf "update_var_sub_map: incorrect sub/super types (%s,%d)->%s"
            (SingleEntryType.to_string sub) sub_pc (SingleEntryType.to_string sup))
    in
    let update_one_block_pair
        (all_useful_var: IntSet.t)
        (acc: VarSubMap.t) (sub_block: ArchType.t) : VarSubMap.t =
      let helper = update_one_pair all_useful_var sub_block.label sub_block.pc in
      let acc = List.fold_left2 helper acc target_block.reg_type sub_block.reg_type in
      ArchType.MemType.fold_left2 helper acc target_block.mem_type sub_block.mem_type
    in
    let var_sub_map = 
      List.fold_left (update_one_block_pair all_useful_var) VarSubMap.empty sub_block_list
      |> VarSubMap.to_list
    in
    (* Check whether sub are all useful. If so, add them to all_useful_var and tv_rel_list *)
    let all_useful_var, new_useful_var, tv_rel_list =
      List.fold_left (
        fun (acc: IntSet.t * IntSet.t * t) (var_id, sub_list) ->
          let find_not_useful =
            List.find_opt (
              fun (e, _, _) ->
                not (SingleExp.is_val all_useful_var e)
            ) sub_list
          in
          if find_not_useful <> None then acc else
          let sub_list =
            List.map (
              fun (sub_exp, sub_label, sub_pc) ->
                get_subtype_info input_var_set
                (SingleEntryType.repl_local_var (StrMap.find sub_label all_local_var_map) sub_exp)
                sub_pc
            ) sub_list
          in
          let acc_all_useful_var, acc_new_useful_var, acc_tv_rel_list = acc in
          IntSet.add var_id acc_all_useful_var,
          IntSet.add var_id acc_new_useful_var,
          List.fold_left (
            fun (acc_tv_rel_list: t) (entry: subtype_info_t) ->
              add_sub_sub_super_super var_pc_map acc_tv_rel_list entry var_id
          ) acc_tv_rel_list sub_list
      ) (all_useful_var, IntSet.empty, tv_rel_list) var_sub_map
    in
    if not (IntSet.is_empty new_useful_var) then
      Printf.printf "Block %s %d new useful var %s\n" 
        target_block.label target_block.pc 
        (Sexplib.Sexp.to_string_hum (IntSet.sexp_of_t new_useful_var));
    all_useful_var, tv_rel_list,
    ({ target_block with useful_var = IntSet.union target_block.useful_var new_useful_var}, sub_block_list)
    
  let add_all_useful_var_forward_prop
      (input_var_set: IntSet.t)
      (var_pc_map: var_pc_t list)
      (tv_rel_list: t)
      (block_subtype_list: ArchType.block_subtype_t list) :
      t * (ArchType.block_subtype_t list) =
    let all_useful_var =
      List.fold_left (
        fun (acc: IntSet.t) (entry: ArchType.block_subtype_t) ->
          IntSet.union acc (fst entry).useful_var
      ) IntSet.empty block_subtype_list
    in
    let all_local_var_map =
      List.map (
        fun (entry: ArchType.block_subtype_t) ->
          let target_block, _ = entry in
          target_block.label, target_block.local_var_map
      ) block_subtype_list |> StrMap.of_list
    in
    let rec helper
        (all_useful_var: IntSet.t)
        (tv_rel_list: t)
        (block_subtype_list: ArchType.block_subtype_t list) :
        t * (ArchType.block_subtype_t list) =
      let all_useful_var = get_all_useful_local_var all_useful_var block_subtype_list in
      let old_useful_var_count = IntSet.cardinal all_useful_var in
      let (all_useful_var, tv_rel_list), block_subtype_list =
        List.fold_left_map (
          fun (x, y) z -> 
            let x, y, z = 
              update_var_sub_map_one_block_subtype input_var_set all_local_var_map var_pc_map x y z
            in
            (x, y), z
        ) (all_useful_var, tv_rel_list) block_subtype_list
      in
      let new_useful_var_count = IntSet.cardinal all_useful_var in
      if new_useful_var_count > old_useful_var_count then
        helper all_useful_var tv_rel_list block_subtype_list
      else tv_rel_list, block_subtype_list
    in
    helper all_useful_var tv_rel_list block_subtype_list
  
  let init_useful_var_from_block_subtype
      (block_subtype_list: ArchType.block_subtype_t list) : useful_var_t list =
    List.map (
      fun (x: ArchType.block_subtype_t) ->
        let x, _ = x in
        x.label, x.useful_var
    ) block_subtype_list
    
  let init
      (func_name: string)
      (input_var_set: IntSet.t)
      (block_subtype_list: ArchType.block_subtype_t list) : t * (ArchType.block_subtype_t list) =
    (* Reverse local_var_map, which help to infer vars derived from useful vars and add them to useful_var_list *)
    let block_subtype_list =
      List.map (
        fun (entry: ArchType.block_subtype_t) ->
          let block, sub_list = entry in
          { block with local_var_map = List.rev block.local_var_map },
          sub_list
      ) block_subtype_list
    in
    let useful_var_list = init_useful_var_from_block_subtype block_subtype_list in
    let all_var_map_list =
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
    let tv_rel_list, block_subtype_list = 
      add_all_useful_var_block_subtype input_var_set all_var_map_list block_subtype_list useful_var_list [] 200
    in
    add_all_useful_var_forward_prop input_var_set all_var_map_list tv_rel_list block_subtype_list

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
      (sol: t) (a_type: ArchType.t) : SingleContext.t list =
      (* (useful_single_var: SingleExp.SingleVarSet.t) : SingleContext.t list = *)
    let block_var_set = ArchType.get_reg_mem_var_set a_type in
    let useful_single_var = SingleExp.SingleVarSet.inter block_var_set a_type.useful_var in
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

  let add_range_constraint_to_block_tmp_context
      (sol: t) (a_type: ArchType.t) : ArchType.t =                          
    (* Since we derive the solution by pattern matching with a subset of the subtypes, 
       we need to double check/assert this solution holds for all input control-flow paths. *)
    let block_var_set = ArchType.get_reg_mem_var_set a_type in
    let useful_single_var = SingleExp.SingleVarSet.inter block_var_set a_type.useful_var in
    let helper (x: type_rel) : SingleContext.t list =
      if not (SingleExp.SingleVarSet.mem (fst x.var_idx) useful_single_var) then [] else
      match x.sol with
      | SolCond (_, Range _, _, _) -> 
        (* Note: we can also be more careful to check/assert all solutions.
           For now I only double check for range solution since single/set solutions seem to already cover all input paths,
           while range solution is generated from one selected base and step subtypes.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               *)
        begin match to_context x with
        | Some sol_context -> sol_context :: x.other_constraint
        | None -> x.other_constraint
        end
      | _ -> []
    in
    let tmp_context = List.concat_map helper sol in
    { a_type with tmp_context = tmp_context @ a_type.tmp_context}

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
      (* (is_val_helper: SingleExp.SingleVarSet.t -> 'a -> bool) *)
      (tv_rel_list: t)
      (input_var_set: SingleExp.SingleVarSet.t)
      (e: SingleExp.t) : bool =
    (* New logic: if any block var in e has any unresolved dependency, then e is not resolved. *)
    let e_block_vars = IntSet.diff (SingleExp.get_vars e) input_var_set in
    if IntSet.is_empty e_block_vars then true else
    let dep_map =
      List.fold_left (
        fun (acc: IntSet.t IntMap.t) (tv_rel: type_rel) ->
          match tv_rel.sol with
          | SolNone -> acc
          | _ -> IntMap.add (fst tv_rel.var_idx) (SingleSol.get_vars tv_rel.sol) acc
      ) IntMap.empty tv_rel_list
    in
    let rec check_dep (checked_dep_set: IntSet.t) (unchecked_var_set: IntSet.t) : bool =
      (* checked_dep_set include 
          (1) input var; 
          (2) block var that has Sol (directly dep checked); 
          (3) block var in unchecked_var_set to be checked in this round. *)
      let acc_resolved, acc_unchecked_set =
        List.fold_left (
          fun (acc: bool * IntSet.t) (var_to_check: int) ->
            let acc_resolved, acc_unchecked_set = acc in
            if not acc_resolved then acc
            else begin
              match IntMap.find_opt var_to_check dep_map with
              | Some var_dep_set ->
                (* var_to_check has dep in var_dep_set, filter dep var not in checked_dep_set 
                    (has not been checked or not in the to_check_list) *)
                true,
                IntSet.union acc_unchecked_set (IntSet.diff var_dep_set checked_dep_set)
              | None -> (* var_to_check does not have sol, dep check fail *)
                false, acc_unchecked_set
            end
        ) (true, IntSet.empty) (IntSet.to_list unchecked_var_set)
      in
      if not acc_resolved then false
      else if IntSet.is_empty acc_unchecked_set then true
      else check_dep (IntSet.union checked_dep_set acc_unchecked_set) acc_unchecked_set
    in
    check_dep (IntSet.union input_var_set e_block_vars) e_block_vars

    (* let resolved_vars = 
      List.filter_map (
        fun (x: type_rel) -> 
          (* match x.sol with
          | SolNone | SolSimple Top -> None
          | _ -> let idx, _ = x.var_idx in Some idx *)
          if x.sol <> SolNone then let idx, _ = x.var_idx in Some idx else None
      ) tv_rel_list
    in
    SingleExp.is_val (
      SingleExp.SingleVarSet.union 
        (SingleExp.SingleVarSet.of_list resolved_vars) 
        input_var_set
    ) e *)

  let substitute_one_exp_single_sol
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
        (* Printf.printf "substitute_one_exp_single_sol %s %d %s" (SingleExp.to_string exp) idx (SingleExp.to_string e); *)
        SingleEntryType.eval (SingleEntryType.repl_var_exp exp (idx, e)), e_pc
      | SolCond (pc, r_before_branch, r_taken, r_not_taken) ->
        (* TODO: Double check this!!! *)
        let r = if (List.hd e_pc) < pc then r_before_branch else if (List.hd e_pc) = pc then r_taken else r_not_taken in
        begin match r with
        | Single e -> SingleEntryType.eval (SingleEntryType.repl_var_exp exp (idx, e)), e_pc
        | _ -> exp_pc
        end
      | _ -> exp_pc

  let substitute_one_exp_single_sol_list
      (idx_sol_list: (var_pc_t * SingleSol.t) list)
      (exp_pc: type_pc_list_t) : type_pc_list_t =
    let e_vars = SingleExp.get_vars (fst exp_pc) in
    let idx_sol_list =
      List.filter (
        fun ((var_idx, _), _) -> SingleExp.SingleVarSet.mem var_idx e_vars
      ) idx_sol_list
    in
    List.fold_left substitute_one_exp_single_sol exp_pc idx_sol_list

  let substitute_one_exp_subtype_list
      (tv_rel_list: t) (exp_pc: type_pc_t) : SingleEntryType.t =
    let e_vars = SingleExp.get_vars (fst exp_pc) in
    let idx_sol_list =
      List.filter_map (
        fun (x: type_rel) ->
          if SingleExp.SingleVarSet.mem (fst x.var_idx) e_vars then
            Some (x.var_idx, x.sol)
          else None
      ) tv_rel_list
    in
    let exp, pc = exp_pc in
    let exp, _ = substitute_one_exp_single_sol_list idx_sol_list (exp, [pc]) in
    exp

  type sol_list_t = (var_pc_t * SingleSol.t) list
  [@@deriving sexp]

  let rec update_subtype_single_sol
      (tv_rel_list: t) (idx_sol_list: (var_pc_t * SingleSol.t) list) : t =
    let idx_sol_exp_list =
      List.filter_map (
        fun ((idx, _), sol) ->
          match sol with
          | SingleSol.SolSimple (Single e) -> Some (idx, e)
          | _ -> None
      ) idx_sol_list
    in
    let sub_exp_helper = SingleExp.repl_local_var idx_sol_exp_list in
    let sub_range_helper = RangeExp.eval_helper sub_exp_helper in 
    let get_sol_helper (var_pc: int * int) : SingleExp.t option =
      let v, v_pc = var_pc in
      match List.find_opt (fun ((x, _), _) -> x = v) idx_sol_list with
      | None -> None
      | Some (_, sol) -> 
        begin match sol with
        | SolSimple (Single e) -> Some e
        | SolCond (pc, r_before_branch, r_taken, r_not_taken) ->
          (* TODO: Double check this!!! *)
          let r = if v_pc < pc then r_before_branch else if v_pc = pc then r_taken else r_not_taken in
          begin match r with
          | Single e -> Some e
          | _ -> None
          end
        | _ -> None
        end
    in
    let sub_subtype_helper (subtype: subtype_info_t) : subtype_info_t =
      { subtype with 
        sub_exp = SingleExp.repl_local_var_general get_sol_helper (subtype.sub_exp, List.hd subtype.path_pc_list) }
    in
    let remain_id_sol_list, tv_rel_list =
      List.fold_left_map (
        fun (acc: (var_pc_t * SingleSol.t) list) (tv_rel: type_rel) ->
          (* let tv_rel_pc = snd tv_rel.var_idx in
          let sub_exp_helper (e: SingleExp.t) : SingleExp.t =
            substitute_one_exp_single_sol_list idx_sol_list (e, [tv_rel_pc]) |> fst
          in *) 
          (* We update subtype_list whenever solution containts Top. *)
          let tv_rel =
            if SingleSol.has_top tv_rel.sol then
              { tv_rel with subtype_list = List.map sub_subtype_helper tv_rel.subtype_list }
            else tv_rel
          in
          match tv_rel.sol with
          | SolNone ->
            (* let v_idx, _ = tv_rel.var_idx in
            Printf.printf "Sub for %d sol list len %d\n" v_idx (List.length idx_sol_list); *)
            acc, (* Solution is not updated, do not need add to remain id_sol_list *)
            tv_rel (* subtype_list is updated above *)
          | SolSimple s -> 
            let new_s = sub_range_helper s in
            if RangeExp.cmp new_s s = 0 then acc, tv_rel
            else begin
              (tv_rel.var_idx, SolSimple new_s) :: acc,
              { tv_rel with sol = SolSimple new_s}
            end
          | SolCond (br_pc, s1, s2, s3) ->
            let new_sol: SingleSol.t = SolCond (br_pc, sub_range_helper s1, sub_range_helper s2, sub_range_helper s3) in
            if SingleSol.cmp new_sol tv_rel.sol = 0 then acc, tv_rel
            else begin
              (tv_rel.var_idx, new_sol) :: acc,
              { tv_rel with sol = new_sol}
            end
      ) [] tv_rel_list
    in
    match remain_id_sol_list with
    | [] -> tv_rel_list
    | _ -> update_subtype_single_sol tv_rel_list remain_id_sol_list

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
      let result: RangeExp.t = match e with
      | SingleTop -> Top
      | SingleConst c -> Single (SingleConst c)
      | SingleVar v ->
        if SingleEntryType.SingleVarSet.mem v input_var_set then Single e
        else begin match find_var_sol tv_rel_list v e_pc with
        | Single e_sub -> 
          if SingleEntryType.cmp e e_sub = 0 then Single e (* if solution is the var itself, then keep it there *)
          else helper e_sub
        | Range (l, r, step) -> 
          if l = SingleTop || r = SingleTop then Top
          else Range (l, r, step)
          (* if SingleExp.is_val input_var_set l && SingleExp.is_val input_var_set r then
            Range (l, r, step)
          else single_subtype_error (Printf.sprintf "sub_sol_single_to_range range %s contain local var\n" (RangeExp.to_string (Range (l, r, step)))) *)
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
        | SingleAdd, Range (e11, e12, s1), Range (e21, e22, s2) -> begin
            if (Int64.to_int s1) < 0 || (Int64.to_int s2) < 0 then begin
              Printf.printf "e1 %s\n%s\ne2 %s\n%s\n" 
                (SingleExp.to_string e1) (RangeExp.to_string (Range (e11, e12, s1)))
                (SingleExp.to_string e2) (RangeExp.to_string (Range (e21, e22, s2)));
              single_subtype_error "expecting left aligned now"
            end else
            let new_s = Z.to_int64 (Z.gcd (Z.of_int64 s1) (Z.of_int64 s2)) in
            Range (SingleEntryType.eval (SingleBExp (SingleAdd, e11, e21)), SingleEntryType.eval (SingleBExp (SingleAdd, e12, e22)), new_s)
          end
        (* | SingleAdd, Single e, SingleSet e_list
        | SingleAdd, SingleSet e_list, Single e ->
          SingleSet (List.map (fun x -> SingleEntryType.eval (SingleBExp (SingleAdd, e, x))) e_list) *)
        | SingleSub, Single e, Range (e1, e2, s) ->
          Range (SingleEntryType.eval (SingleBExp (SingleSub, e, e2)), SingleEntryType.eval (SingleBExp (SingleSub, e, e1)), s)
        | SingleSub, Range (e1, e2, s), Single e ->
          Range (SingleEntryType.eval (SingleBExp (SingleSub, e, e1)), SingleEntryType.eval (SingleBExp (SingleSub, e, e2)), s)
        | SingleSub, Range (e11, e12, s1), Range (sub_e21, sub_e22, s2) -> begin
          (* - Range (sub_e21, sub_e22, s2) = Range (- sub_e22, - sub_e21, s2) = Range (e21, e22, s2) *)
          let e21 = SingleEntryType.eval (SingleBExp (SingleMul, SingleConst (-1L), sub_e22)) in
          let e22 = SingleEntryType.eval (SingleBExp (SingleMul, SingleConst (-1L), sub_e21)) in
          (* The below part is the same as adding two ranges. *)
          if (Int64.to_int s1) < 0 || (Int64.to_int s2) < 0 then begin
            Printf.printf "e1 %s\n%s\ne2 %s\n%s\n" 
              (SingleExp.to_string e1) (RangeExp.to_string (Range (e11, e12, s1)))
              (SingleExp.to_string e2) (RangeExp.to_string (Range (e21, e22, s2)));
            single_subtype_error "expecting left aligned now"
          end else
          let new_s = Z.to_int64 (Z.gcd (Z.of_int64 s1) (Z.of_int64 s2)) in
          Range (SingleEntryType.eval (SingleBExp (SingleAdd, e11, e21)), SingleEntryType.eval (SingleBExp (SingleAdd, e12, e22)), new_s)
        end
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
            (* NOTE: We need to guarantee that all ranges we are operating on aligned ranges *)
            Range (SingleEntryType.eval (SingleBExp (SingleMul, SingleConst c, e2)),
            SingleEntryType.eval (SingleBExp (SingleMul, SingleConst c, e1)),
            Int64.mul c s |> Int64.neg)
        (* | SingleMul, Single e, SingleSet e_list
        | SingleMul, SingleSet e_list, Single e ->
          SingleSet (List.map (fun x -> SingleEntryType.eval (SingleBExp (SingleMul, e, x))) e_list) *)
        | SingleSar, Range (e1, e2, s), Single (SingleConst c) ->
          (* Printf.printf "Cal sar %s %s\n" (RangeExp.to_string (Range (e1, e2, s))) (RangeExp.to_string (Single (SingleConst c))); *)
          let s_sar = Int64.shift_right s (Int64.to_int c) in
          if Int64.shift_left s_sar (Int64.to_int c) = s then
            Range (SingleEntryType.eval (SingleBExp (SingleSar, e1, SingleConst c)),
              SingleEntryType.eval (SingleBExp (SingleSar, e2, SingleConst c)),
              s_sar)
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
      | SingleITE (cond, e1, e2) ->
        begin match helper e1, helper e2 with
        | Single e1, Single e2 -> Single (SingleITE (cond, e1, e2))
        | Single e, Range (e1, e2, s) ->
          Range (SingleITE (cond, e, e1), SingleITE (cond, e, e2), s)
        | Range (e1, e2, s), Single e ->
          Range (SingleITE (cond, e1, e), SingleITE (cond, e2, e), s)
        | Range (l1, r1, s1), Range (l2, r2, s2) ->
          if s1 = s2 then
            Range (SingleITE (cond, l1, l2), SingleITE (cond, r1, r2), s1)
          else Top
        | Single e, SingleSet e_list ->
          SingleSet (List.map (fun x -> SingleExp.SingleITE (cond, e, x)) e_list)
        | SingleSet e_list, Single e ->
          SingleSet (List.map (fun x -> SingleExp.SingleITE (cond, x, e)) e_list)
        | _ -> Top
        end
      in
      (* NOTE: We only need canonicalize when generate a new range solution. *)
      (* RangeExp.canonicalize result *)
      result
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
    if is_sol_resolved tv_rel_list input_var_set e then
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
    if is_sol_resolved tv_rel_list input_var_set exp then
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
    | SingleSet _ -> e
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
    (* NOTE: I did not check pc history here, so I may find incorrect correlation!!! *)
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

  let update_block_context_helper
      (tv_rel_list: t)
      (smt_ctx: SmtEmitter.t)
      (branch_block: ArchType.t) : unit =
    update_block_smt_ctx smt_ctx tv_rel_list branch_block.useful_var;
    ArchType.add_assertions smt_ctx branch_block;
    SingleCondType.add_assertions smt_ctx (List.map (fun (x, _) -> x) branch_block.branch_hist)

  let update_br_context_helper
      (smt_ctx: SmtEmitter.t)
      (tv_rel_list: t)
      (block_subtype: ArchType.block_subtype_t list)
      (sup_pc: int) (sub_pc: int) : unit =
    let sub_block = ArchType.get_block_subtype_block block_subtype sup_pc sub_pc in
    update_block_context_helper tv_rel_list smt_ctx sub_block

  let try_solve_one_var
      (smt_ctx: SmtEmitter.t) (* Maybe I need add_no_overflow later *)
      (tv_rel: type_rel)
      (tv_rel_list: t)
      (block_subtype: ArchType.block_subtype_t list)
      (ctx_map_map: ArchContextMap.t)
      (single_ite_eval: SingleIteEval.t)
      (input_var_set: SingleEntryType.SingleVarSet.t) : 
      type_rel =
    (* let target_idx, target_pc = tv_rel.var_idx in *)
    (* let try_solve_top (tv_rel: type_rel) : type_rel =
      match List.find_opt (fun (x, _) -> x = SingleEntryType.SingleTop) tv_rel.subtype_list with
      | Some _ -> { tv_rel with sol = SolSimple Top }
      | None -> tv_rel
    in *)
    let try_solve_single_corr (tv_rel: type_rel) : type_rel =
      (* This is similar to try_solve_extra_slot in RangeSubtype *)
      let get_context_map = ArchContextMap.get_context_map ctx_map_map in
      let get_reverse_map = ArchContextMap.get_reverse_map ctx_map_map in
      let guess_sol_template
          (sub_exp_pc: type_pc_t) : SingleExp.t option =
        let sub_exp, br_pc = sub_exp_pc in
        (* Several requirements to get sol_template:
          1. all block var in sub_exp must be mapped by reverse_map, or sub_exp and target var are in the same block context (i.e., br_pc=target_pc)
          2. sol template should not be top
          3. sol template should not contain the target var (otherwise we are using the var itself to represent its sol)
            (optimization: this will only happen when sol_exp is SingleVar var (as long as my reverse_map is built correctly)) *)
        let filter_sol_exp (sol_exp: SingleExp.t) : SingleExp.t option =
          match sol_exp with
          | SingleTop -> None (* Check requirement 2 *)
          | _ -> (* Check requirement 3: Prevent solution exp contain self, otherwise there would be circular dependency in solution repl *)
            let sol_exp_var_set = SingleExp.get_vars sol_exp in
            if IntSet.mem (fst tv_rel.var_idx) sol_exp_var_set then None
            else Some sol_exp
        in
        if br_pc = snd tv_rel.var_idx then (* sub_exp and target_v are in the same context (not only the same block) *)
          filter_sol_exp sub_exp
        else 
          let reverse_map = get_reverse_map br_pc in
          let known_var_set = SingleExp.get_mapped_var_set reverse_map |> IntSet.union input_var_set in
          if SingleExp.is_val known_var_set sub_exp then
            let sol_exp = SingleExp.repl_var reverse_map sub_exp in
            filter_sol_exp sol_exp
          else None
      in
      let get_direct_subtype_list (tv_rel: type_rel) : type_pc_t list =
        List.filter_map (
          fun (sub: subtype_info_t) ->
            match sub.path_pc_list with
            | [] -> single_subtype_error "try_solve_single_corr: get empty pc list"
            | hd_pc :: [] -> Some (sub.sub_exp, hd_pc)
            | _ -> None
        ) tv_rel.subtype_list
      in
      let rec filter_tv_rel_sat_sol_template
          (checked_var_sol_map: IntIntSingleMap.t) (* this records pre-assumed var sol, similar to tmp context in in SingleBlockInvariance *)
          (sol_template_list: IntSingleMap.t) (* sol id -> sol *)
          (tv_rel: type_rel) : 
          (IntIntSingleMap.t) * (IntSingleMap.t) =
        let sup_var_set = List.map fst tv_rel.supertype_list |> IntSet.of_list in
        let partition_sub_sat_sol_template
            (get_sub_exp: int -> SingleExp.t) 
            (* Two cases: 
                (1) return sub_exp_pc; 
                (2) when sub_exp_pc is a var with asserted solution, 
                return its asserted solution according to the tested sol template's id *)
            (sub_pc: int)
            (acc_sol_template_list: IntSingleMap.t) : 
            (IntSingleMap.t) * (IntSingleMap.t) =
          let test_one_sol_template 
              (idx_sol_template: int * SingleExp.t) : 
              (int * SingleExp.t, (int * SingleExp.t) option) Either.t =
            (* if test success, return Left sol_tempate, otherwise, return Right sub_sol !!! *)
            let sol_idx, sol_template = idx_sol_template in
            let sub_exp = get_sub_exp sol_idx in
            let sub_sol =
              if sub_pc = snd tv_rel.var_idx then
                (* target_v, sol, and sub_exp are in the same context (not only the same block), 
                  no need to switch context (from subtype generated by func call)*)
                sol_template
              else
                let sub_context_map = get_context_map sub_pc in
                SingleExp.repl_var sub_context_map sol_template 
            in
            if sub_exp = SingleTop || sub_sol = SingleTop then 
              Right None
            else
              match SingleCondType.check true smt_ctx [Eq, sub_sol, sub_exp] with
              | SatYes -> Left idx_sol_template
              | SatUnknown -> Right (Some (sol_idx, sub_sol))
              | _ -> Right None
          in
          SmtEmitter.push smt_ctx;
          update_br_context_helper smt_ctx tv_rel_list block_subtype (snd tv_rel.var_idx) sub_pc;
          let sat_list, unknown_opt_list = List.partition_map test_one_sol_template (IntMap.to_list acc_sol_template_list) in
          SmtEmitter.pop smt_ctx 1;
          IntMap.of_list sat_list, List.filter_map (fun x ->  x) unknown_opt_list |> IntMap.of_list
        in
        let filter_sub_sat_sol_template
            (acc: (IntIntSingleMap.t) * (IntSingleMap.t))
            (sub_exp_pc: type_pc_t) : 
            (IntIntSingleMap.t) * (IntSingleMap.t) =
          let acc_checked_var_sol_map, acc_sol_template_list = acc in
          if IntMap.is_empty acc_sol_template_list then acc else
          let sub_exp, sub_pc = sub_exp_pc in
          let get_self = fun _ -> sub_exp in
          match sub_exp_pc with
          | SingleVar v, _ -> 
            if v = fst tv_rel.var_idx || IntSet.mem v sup_var_set then begin 
              (* We only need to break loop for var on the circular dependency chain *)
              match IntMap.find_opt v acc_checked_var_sol_map with
              | None -> (* Never checked v before *)
                (* sat_unknown_list: val is already in sub_exp_pc's block context *)
                let sat_list, sat_unknown_list = partition_sub_sat_sol_template get_self sub_pc acc_sol_template_list in
                if IntMap.is_empty sat_unknown_list then
                  IntMap.add v IntMap.empty acc_checked_var_sol_map, (* add v to checked list, but do not need to add any asserted sol *)
                  sat_list
                else
                  let acc_checked_var_sol_map = IntMap.add v sat_unknown_list acc_checked_var_sol_map in
                  let sub_tv_rel = List.find (fun (tv_rel: type_rel) -> fst tv_rel.var_idx = v) tv_rel_list in
                  let acc_checked_var_sol_map, sat_sub_list = filter_tv_rel_sat_sol_template acc_checked_var_sol_map sat_unknown_list sub_tv_rel in
                  let acc_sol_template_list =
                    IntMap.filter (
                      fun v _ -> IntMap.mem v sat_list || IntMap.mem v sat_sub_list
                    ) acc_sol_template_list
                  in
                  acc_checked_var_sol_map, acc_sol_template_list
              | Some sol_map -> (* Have checked/asserted v's sol before *)
                let get_sol_or_self (sol_idx: int) : SingleExp.t =
                  match IntMap.find_opt sol_idx sol_map with
                  | Some sol -> sol
                  | None -> SingleVar v
                in
                let sat_list, _ = partition_sub_sat_sol_template get_sol_or_self sub_pc acc_sol_template_list in
                acc_checked_var_sol_map, sat_list
            end else
              let sat_list, _ = partition_sub_sat_sol_template get_self sub_pc acc_sol_template_list in
              acc_checked_var_sol_map, sat_list
          | _ ->
              let sat_list, _ = partition_sub_sat_sol_template get_self sub_pc acc_sol_template_list in
              acc_checked_var_sol_map, sat_list
        in
        let todo_subtype_list = get_direct_subtype_list tv_rel in
        (* Printf.printf "@@@var %d\nacc_checked_var_sol_map\n%s\nsol_template_list\n%s\ntodo_subtype_list\n%s\n"
        (fst tv_rel.var_idx)
        (Sexplib.Sexp.to_string_hum (IntIntSingleMap.sexp_of_t checked_var_sol_map))
        (Sexplib.Sexp.to_string_hum (IntSingleMap.sexp_of_t sol_template_list))
        (Sexplib.Sexp.to_string_hum (sexp_of_list sexp_of_type_pc_t todo_subtype_list)); *)
        List.fold_left filter_sub_sat_sol_template (checked_var_sol_map, sol_template_list) todo_subtype_list
      in
      let direct_subtype_list = get_direct_subtype_list tv_rel in
      let possible_sol_template_list = 
        List.filter_map guess_sol_template direct_subtype_list
        |> List.mapi (fun i x -> i, x)
        |> IntMap.of_list
      in
      let checked_var_sol_map = IntMap.singleton (fst tv_rel.var_idx) possible_sol_template_list in
      (* Printf.printf "###var %d\nacc_checked_var_sol_map\n%s\nsol_template_list\n%s\ntodo_subtype_list\n%s\n"
        (fst tv_rel.var_idx)
        (Sexplib.Sexp.to_string_hum (IntIntSingleMap.sexp_of_t checked_var_sol_map))
        (Sexplib.Sexp.to_string_hum (IntSingleMap.sexp_of_t possible_sol_template_list))
        (Sexplib.Sexp.to_string_hum (sexp_of_list sexp_of_type_pc_t direct_subtype_list)); *)
      let _, sat_list = filter_tv_rel_sat_sol_template checked_var_sol_map possible_sol_template_list tv_rel in
      match IntMap.choose_opt sat_list with
      | None -> tv_rel
      | Some (_, hd) ->
        let simp_hd = substitute_one_exp_subtype_list tv_rel_list (hd, snd tv_rel.var_idx) in
        { tv_rel with sol = SolSimple (Single simp_hd) }

      (* let filter_sat_sol_template
          (acc_sol_template_list: type_pc_t list)
          (sub_exp_pc: type_pc_t) : type_pc_t list =
        let sub_exp, sub_pc = sub_exp_pc in
        let test_one_sol_template (br_sol_template: type_pc_t) : bool =
          (* if test success, return true!!! *)
          let sol_template, br_pc = br_sol_template in (* br_pc is used to skip unnecessary tests on the same source. *)
          if sub_pc = br_pc then true else
          let sub_context_map = get_context_map sub_pc in
          let sub_sol = SingleExp.repl_var sub_context_map sol_template in
          if sub_exp = SingleTop || sub_sol = SingleTop then false else
          match SingleCondType.check true smt_ctx [Eq, sub_sol, sub_exp] with
          | SatYes -> true
          | _ -> false
        in
        SmtEmitter.push smt_ctx;
        update_br_context_helper smt_ctx tv_rel_list block_subtype (snd tv_rel.var_idx) sub_pc;
        let result = List.filter test_one_sol_template acc_sol_template_list in
        SmtEmitter.pop smt_ctx 1;
        result
      in
      let direct_subtype_list = get_direct_subtype_list tv_rel in
      let possible_sol_template_list = List.filter_map guess_sol_template direct_subtype_list in
      let possible_sol_template_list = List.fold_left filter_sat_sol_template possible_sol_template_list direct_subtype_list in
      match possible_sol_template_list with
      | [] -> tv_rel
      | (_, hd) :: _ -> { tv_rel with sol = SolSimple (Single hd) } *)

      (* let subtype_list = (* Only keep direct subtype and repl them with inverse context var map *)
        List.filter_map (
          fun (e, pc_list) ->
            match pc_list with
            | [] -> single_subtype_error "try_solve_single_corr: get empty pc list"
            | hd_pc :: [] -> Some (SingleBrInverseMap.repl_br_context_var single_br_inverse_map (e, hd_pc))
            | _ -> None
        ) tv_rel.subtype_list
        |> SingleExpSet.of_list |> SingleExpSet.to_list (* Remove repeated sutypes *)
      in
      let sub_helper (e: SingleExp.t) : SingleExp.t =
        substitute_one_exp_subtype_list tv_rel_list (e, snd (tv_rel.var_idx))
      in
      let sol_exp_opt =
        match subtype_list with
        | [] -> None
        | SingleTop :: [] -> None
        | SingleVar v :: [] -> 
          if v = fst tv_rel.var_idx then None 
          else Some (SingleVar v |> sub_helper)
        | hd_e :: [] -> Some (hd_e |> sub_helper)
        | _ -> None
      in
      match sol_exp_opt with
      | None -> tv_rel
      | Some sol_exp -> { tv_rel with sol = SolSimple (Single sol_exp)} *)
    in
    let try_solve_single_sub_val (tv_rel: type_rel) : type_rel =
      let subtype_list = (* remove non-leaf subtype, i.e., non-input SingleVar before any subsitution *)
        List.filter (fun (sub: subtype_info_t) -> sub.is_leaf) tv_rel.subtype_list
      in
      if List.find_opt (fun (sub: subtype_info_t) -> sub.sub_exp = SingleEntryType.SingleTop) subtype_list <> None then
        tv_rel
      else if List.find_opt (fun (sub: subtype_info_t) -> not (SingleEntryType.is_val input_var_set sub.sub_exp)) subtype_list = None then
        match List.map (fun (sub: subtype_info_t) -> sub.sub_exp, List.rev sub.path_pc_list) subtype_list with
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
            { tv_rel with sol = SolSimple (SingleSet (sol |> SingleExpSet.of_list |> SingleExpSet.to_list)); set_sol = single_pc_list }
            (* Printf.printf "Set sol for SymImm %d pc %d %s\n" target_idx target_pc (Sexplib.Sexp.to_string (sexp_of_list sexp_of_type_pc_list_t single_pc_list));
            find_other_sol_set_correlation target_idx target_pc single_pc_list tv_rel_list input_var_set *)
            (* SolSimple (SingleSet single_list) *)
          end
      else begin 
        (* To handle the case where exp contains resolved block vars *)
        let sub_range_opt_list = List.map (fun (sub: subtype_info_t) -> sub_sol_single_to_range_opt (fun x -> x) tv_rel_list input_var_set (sub.sub_exp, List.hd sub.path_pc_list)) subtype_list in
        let sub_range_list = List.filter_map (fun x -> x) sub_range_opt_list in
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
          (* NOTE: This may introduce non-input var to SingleSet sol!!! *)
          | single_list, [] -> { tv_rel with sol = SolSimple (SingleSet (single_list |> SingleExpSet.of_list |> SingleExpSet.to_list)) }
            (* TODO: I cannot resolve correlation between vars with this solution format!!! *)
            (* Printf.printf "!!! single list %s\n" (Sexplib.Sexp.to_string (sexp_of_list SingleEntryType.sexp_of_t single_list)); *)
            (* begin match List.sort_uniq SingleExp.cmp single_list with
            | [] -> single_subtype_error "sub range list is empty"
            | hd :: [] -> SolSimple (Single hd)
            | _ -> SolSimple (SingleSet single_list)
            end *)
          | s :: [], (l, r, step) :: [] ->
            if step <= 0L then single_subtype_error "step should be > 0";
            if SingleEntryType.cmp s (SingleEntryType.eval (SingleBExp (SingleAdd, r, SingleConst step))) = 0 then
              { tv_rel with sol = SolSimple (Range (l, s, step)) (* |> RangeExp.canonicalize) *) }
            else if SingleEntryType.cmp s (SingleEntryType.eval (SingleBExp (SingleSub, l, SingleConst step))) = 0 then
              { tv_rel with sol = SolSimple (Range (s, r, step)) (* |> RangeExp.canonicalize) *) }
            else tv_rel
          | _ -> tv_rel (* TODO: Maybe need to improve this!!! *)
        end
      end
    in
    let find_base_step 
        (supertype_list: var_pc_list_t list)
        (subtype_list: subtype_info_t list) (v_idx_pc: var_pc_t) : 
        loop_info_t option =
      (* <TODO> Later we need to think about whether this pattern matching is strong enough to resolve for nested loops. *)
      (* let find_base = 
        List.find_opt (
          fun (x, _) -> SingleEntryType.is_val input_var_set x && x <> SingleTop
        ) subtype_list
      in *)
      let subtype_list = List.map (fun (sub: subtype_info_t) -> sub.sub_exp, sub.path_pc_list) subtype_list in
      let v_idx, v_pc = v_idx_pc in
      let find_step_helper (is_header_direct: bool) (sub_list: type_pc_list_t list) =
        let get_candidate_helper (x_pc: type_pc_list_t) =
          let orig_x, pc_list = x_pc in
          let x = SingleIteEval.eval single_ite_eval (orig_x, List.hd pc_list) in
          (* if SingleExp.cmp orig_x x <> 0 then
            Printf.printf "SingleIteEval simplify (%d) %s to %s\n" (List.hd pc_list) (SingleExp.to_string orig_x) (SingleExp.to_string x); *)
          match x with
          | SingleBExp (SingleAdd, SingleVar v, SingleConst s)
          | SingleBExp (SingleAdd, SingleConst s, SingleVar v) ->
            if v = v_idx then
              if s = 0L then single_subtype_error "try_solve_loop_cond: step should be non-zero"
              else Some ((x, pc_list), v, s)
            else if List.find_opt (fun (sup_idx, _) -> v = sup_idx) supertype_list <> None then
              if s = 0L then single_subtype_error "try_solve_loop_cond: step should be non-zero"
              else Some ((x, pc_list), v, s)
            else None
          | _ -> None
        in
        if is_header_direct then
          (* Heuristic: when there are two x+1 directly jump back to the header, one is loop resume, the other is loop out,
              it's likely that the one with smaller pc is the loop in and the other is loop out (for some outer loop).
              In this case, loop out one is actually equal to the loop out value, i.e., x+n where n is #repeats of the inner loop. *)
          (List.filter_map get_candidate_helper sub_list 
          |> List.sort (
                fun ((_, pc_list1), _, _) ((_, pc_list2), _, _) ->
                  Int.compare (List.hd pc_list1) (List.hd pc_list2)
              ) 
          |> List.nth_opt) 0
        else
          let direct_br_pc_step_map =
            List.filter_map (
              fun (tv_rel: type_rel) ->
                Option.map (
                  fun (loop_info: loop_info_t) -> List.hd loop_info.step_pc_list, loop_info.step_exp
                ) tv_rel.loop_info
            ) tv_rel_list |> IntMap.of_list
          in
          let find_candidate_helper ((x, pc_list), v, s) =
            let other_step_exp = IntMap.find_opt (List.hd pc_list) direct_br_pc_step_map in
            Option.bind other_step_exp (
              fun other_step_exp -> 
                if SingleExp.cmp other_step_exp x = 0 then Some ((x, pc_list), v, s)
                else None
            )
          in
          let get_indirect_candidate_helper (x_pc: type_pc_list_t) =
            Option.bind (get_candidate_helper x_pc) find_candidate_helper
          in
          List.find_map get_indirect_candidate_helper sub_list
      in
      let unify_header_base_block_var
        (base: SingleExp.t) (target_pc: int) (loop_step_br_pc: int) : SingleExp.t =
        if base = SingleTop then base else
        let base_var_set = SingleExp.get_vars base in
        let base_block_var_set = IntSet.diff base_var_set input_var_set in
        if IntSet.is_empty base_block_var_set then base else
        let target_var_set =
          List.filter_map (
            fun (tv_rel: type_rel) ->
              let v_idx, v_pc = tv_rel.var_idx in
              if v_pc = target_pc then
                List.find_map (
                  fun (sup_idx, sup_pc_list) ->
                    if sup_idx = v_idx && list_tl sup_pc_list = loop_step_br_pc then
                      Some v_idx
                    else None
                ) tv_rel.supertype_list
              else None
          ) tv_rel_list |> IntSet.of_list
        in
        let remain_base_block_var_set, base_context_map =
          List.fold_left (
            fun (acc: IntSet.t * SingleExp.local_var_map_t) (tv_rel: type_rel) ->
              let acc_remain_var, acc_map = acc in
              let v_idx, _ = tv_rel.var_idx in
              if IntSet.mem v_idx acc_remain_var then
                match List.find_opt (fun (sup, _) -> IntSet.mem sup target_var_set) tv_rel.supertype_list with
                | Some (sup, _) -> 
                  IntSet.remove v_idx acc_remain_var,
                  (v_idx, SingleVar sup) :: acc_map
                | None -> acc
              else acc
          ) (base_block_var_set, []) tv_rel_list
        in
        if IntSet.is_empty remain_base_block_var_set then
          (* Since we already ensure all block vars are mapped, we can use repl_var to keep input var (not in the context map). *)
          SingleExp.repl_var base_context_map base
        else SingleTop (* Important: if we cannot unify block, then we should return SingleTop and not use the non-unified base!!! *)
      in
      let find_header_base_helper (direct_subtype_list: type_pc_list_t list) (step_br_pc: int) : SingleExp.t =
        (* Find base for counter at loop header. *)
        (* NOTE: So far we only support loops with single entrance *)
        let base_candidate_list = 
          List.filter (
            fun (_, pc_list) -> List.hd pc_list <> step_br_pc
          ) direct_subtype_list
        in
        match base_candidate_list with
        | [] -> SingleTop
        | (base, [ _ ]) :: [] -> 
          unify_header_base_block_var base v_pc step_br_pc
        | _ -> SingleTop
      in
      let unify_other_base_block_var
          (base: SingleExp.t) (target_pc: int) : SingleExp.t =
        if base = SingleTop then base else
        let base_var_set = SingleExp.get_vars base in
        let base_block_var_set = IntSet.diff base_var_set input_var_set in
        if IntSet.is_empty base_block_var_set then base else
        let target_var_set =
          List.filter_map (
            fun (tv_rel: type_rel) -> 
              let v_idx, v_pc = tv_rel.var_idx in
              if v_pc = target_pc then
                List.find_map (
                fun (sup_idx, _) ->
                  if IntSet.mem sup_idx base_block_var_set then 
                    Some v_idx
                  else None
              ) tv_rel.supertype_list
              else None
            ) tv_rel_list |> IntSet.of_list
        in
        let remain_base_block_var_set, base_context_map =
          List.fold_left (
            fun (acc: IntSet.t * SingleExp.local_var_map_t) (tv_rel: type_rel) ->
              let acc_remain_var, acc_map = acc in
              let v_idx, _ = tv_rel.var_idx in
              if IntSet.mem v_idx acc_remain_var then
                match List.find_opt (fun (sup, _) -> IntSet.mem sup target_var_set) tv_rel.supertype_list with
                | Some (sup, _) -> 
                  IntSet.remove v_idx acc_remain_var,
                  (v_idx, SingleVar sup) :: acc_map
                | None -> acc
              else acc
          ) (base_block_var_set, []) tv_rel_list
        in
        if IntSet.is_empty remain_base_block_var_set then
          (* Since we already ensure all block vars are mapped, we can use repl_var to keep input var (not in the context map). *)
          SingleExp.repl_var base_context_map base
        else SingleTop (* Important: if we cannot unify block, then we should return SingleTop and not use the non-unified base!!! *)
      in
      let find_other_base_helper (step_br_pc: int) : SingleExp.t =
        (* Find base for counter not at loop header *)
        List.find_map (
          fun (tv_rel: type_rel) ->
            match tv_rel.loop_info with
            | Some loop_info ->
            (* | Some (base, other_step_br_pc) -> *)
              (* v_idx must share the same step_br_pc with other and be other's supertype *)
              let other_is_same_counter =
                step_br_pc = List.hd loop_info.step_pc_list &&
                List.find_opt (fun (x, _) -> x = v_idx) tv_rel.supertype_list <> None
              in
              if other_is_same_counter then
                let unified_base = unify_other_base_block_var loop_info.base v_pc in
                if unified_base <> SingleTop then Some unified_base else None
              else None
            | _ -> None
        ) tv_rel_list |> Option.value ~default:SingleExp.SingleTop
      in
      let direct_subtype_list, other_subtype_list =
        List.partition (
          fun (_, pc_list) ->
            match pc_list with
            | [] -> single_subtype_error "get empty pc list"
            | _ :: [] -> true
            | _  -> false
        ) subtype_list
      in
      match find_step_helper true direct_subtype_list with
      | Some ((var_step, step_br_pc), var_idx, step) ->
        Some {
          base = find_header_base_helper direct_subtype_list (List.hd step_br_pc);
          step_exp = var_step;
          step_pc_list = step_br_pc;
          step_var_idx = var_idx;
          step_val = step;
        }
      | None ->
        begin match find_step_helper false other_subtype_list with
        | Some ((var_step, step_br_pc), var_idx, step) ->
          Some {
            base = find_other_base_helper (List.hd step_br_pc);
            step_exp = var_step;
            step_pc_list = step_br_pc;
            step_var_idx = var_idx;
            step_val = step;
          }
        | None -> None
        end
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
      let add_const (e: SingleExp.t) (v: int64) =
        SingleExp.eval (SingleBExp (SingleAdd, e, SingleConst v))
      in
      let canonicalize_end_val (* e1, step, e2 *)
          (begin_val: SingleExp.t) (step: int64) (end_val: SingleExp.t) : SingleExp.t =
        if step > 0L then (* e1 < e2 *)
          (* return e2' <= e2 such that (e2' - e1) % s = 0 *)
          SingleBExp (SingleSub, end_val, SingleBExp (SingleMod, SingleBExp (SingleSub, end_val, begin_val), SingleConst step))
        else (* l > r *)
          (* return e2' >= e2 such that (e1 - e2') % s = 0 *)
          SingleBExp (SingleAdd, end_val, SingleBExp (SingleMod, SingleBExp (SingleSub, begin_val, end_val), SingleConst (Int64.neg step)))
        |> SingleExp.eval
      in
      let end_val_resume = (* boundary value of the counter (in the beginning of the loop) when the loop will resume after finish the current one *)
        match cond with
        | Ne -> add_const end_val (Int64.neg step)
        | Lt | Bt ->
          if len_div_step then add_const end_val (Int64.neg step) (* equal to value on the else branch when len_div_step *)
          else add_const end_val (if inc then -1L else 1L) |> canonicalize_end_val begin_val step
        | Le | Be ->
          if len_div_step then end_val (* equal to value on the else branch when len_div_step *)
          else end_val |> canonicalize_end_val begin_val step
        | Eq -> single_subtype_error "this should not be reachable"
      in
      let end_val_in = add_const end_val_resume step in
      let get_range_helper l r (s: int64) =
        if s > 0L then RangeExp.Range (l, r, s)
        else if s < 0L then RangeExp.Range (r, l, Int64.neg s)
        else single_subtype_error "get_range_helper step = 0L"
      in
      Some (
        get_range_helper begin_val end_val_in step,
        get_range_helper begin_val end_val_resume step,
        Single end_val_in
      )
      (* TODO: Below is the old implementation. Check!!! *)
      (* let in_loop_bound_off, resume_loop_bound_off =
        match cond with
        | Ne -> 0L, Int64.neg step
        | Lt | Bt ->
          if len_div_step then 0L, Int64.neg step
          else if inc then Int64.sub step 1L, -1L
          else Int64.add step 1L, 1L
        | Le | Be -> step, 0L
        | Eq -> single_subtype_error "this should not be reachable"
      in
      let get_range_helper (begin_val: SingleEntryType.t) (end_val: SingleEntryType.t) (step: int64) : RangeExp.t =
        if step > 0L then
          Range (begin_val, end_val, step) |> RangeExp.canonicalize
        else if step < 0L then
          Range (end_val, begin_val, step) |> RangeExp.canonicalize
        else
          single_subtype_error "get_range_helper step = 0L"
      in
      let get_range_l_r (range: RangeExp.t) : SingleEntryType.t * SingleEntryType.t =
        match range with
        | Range (l, r, _) -> l, r
        | _ -> single_subtype_error "get_range_l_r is not range"
      in
      let end_val_in = SingleEntryType.eval (SingleBExp (SingleAdd, end_val, SingleConst in_loop_bound_off)) in
      let end_val_resume = SingleEntryType.eval (SingleBExp (SingleAdd, end_val, SingleConst resume_loop_bound_off)) in
      let range_in = get_range_helper begin_val end_val_in step in (* range_in is canonicalized *)
      let range_resume = get_range_helper begin_val end_val_resume step in (* range_resume is canonicalized *)
      let range_out : RangeExp.t =
        let in_l, in_r = get_range_l_r range_in in
        if step > 0L then Single in_r else Single in_l
      in
      (* let new_len_val = SingleEntryType.eval (SingleBExp (SingleSub, end_val_in, begin_val)) in
      let new_len_div_step = SingleEntryType.is_div new_len_val step in
      let range_out : RangeExp.t = 
        if len_div_step || new_len_div_step then Single end_val_in
        else get_range_helper (SingleExp.eval (SingleBExp (SingleAdd, end_val_resume, SingleConst 1L))) end_val_in (if step > 0L then 1L else -1L)
      in *)
      Some (range_in, range_resume, range_out) *)
    in
    let gen_self_loop_sol
        (begin_val: SingleEntryType.t) 
        (bound_val: SingleEntryType.t) (* the bound exp found in the cond *)
        (step: int64) (* signed, which could be positive or negative *)
        (step_before_cmp: bool) 
        (cmp_pc: int) (* or cond_pc: note this may not be the branch pc*)
        (resume_on_taken: bool)
        (cmp_operand: ArchType.CondType.cond) (cmp_var_on_left: bool)
        (var_cmp_in_same_block: bool) : SingleSol.t =
      let end_val = (* end_val of the counter reg in the beginning of the block! *)
        if step_before_cmp then 
          (* Pattern: counter += step; cmp counter bound; conditional jmp *)
          SingleEntryType.eval (SingleBExp (SingleSub, bound_val, SingleConst step))
        else 
          (* Pattern: cmp counter bound; conditional jmp *)
          bound_val
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
    in
    let gen_other_loop_sol
        (self_info: int * SingleExp.t * int64)
        (other_info: int * SingleExp.t * int64)
        (other_sol: SingleSol.t) :
        SingleSol.t * SingleContext.t =
      let self_idx, self_base, self_sign_step = self_info in
      let other_idx, other_base, other_sign_step = other_info in
      (* Printf.printf "%s %Ld %s %Ld\n" (SingleExp.to_string self_base) self_sign_step (SingleExp.to_string other_base) other_sign_step; *)
      let get_shift (step: int64) : int64 option =
        match Int64.abs step with
        | 1L -> Some 0L | 2L -> Some 1L | 4L -> Some 2L | 8L -> Some 3L | 16L -> Some 4L 
        | 32L -> Some 5L | 64L -> Some 6L | 128L -> Some 7L | 256L -> Some 8L
        | _ -> None
      in
      let get_sol_helper (sub_base: SingleExp.t) (mult_step: int64) (shift_right: int64) (add_base: SingleExp.t) (range: RangeExp.t) : RangeExp.t =
        let boundary_converter (e: SingleExp.t) : SingleExp.t =
          SingleExp.eval (
            SingleBExp (
              SingleAdd, 
              (SingleBExp (SingleSar, (SingleBExp (SingleMul, (SingleBExp (SingleSub, e, sub_base)), SingleConst mult_step)), SingleConst shift_right)),
              add_base
            )
          )
        in
        (* Note: The following logic and boundary_converter_with_fix are deprecated since 
          (1) it cannot handle the case when bound - 1 < 0; 
          (2) we get rid of the case by make range canonical and avoid loop out sol to be a range. 
          TODO: still need to check whether the case fixed by the following logic is also fixed in current version.
        *)
        (* Why get_sol_helper is so complex: 
          Other solution may have the following formula:
          [0, 4a + 2], [0, 4a - 2], [(4a - 2) + 1, 4a + 2], step = 4
          We use +1 in ((4a - 2) + 1) to make the bounary accurate (applied when not aligned)
          To get a sol of a var with step = 1, it is expected to be 
          [0, a], [0, a - 1], a, step = 1.
          But if we directly divide ((4a - 2) + 1) by 4, it is a - 1.
          We need to do calculate as follows:
          (((4a-2)+1) - 1) / 4 + 1.
        *)
        (* let boundary_converter_with_fix (e: SingleExp.t) (boundary_adjust: int64) : SingleExp.t =
          let e = SingleExp.SingleBExp (SingleSub, e, SingleConst boundary_adjust) in
          let e = boundary_converter e in
          SingleExp.eval (SingleBExp (SingleAdd, e, SingleConst boundary_adjust))
        in *)
        match range with
        | Single other_x -> Single (boundary_converter other_x) (* Single (boundary_converter_with_fix other_x 0L) *)
        | Range (l, r, step) -> 
          let new_l = boundary_converter l in
          let new_r = boundary_converter r in
          (* let new_l = boundary_converter_with_fix l (if step > 0L then 1L else 0L) in
          let new_r = boundary_converter_with_fix r (if step < 0L then -1L else 0L) in *)
          if SingleExp.cmp new_l new_r = 0 then Single new_l
          else Range (new_l, new_r, Int64.shift_right (Int64.mul step mult_step) (Int64.to_int shift_right))
        | _ -> single_subtype_error (Printf.sprintf "Invalid loop sol %s" (SingleSol.to_string other_sol))
        (* |> RangeExp.canonicalize *)
      in
      let shift_opt = get_shift other_sign_step in
      let mult_step = if other_sign_step > 0L then self_sign_step else Int64.neg self_sign_step in
      let sol : SingleSol.t =
        (* We only derive detailed solution formula when other_sign_step is power of 2, since we do not have div and can only use Sar as a replacement. *)
        match shift_opt with
        | None -> SolSimple (Single (SingleVar self_idx))
        | Some shift_right ->
          let helper = get_sol_helper other_base mult_step shift_right self_base in 
          begin match other_sol with
          | SolSimple range -> SolSimple (helper range)
          | SolCond (cond_pc, range1, range2, range3) ->
            SolCond (cond_pc, helper range1, helper range2, helper range3)
          | _ -> SolSimple (Single (SingleVar self_idx))
          end
      in
      let extra_cond : SingleContext.t = Cond (
        Eq,
        SingleExp.eval (SingleBExp (SingleMul, (SingleBExp (SingleSub, SingleVar self_idx, self_base)), SingleConst other_sign_step)),
        SingleExp.eval (SingleBExp (SingleMul, (SingleBExp (SingleSub, SingleVar other_idx, other_base)), SingleConst self_sign_step))
      )
      in
      (* Printf.printf "!!! extra_cond \n%s\n" (Sexplib.Sexp.to_string_hum (SingleContext.sexp_of_t extra_cond));
      Printf.printf "%s\n" (SingleExp.to_string (SingleBExp (SingleSub, SingleVar other_idx, other_base)));
      Printf.printf "%s\n" (SingleExp.to_string (SingleExp.eval (SingleBExp (SingleSub, SingleVar other_idx, other_base))));
      Printf.printf "%s\n" (SingleExp.to_string (SingleBExp (SingleMul, (SingleBExp (SingleSub, SingleVar other_idx, other_base)), SingleConst self_sign_step))); *)
      sol, extra_cond
    in
    let try_solve_loop_cond (tv_rel: type_rel) : type_rel =
      let target_idx, target_pc = tv_rel.var_idx in
      match find_base_step tv_rel.supertype_list tv_rel.subtype_list tv_rel.var_idx with
      | Some loop_info ->
        if loop_info.step_val = 0L then single_subtype_error "try_solve_loop_cond: find step 0";
      (* | Some (_, (_, _, 0L)) -> single_subtype_error "try_solve_loop_cond: find step 0" 
      | Some (base_opt, ((var_step, branch_pc), var_idx, step)) -> *)
        (* let var_step = loop_info.step_exp in
        let branch_pc = loop_info.step_pc_list in
        let var_idx = loop_info.step_var_idx in
        let step = loop_info.step_val in *)
        begin match ArchType.get_branch_cond block_subtype (List.hd loop_info.step_pc_list) with
        | None -> 
          (* Note uncond jump now can use the last not taken cond! *)
          (* TODO: Maybe we should make loop inference use more conditions. *)
          Printf.printf "Warning: try_solve_loop_cond found a uncond jump\n";
          tv_rel
        | Some ((cond, l, r), cond_pc) ->
          (* if resume_loop_on_taken, then (cond, l, r) is the cond_pc's taken cond, else, it is not taken cond *)
          (* in summary, (cond, l, r) is always the condition to resume the loop! *)
          (* NOTE: We have to simplify cond (found from block_subtype) as how we have simplified base_step (found from subtype_list). *)
          (* NOTE: Here we want to match base_step (the subtype used to update the counter) with one side of the branch condition.
              base_step is simplied with pc=List.hd branch_pc, since we only care about its effective expression on the taken side 
              (inspired by chacha20, where base_step=ctr-min(ctr, 64), and the step is 64 when taken, so base_step=ctr-64).
              Hence, cond is also simplified with pc=List.hd branch_pc. *)
          let eval_cond_pc = List.hd loop_info.step_pc_list in
          let new_l = SingleIteEval.eval single_ite_eval (substitute_one_exp_subtype_list tv_rel_list (l, eval_cond_pc), eval_cond_pc) in
          let new_r = SingleIteEval.eval single_ite_eval (substitute_one_exp_subtype_list tv_rel_list (r, eval_cond_pc), eval_cond_pc) in
          (* NOTE: We return original l or r as the bound, since we need to simplify bound in a different way later 
              (with different pc and sub_sol function sub_sol_single_to_range_opt) *)
          let find_cond_var_step = (* on_left, bound, add step before cmp *)
            if SingleEntryType.cmp new_l loop_info.step_exp = 0 then Some (true, new_r, true)
            else if SingleEntryType.cmp new_r loop_info.step_exp = 0 then Some (false, new_l, true)
            else if SingleEntryType.cmp new_l (SingleVar loop_info.step_var_idx) = 0 then Some (true, new_r, false)
            else if SingleEntryType.cmp new_r (SingleVar loop_info.step_var_idx) = 0 then Some (false, new_l, false)
            else None
          in
          Printf.printf "Find var %d var step %s cond %s\n" target_idx (SingleExp.to_string loop_info.step_exp) (ArchType.CondType.to_string (cond, new_l, new_r));
          match find_cond_var_step with
          | Some (on_left, bound, step_before_cmp) -> 
            (* let inc = (step > 0L) in *)
            let resume_loop_on_taken = cond_pc = List.hd loop_info.step_pc_list in
            let target_idx_branch_pc_same_block = target_idx = loop_info.step_var_idx in
            (* TODO: Need to repl bound here!!! *)
            (* let bound = (
              (* Very imporant: here bound comes from the branch condition, which should be evaluated in the context of branch_pc - 1!!! *)
              begin match sub_sol_single_to_range_opt (fun x -> x) tv_rel_list input_var_set (bound, (List.hd branch_pc) - 1) with
              | Some (Single e) -> e
              | Some _ -> bound
              | None -> SingleTop
              end
            ) in *)
            (* Goal: Support bound represented by block var. Need to sub block var in the current bound with block var from the counter var's block.
                1. Identify all block var
                2. Identify the counter's var block pc
                3. Build a map, from counter block var to bound block var, where the mapping relation is found by checking supertype. *)
            let unify_bound_block_var
                (bound: SingleExp.t) (target_pc: int) (same_block: bool) : SingleExp.t =
              (* if not (is_sol_resolved tv_rel_list input_var_set bound) then SingleTop else *)
              if same_block || bound = SingleTop then bound else
              let bound_var_set = SingleExp.get_vars bound in
              let bound_block_var_set = IntSet.diff bound_var_set input_var_set in
              if IntSet.is_empty bound_block_var_set then bound else
              let remain_bound_block_var_set, bound_context_map =
                List.fold_left (
                  fun (acc: IntSet.t * SingleExp.local_var_map_t) (tv_rel: type_rel) ->
                    let v_idx, v_pc = tv_rel.var_idx in
                    if v_pc = target_pc then
                      let acc_remain_var, acc_map = acc in
                      let v_super_idx_set = List.map (fun (x, _) -> x) tv_rel.supertype_list |> IntSet.of_list in
                      let new_mapped_var = IntSet.inter acc_remain_var v_super_idx_set in (* orzzz this must be done before acc_remain_var is changed*)
                      let acc_remain_var = IntSet.diff acc_remain_var v_super_idx_set in
                      let acc_map = List.map (fun x -> (x, SingleExp.SingleVar v_idx)) (IntSet.to_list new_mapped_var) @ acc_map in
                      acc_remain_var, acc_map
                    else acc
                ) (bound_block_var_set, []) tv_rel_list
              in
              if IntSet.is_empty remain_bound_block_var_set then
                (* Since we already ensure all block vars are mapped, we can use repl_var to keep input var (not in the context map). *)
                SingleExp.repl_var bound_context_map bound
              else SingleTop (* Important: if we cannot unify block, then we should return SingleTop and not use the non-unified bound!!! *)
            in
            let bound = unify_bound_block_var bound target_pc target_idx_branch_pc_same_block in
            if bound <> SingleTop (* && SingleEntryType.is_val input_var_set bound *) then begin
              Printf.printf "@@@ Var %d Bound is val %s same_block=%b\n" target_idx (SingleExp.to_string bound) target_idx_branch_pc_same_block;
              let sol = 
                gen_self_loop_sol loop_info.base bound loop_info.step_val step_before_cmp 
                  cond_pc resume_loop_on_taken cond on_left target_idx_branch_pc_same_block 
              in
              { tv_rel with sol = sol; loop_info = Some loop_info }
            end else begin
              Printf.printf "Bound is not val %s bound %s\n" (ArchType.CondType.to_string (cond, l, r)) (SingleExp.to_string bound);
              tv_rel
            end
          | None ->
            (* Represent sol with the actual loop counter *)
            if loop_info.base = SingleTop then tv_rel else
            let find_other : (SingleSol.t * (SingleContext.t list)) option =
              List.find_map (
                fun (tv: type_rel) ->
                  let v_idx, v_pc = tv.var_idx in
                  if v_pc <> target_pc then None
                  else begin
                    (* TODO: can get rid of this find_base_step to use the info we already have! *)
                    match tv.loop_info with
                    | Some other_loop_info ->
                      if other_loop_info.step_val = 0L then None else
                    (* match find_base_step tv.supertype_list tv.subtype_list v_idx with
                    | Some (_, _, _, 0L) -> None
                    | Some (_, (other_step, other_branch_pc), other_cmp_var_idx, v_step) -> *)
                      (* let other_step = other_loop_info.step_exp in
                      let other_branch_pc = other_loop_info.step_pc_list in
                      let other_cmp_var_idx = other_loop_info.step_var_idx in
                      let v_step = other_loop_info.step_val in *)
                      (* NOTE: Must ensure the other_var is indeed in the cond!!! *)
                      (* IMPORTANT: We need to use v_step from find_base_step instead of the solution*)
                      if SingleEntryType.cmp new_l other_loop_info.step_exp = 0 || SingleEntryType.cmp new_r other_loop_info.step_exp = 0
                        || SingleEntryType.cmp new_l (SingleVar other_loop_info.step_var_idx) = 0
                        || SingleEntryType.cmp new_r (SingleVar other_loop_info.step_var_idx) = 0 then begin
                        match tv.sol with
                        | SolCond (_, Range (v_l, v_r, _), _, _)
                        | SolSimple (Range (v_l, v_r, _)) ->
                          if List.hd other_loop_info.step_pc_list = List.hd loop_info.step_pc_list then (* We check branch pc, which might not be the pc of the cond branch that do the cmp *)
                            let v_base = if other_loop_info.step_val > 0L then v_l else v_r in
                            if Int64.rem loop_info.step_val other_loop_info.step_val = 0L then
                              Some (SingleSol.SolSimple (Single (SingleEntryType.eval (
                                SingleBExp (
                                  SingleAdd,
                                  SingleBExp (
                                    SingleMul, 
                                    SingleBExp (SingleSub, SingleVar v_idx, v_base),
                                    SingleConst (Int64.div loop_info.step_val other_loop_info.step_val)
                                  ),
                                  loop_info.base
                                )
                              ))), [])
                            else 
                              let sol, other_cons = 
                                gen_other_loop_sol (target_idx, loop_info.base, loop_info.step_val) (v_idx, v_base, other_loop_info.step_val) tv.sol 
                              in
                              Some (sol, [other_cons])
                          else None
                        | _ -> None
                      end else None
                    | _ -> None
                  end
              ) tv_rel_list
            in
            begin match find_other with
            | Some (sol, other_constraint) -> { tv_rel with sol = sol; other_constraint = other_constraint }
            | None -> 
              Printf.printf "Warning: cannot find sol for var %d with find loop cond\n" target_idx;
              tv_rel
            end
          end
      | None -> tv_rel
    in
    let solve_rules = [
      (* try_solve_top; *)
      try_solve_single_corr;
      try_solve_loop_cond;
      try_solve_single_sub_val;
    ] in
    let tv_rel_opt = 
      if List.is_empty tv_rel.subtype_list then None else
      List.find_map (
        fun rule ->
          let tv_rel = rule tv_rel in
          if SingleSol.has_top tv_rel.sol then None
          else Some tv_rel
      ) solve_rules
    in
    match tv_rel_opt with
    | Some tv_rel -> tv_rel
    | None -> tv_rel

  let get_single_ite_eval
      (smt_ctx: SmtEmitter.t)
      (tv_rel_list: t)
      (block_subtype: ArchType.block_subtype_t list) : SingleIteEval.t =
    let single_subtype_list =
      List.map (
        fun (x: type_rel) ->
          (snd x.var_idx),
          List.filter_map (
            fun (sub: subtype_info_t) ->
              match sub.path_pc_list with
              | hd :: [] -> Some (sub.sub_exp, hd)
              | _ -> None
          ) x.subtype_list
      ) tv_rel_list
    in
    let single_ite_eval = SingleIteEval.init smt_ctx (update_block_context_helper tv_rel_list) single_subtype_list block_subtype in
    single_ite_eval
    (* Printf.printf "@@@ SingleIteEval Map \n%s\n" (Sexplib.Sexp.to_string_hum (SingleIteEval.sexp_of_t single_ite_eval)); *)

  let try_solve_vars
      (smt_ctx: SmtEmitter.t)
      (tv_rel_list: t)
      (block_subtype: ArchType.block_subtype_t list)
      (ctx_map_map: ArchContextMap.t)
      (single_ite_eval: SingleIteEval.t)
      (input_var_set: SingleEntryType.SingleVarSet.t) : t * bool =
    let new_sol_list, new_subtype = List.fold_left_map (
      fun acc tv_rel ->
        if not (SingleSol.has_top tv_rel.sol) then acc, tv_rel
        else
          (* NOTE: I eventually decide to not merge since this may get rid of some useful branch information by removing subtype with some pc history,
              which may prevent us from find correct loop solution. *)
          (* Merge same subtype exp, keep the smaller pc which means fewer constraints from cond branch *)
          (* let rec merge (last_entry: type_pc_list_t option) (sub_list: type_pc_list_t list) : type_pc_list_t list =
            match last_entry, sub_list with
            | None, [] -> []
            | Some last, [] -> [ last ]
            | None, hd :: tl -> merge (Some hd) tl
            | Some (last_exp, last_pc), (hd_exp, hd_pc) :: tl ->
              if SingleEntryType.cmp hd_exp last_exp = 0 && list_tl last_pc = list_tl hd_pc then 
                (* I only merge when tl pc is the same, so that I do not merge sub types come from different direct src. *)
                (* When sub exp are the same, there are two cases 
                    (1) with block var, they must be in the same block, then compare pc and keep the one with smaller pc makes potential subsitution do not miss possible range
                    (2) without block var, they must only have input var, so it does not affect subsitution. 
                      Although they may in different blocks and have different context, for context check we do not rely on hd pc either, so it's fine.
                      hd pc is used to subsitute solution. *)
                (* NOTE: in current implementation, I only keep the first and last pc, so the length can only be one or two.*)
                (* Keep one of last and hd which has smaller pc list (more direct subtype) *)
                if List.length last_pc < List.length hd_pc then merge last_entry tl
                else merge (Some (hd_exp, hd_pc)) tl
              else (* Keep both last and hd since they are different, and use hd to merge with following *)
                (last_exp, last_pc) :: (merge (Some (hd_exp, hd_pc)) tl)
          in
          let subtype_list =
            merge None (List.sort (
              fun (e1, pc1) (e2, pc2) ->
                let cmp_e = SingleEntryType.cmp e1 e2 in
                if cmp_e = 0 then compare (List.rev pc1) (List.rev pc2) 
                (* if we compare tl, then we need to sort rev pc to make sure exp_pc with same exp and tl pc are next to each other *)
                else cmp_e
            ) tv_rel.subtype_list)
          in
          let tv_rel = { tv_rel with subtype_list = subtype_list } in *)
          let tv_rel = try_solve_one_var smt_ctx tv_rel tv_rel_list block_subtype ctx_map_map single_ite_eval input_var_set in
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
      (smt_ctx: SmtEmitter.t)
      (tv_rel_list: t)
      (block_subtype: ArchType.block_subtype_t list)
      (input_var_set: SingleEntryType.SingleVarSet.t)
      (num_iter: int) : t =
    Printf.printf "Before ArchContextMap.init\n%!";
    let ctx_map_map = ArchContextMap.init input_var_set block_subtype in
    (* Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum (ArchContextMap.sexp_of_t ctx_map_map)); *)
    Printf.printf "After ArchContextMap.init\n%!";
    let rec helper
        (tv_rel_list: t)
        (num_iter: int) : t =
      Printf.printf "solve_vars iter %d\n" num_iter;
      if num_iter = 0 then update_sol_set_correlation tv_rel_list input_var_set
      else begin
        let single_ite_eval = get_single_ite_eval smt_ctx tv_rel_list block_subtype in
        let new_subtype, found_new_sol = try_solve_vars smt_ctx tv_rel_list block_subtype ctx_map_map single_ite_eval input_var_set in
        if found_new_sol then helper new_subtype (num_iter - 1)
        else begin
          let resolved_vars = 
            List.filter_map (
              fun (tv_rel: type_rel) -> 
                if is_sol_resolved new_subtype input_var_set (SingleVar (fst tv_rel.var_idx)) then
                  Some (fst tv_rel.var_idx)
                else None
            ) new_subtype 
          in
          Printf.printf "SingleSubtype resolved vars:\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list sexp_of_int resolved_vars));
          update_sol_set_correlation new_subtype input_var_set
        end
      end
    in
    helper tv_rel_list num_iter

  let merge_one_set_sol
      (smt_ctx: SmtEmitter.t)
      (input_var_set: IntSet.t)
      (func_type: ArchType.t list)
      (block_subtype_list: ArchType.block_subtype_t list)
      (tv_rel_list: t)
      (tv_rel: type_rel) : SingleSol.t option =
    match tv_rel.sol with
    | SolSimple (SingleSet sol_set) ->
      begin match sol_set with
      | [] -> single_subtype_error "empty sol set"
      | [ sol ] -> Some (SolSimple (Single sol))
      | _ ->
        (* 1. Filter direct subtypes *)
        let direct_subtype =
          List.filter_map (
            fun (sub: subtype_info_t) ->
              match sub.path_pc_list with
              | [] -> single_subtype_error "sub exp has empty pc hist"
              | [ sub_pc ] -> Some (sub.sub_exp, sub_pc)
              | _ -> None
          ) tv_rel.subtype_list
        in
        (* 2. If sub cannot be simplified to one single exp, then skip.
          since we do not handle block context here, we require that all solution must only contain block var!!! *)
        let single_direct_subtype =
          List.filter_map (
            fun (sub, sub_pc) ->
              match sub_sol_single_to_range (fun x -> x) tv_rel_list input_var_set (sub, sub_pc) with
              | Single e -> (* Some (e, sub_pc) *)
                if SingleExp.is_val input_var_set e then
                  Some (e, sub_pc)
                else None
              | _ -> None
          ) direct_subtype
        in
        if List.length direct_subtype > List.length single_direct_subtype then None else
        (* 3. For each sub_exp, get a set of other sub_exp that can replace this sub_exp *)
        let sup_pc = snd tv_rel.var_idx in
        let sub_block_list = 
          List.find_map (
            fun (entry: ArchType.block_subtype_t) -> 
              let sup_block, sub_list = entry in
              if sup_block.pc = sup_pc then Some sub_list else None
          ) block_subtype_list
          |> Option.get
        in
        let update_block_smt_ctx (sub_pc: int) : unit =
          match List.find_opt (fun (block: ArchType.t) -> block.pc = sub_pc) sub_block_list with
          | Some br_block ->
            let sub_block = List.find (fun (block: ArchType.t) -> block.label = br_block.label) func_type in
            (* For single infer, we need to get newest each block's entrance invariants from func_type,
              and br hist from block_subtype_list *)
            SingleContext.add_assertions smt_ctx sub_block.context;
            SingleCondType.add_assertions smt_ctx (List.split br_block.branch_hist |> fst)
          | None ->
            (* NOTE: the not-found case is because for some function call, we will generate subtype relation between vars from the same block. 
              i.e., in this case, there should be sub_pc = sup_pc (we do not check this since here we directly get context from func_type and do not need this info).
              Here we do not add branch history since we do not record pc of the function call.
            *)
            let sub_block = List.find (fun (block: ArchType.t) -> block.pc = sub_pc) func_type in
            SingleContext.add_assertions smt_ctx sub_block.context
        in
        let get_equal_set 
            (general_sol_candidate: SingleExpSet.t) 
            (sub_exp_pc: type_pc_t) : SingleExpSet.t =
          let sub, sub_pc = sub_exp_pc in
          SmtEmitter.push smt_ctx;
          update_block_smt_ctx sub_pc;
          let general_sol_candidate = List.fold_left (
            fun (acc: SingleExpSet.t) (other_sub, _) ->
              (* if other_pc = sub_pc then acc *)
              (* NOTE: We have to remove this optimization, since var generated after fun call with several subtype
                 having the same PC will have its solution merged by mistake. *)
              (* NOTE: this requires that single set solution only contains input vars (added filter to single_direct_subtype_!!! *)
              if SingleExpSet.mem other_sub acc then
                if SingleCondType.check true smt_ctx [ Eq, sub, other_sub ] = SatYes then acc
                else SingleExpSet.remove other_sub acc
              else acc
          ) general_sol_candidate single_direct_subtype
          in
          SmtEmitter.pop smt_ctx 1;
          general_sol_candidate
        in
        let simp_subtype_set = List.map fst single_direct_subtype |> SingleExpSet.of_list in
        let general_sol_candidate =
          List.fold_left get_equal_set 
            simp_subtype_set
            single_direct_subtype
          |> SingleExpSet.to_list
        in
        match general_sol_candidate with
        | [] -> 
          if SingleExpSet.cardinal simp_subtype_set < List.length sol_set then begin
            let simp_subtype_list = SingleExpSet.to_list simp_subtype_set in
            Printf.printf "Partial merge sol for SymImm %d\n" (fst tv_rel.var_idx);
            Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list SingleEntryType.sexp_of_t simp_subtype_list));
            Some (SolSimple (SingleSet simp_subtype_list)) 
          end else None
        | hd :: _ -> 
          Printf.printf "Merge sol for SymImm %d\n" (fst tv_rel.var_idx);
          Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list SingleEntryType.sexp_of_t general_sol_candidate));
          Some (SolSimple (Single hd)) (* TODO: Consider to pick the best one later *)
      end
    | _ -> None

  let merge_all_set_sol
      (smt_ctx: SmtEmitter.t)
      (input_var_set: IntSet.t)
      (func_type: ArchType.t list)
      (block_subtype_list: ArchType.block_subtype_t list)
      (tv_rel_list: t) : t =
    let rec helper (tv_rel_list: t) : t =
      let merge_one_helper
        (acc: bool) (tv_rel: type_rel) : bool * type_rel =
        match merge_one_set_sol smt_ctx input_var_set func_type block_subtype_list tv_rel_list tv_rel with
        | Some simp_sol ->
          true,
          { tv_rel with sol = simp_sol }
        | _ -> acc, tv_rel
      in
      let find_new_sol, tv_rel_list =
        List.fold_left_map merge_one_helper false tv_rel_list
      in
      if find_new_sol then helper tv_rel_list else tv_rel_list
    in
    helper tv_rel_list

  let remove_top_subtype
      (tv_rel_list: t) : t =
    List.filter (fun (x: type_rel) -> SingleSol.has_top x.sol |> not) tv_rel_list

end
