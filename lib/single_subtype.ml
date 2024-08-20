open Isa
open Single_exp
open Single_entry_type
open Arch_type
(* open Pretty_print *)

module RangeExp = struct
  exception RangeExpError of string

  let range_exp_error msg = raise (RangeExpError ("[Range Exp Error] " ^ msg))

  type t = (* TODO: Maybe need more... *)
    | Single of SingleEntryType.t
    | Range of SingleEntryType.t * SingleEntryType.t * int64 (* begin, end, step *)
    | SingleSet of SingleEntryType.t list
    | Top

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

end

module SingleSubtype = struct
  exception SingleSubtypeError of string

  let single_subtype_error msg = raise (SingleSubtypeError ("[Single Subtype Error] " ^ msg))

  type subtype_t = SingleEntryType.t * int

  type type_rel = {
    var_idx: Isa.imm_var_id;
    sol: SingleSol.t;
    subtype_list: subtype_t list; (* subtype, pc of the branch that jumps to var_idx's block *)
    supertype_list: Isa.imm_var_id list (* NOTE: I omit br hist here since I noticed that current implementation cannot use it anyway *)
  }

  type t = type_rel list

  module ArchType = ArchType (SingleEntryType)

  let find_or_add_entry (tv_rel: t) (var_idx: Isa.imm_var_id) : t * type_rel =
    let find_entry = List.find_opt (fun x -> x.var_idx = var_idx) tv_rel in
    match find_entry with
    | Some entry -> tv_rel, entry
    | None ->
      let entry = 
        { var_idx = var_idx; sol = SingleSol.SolNone; subtype_list = []; supertype_list = [] } 
      in
      entry :: tv_rel, entry


  let type_list_insert 
      (type_list: subtype_t list) (ty: subtype_t) : subtype_t list =
    let t_exp, t_pc = ty in
    let find_type = 
      List.find_opt (
        fun (exp, pc) -> SingleEntryType.cmp t_exp exp = 0 && t_pc = pc
      ) type_list
    in
    if find_type != None then type_list else ty :: type_list

  let type_var_list_insert
      (type_var_list: Isa.imm_var_id list) (var_idx: Isa.imm_var_id) : Isa.imm_var_id list =
    if List.find_opt (fun x -> x = var_idx) type_var_list != None then type_var_list else var_idx :: type_var_list

  let add_one_sub_type (tv_rel: type_rel) (ty: subtype_t) : type_rel =
    { tv_rel with subtype_list = type_list_insert tv_rel.subtype_list ty }

  let add_one_super_type (tv_rel: type_rel) (t_idx: int) : type_rel =
    { tv_rel with supertype_list = type_var_list_insert tv_rel.supertype_list t_idx }

  (* Connect a->b *)
  let add_one_sub_super (tv_rel: t) (a_pc: subtype_t) (b_idx: int) : t =
    match a_pc with
    | SingleVar a_idx, _ ->
      if a_idx = b_idx then tv_rel
      else
        List.map (
          fun x ->
            if x.var_idx = a_idx then add_one_super_type x b_idx
            else if x.var_idx = b_idx then add_one_sub_type x a_pc
            else x
        ) tv_rel
    | _ ->
      List.map (
        fun x ->
          if x.var_idx = b_idx then add_one_sub_type x a_pc
          else x
      ) tv_rel


  let add_sub_sub_super
      (tv_rel: t) (a_pc: subtype_t) (b_idx: int) : t =
    match a_pc with
    | SingleVar a_idx, _ ->
      let tv_rel, a_entry = find_or_add_entry tv_rel a_idx in
      let tv_rel =
        List.fold_left (
          fun acc_tv_rel sub_a ->
            add_one_sub_super acc_tv_rel sub_a b_idx
        ) tv_rel a_entry.subtype_list in
      add_one_sub_super tv_rel a_pc b_idx
    | _ -> add_one_sub_super tv_rel a_pc b_idx

  let add_sub_sub_super_super
      (tv_rel: t) (a_pc: subtype_t) (b_idx: int) : t =
    let tv_rel, b_entry = find_or_add_entry tv_rel b_idx in
    let tv_rel =
      List.fold_left (
        fun acc_tv_rel sup_b ->
          add_sub_sub_super acc_tv_rel a_pc sup_b
      ) tv_rel b_entry.supertype_list
    in
    add_sub_sub_super tv_rel a_pc b_idx
    
  let add_full_subtype 
      (tv_rel: t) 
      (a_pc: subtype_t) 
      (b: SingleEntryType.t) : t =
    (* a_pc -> b *)
    match b with
    | SingleVar b_idx -> add_sub_sub_super_super tv_rel a_pc b_idx
    | SingleTop -> tv_rel
    | _ -> let a, pc = a_pc in
      single_subtype_error 
        (Printf.sprintf "add_full_subtype: incorrect sub/super types (%s,%d)->%s"
          (SingleEntryType.to_string a) pc (SingleEntryType.to_string b))

  type useful_var_t = Isa.label * SingleExp.SingleVarSet.t

  let add_one_useful_var_block_subtype
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
            add_sub_sub_super_super acc_tv_rel (sub, sub_pc) sup_idx,
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
        (label, SingleEntryType.SingleVarSet.inter known_useful_var var_set)
      | None -> single_subtype_error "block not found"
    in
    List.fold_left_map helper block_subtype useful_var_list

  let add_one_useful_var_block_subtype
      (block_subtype_list: ArchType.block_subtype_t list)
      (useful_var: useful_var_t)
      (tv_rel: t) :
      t * (ArchType.block_subtype_t list) * (useful_var_t list) =
    let sup_label, sup_useful_var = useful_var in
    let block_subtype = 
      List.find (fun (x: ArchType.block_subtype_t) -> let x, _ = x in x.label = sup_label) block_subtype_list 
    in
    let tv_rel, new_useful_var_list = add_one_useful_var_block_subtype block_subtype sup_useful_var tv_rel in
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
    match useful_var_list with
    | [] -> tv_rel, block_subtype_list
    | hd :: tl ->
      let tv_rel, block_subtype_list, new_useful_var_list =
        add_one_useful_var_block_subtype block_subtype_list hd tv_rel
      in
      let useful_var_list = merge_useful_var tl new_useful_var_list in
      add_all_useful_var_block_subtype block_subtype_list useful_var_list tv_rel

  let init_useful_var_from_block_subtype
      (block_subtype_list: ArchType.block_subtype_t list) : useful_var_t list =
    List.map (
      fun (x: ArchType.block_subtype_t) ->
        let x, _ = x in
        x.label, x.useful_var
    ) block_subtype_list
    
  let init_useful_var_from_block_subtype
      (block_subtype_list: ArchType.block_subtype_t list) : t * (ArchType.block_subtype_t list) =
    let useful_var_list = init_useful_var_from_block_subtype block_subtype_list in
    add_all_useful_var_block_subtype block_subtype_list useful_var_list []

end
