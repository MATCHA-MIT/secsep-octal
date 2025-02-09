open Isa_basic
open Single_entry_type
open Mem_offset_new
open Constraint
open Range_exp
open Cond_type_new
open Single_context
open Arch_type
open Arch_context_map
open Single_subtype
open Pretty_print
open Smt_emitter
open Set_sexp
open Sexplib.Std

module RangeSubtype = struct
  exception RangeSubtypeError of string

  let range_subtype_error msg = raise (RangeSubtypeError ("[Range Subtype Error] " ^ msg))

  type var_idx_t = MemRange.range_var_id * int
  [@@deriving sexp]

  type type_exp_t = MemRange.t * int
  [@@deriving sexp]

  type type_rel = {
    var_idx: var_idx_t;
    sol: MemRange.t option;
    off: MemOffset.t;
    subtype_list: type_exp_t list;
    equal_subtype_list: var_idx_t list;
    (* supertype_list: MemRange.range_var_id list; *)
    equal_var_set: IntSet.t;
    read_constraint_list: MemOffset.t list;
  }
  [@@deriving sexp]

  type t = type_rel list
  [@@deriving sexp]

  module ArchType = ArchType (SingleEntryType)

  let type_exp_to_string (x: type_exp_t) : string =
    let exp, pc = x in
    Printf.sprintf "%s(%d)" (MemRange.to_string exp) (pc)

  let sol_to_string (x: MemRange.t option) : string =
    match x with
    | Some e -> Printf.sprintf "Some %s" (MemRange.to_string e)
    | None -> "None"

  let pp_type_rel (lvl: int) (x: type_rel) =
    let var_idx, var_pc = x.var_idx in
    PP.print_lvl lvl "<RangeVar %d> at pc %d\n"  var_idx var_pc;
    PP.print_lvl (lvl + 1) "Sol: %s\n" (sol_to_string x.sol);
    PP.print_lvl (lvl + 1) "Off: %s\n" (MemOffset.to_string x.off);
    PP.print_lvl (lvl + 1) "Subtype: [\n";
    List.iter (fun sub -> PP.print_lvl (lvl + 2) "%s;\n" (type_exp_to_string sub)) x.subtype_list;
    PP.print_lvl (lvl + 1) "]\n";
    PP.print_lvl (lvl + 1) "EqualSet: %s\n" (Sexplib.Sexp.to_string (IntSet.sexp_of_t x.equal_var_set));
    PP.print_lvl (lvl + 1) "ReadConstraintList: %s\n" (Sexplib.Sexp.to_string (sexp_of_list MemOffset.sexp_of_t x.read_constraint_list))

  let pp_range_subtype (lvl: int) (tv_rels: t) =
    List.iter (fun x -> pp_type_rel lvl x) tv_rels

  let add_range_subtype_helper
      (tv_rel_list: t) (off: MemOffset.t) (sub: type_exp_t) (sup: var_idx_t) : t =
    let helper (acc: bool) (tv_rel: type_rel) : bool * type_rel =
      if acc then true, tv_rel
      else if tv_rel.var_idx = sup then
        true, { tv_rel with subtype_list = sub :: tv_rel.subtype_list }
      else false, tv_rel
    in
    let found, tv_rel_list = List.fold_left_map helper false tv_rel_list in
    if found then tv_rel_list
    else
      let new_tv_rel: type_rel = {
        var_idx = sup;
        sol = None;
        off = off;
        subtype_list = [ sub ];
        equal_subtype_list = [];
        equal_var_set = IntSet.empty;
        read_constraint_list = [];
      } in
      new_tv_rel :: tv_rel_list

  let add_range_subtype
      (tv_rel_list: t) (off: MemOffset.t) (sub: type_exp_t) (sup: type_exp_t) : t =
    match sup with
    | RangeConst _, _ -> tv_rel_list
    | RangeExp _, _ -> 
      range_subtype_error (Printf.sprintf "add_range_subtype cannot add %s -> %s" (type_exp_to_string sub) (type_exp_to_string sup))
    | RangeVar sup_idx, sup_pc ->
      add_range_subtype_helper tv_rel_list off sub (sup_idx, sup_pc)

  let get_one_block_subtype
      (tv_rel_list: t)
      (block_subtype: ArchType.block_subtype_t) : t =
    let sup_block, sub_block_list = block_subtype in
    let helper
        (acc: t) (sub_block: ArchType.t) : t =
      List.fold_left2 (
        fun (acc: t) (_, sub_part_mem) (_, sup_part_mem) ->
          List.fold_left2 (
            fun (acc: t) (_, sub_range, _) (off, sup_range, _) ->
              add_range_subtype acc off (sub_range, sub_block.pc) (sup_range, sup_block.pc)
          ) acc sub_part_mem sup_part_mem
      ) acc sub_block.mem_type sup_block.mem_type
    in
    List.fold_left helper tv_rel_list sub_block_list
  
  let get_one_block_subtype_faster
      (block_subtype: ArchType.block_subtype_t) : t =
    let sup_block, sub_block_list = block_subtype in
    let mem_subtype =
      ArchType.MemType.map_full (
        fun (off, range, _) -> off, range, ((range, sup_block.pc), [])
      ) sup_block.mem_type
    in
    let helper
        (acc: ('a * type_exp_t list) ArchType.MemType.mem_content)
        (sub_block: ArchType.t) :
        ('a * type_exp_t list) ArchType.MemType.mem_content =
      ArchType.MemType.map2_full true (
        fun (off, range, (sup, sub_list)) (_, sub_range, _) ->
          off, range, (sup, (sub_range, sub_block.pc) :: sub_list)
      ) acc sub_block.mem_type
    in
    let mem_subtype = List.fold_left helper mem_subtype sub_block_list in
    ArchType.MemType.fold_left_full (
      fun (acc: t) (entry: MemOffset.t * MemRange.t * (type_exp_t * (type_exp_t list))) ->
        let off, _, (sup, sub_list) = entry in
        match sup with
        | RangeConst _, _ -> acc
        | RangeExp _, _ -> 
          range_subtype_error (Printf.sprintf "add_range_subtype cannot add sup %s" (type_exp_to_string sup))
        | RangeVar sup_idx, sup_pc ->
          { 
            var_idx = (sup_idx, sup_pc); 
            sol = None; 
            off = off; 
            subtype_list = sub_list; 
            equal_subtype_list = []; 
            equal_var_set = IntSet.empty;
            read_constraint_list = [];
          } :: acc
    ) [] mem_subtype

  
  let simplify_subtype_range
      (repl_helper: (SingleEntryType.t * int) -> SingleEntryType.t)
      (tv_rel_list: t) : t =
    let map_helper = fun (x, y) -> MemRange.repl repl_helper y x, y in
    List.map (
      fun (x: type_rel) ->
        { x with subtype_list = List.map map_helper x.subtype_list }
    ) tv_rel_list

  let get_range_constraint
      (block_subtype: ArchType.block_subtype_t list) : t =
    let tv_rel_list_list = List.map get_one_block_subtype_faster block_subtype in
    List.flatten tv_rel_list_list

  let get_read_constraint
      (tv_rel_list: t) (read_constraint_map: MemOffsetSet.t IntMap.t) : t =
    List.map (
      fun (tv_rel: type_rel) ->
        match IntMap.find_opt (fst tv_rel.var_idx) read_constraint_map with
        | Some read_set ->
          { tv_rel with read_constraint_list = read_set |> MemOffsetSet.to_list }
        | None -> tv_rel
    ) tv_rel_list

  let filter_self_subtype (tv_rel: type_rel) : type_rel =
    let var_idx, _ = tv_rel.var_idx in
    let subtype_list, equal_subtype_list =
      List.partition_map (
        fun (x, pc) ->
          match x with
          | MemRange.RangeVar v ->
            if v = var_idx || IntSet.mem v tv_rel.equal_var_set then
              Right (v, pc)
            else Left (x, pc)
          | _ -> Left (x, pc)
      ) tv_rel.subtype_list
    in
    let equal_subtype_list = List.filter (fun (x, _) -> x <> var_idx) equal_subtype_list in
    { tv_rel with
      subtype_list = subtype_list;
      equal_subtype_list = equal_subtype_list;
    }

  let try_solve_full (tv_rel: type_rel) : MemRange.t option =
    let find_not_full =
      List.find_opt (
        fun (x, _) ->
          match x with
          | MemRange.RangeConst [ off ]
          | MemRange.RangeExp (_, [ off ]) ->
            MemOffset.cmp off tv_rel.off <> 0
          | _ -> true
      ) tv_rel.subtype_list
    in
    if List.length tv_rel.subtype_list = 0 || find_not_full <> None then None
    else Some (RangeConst [ tv_rel.off ])

  let try_solve_empty 
      (input_var_set: SingleEntryType.SingleVarSet.t)
      (* (get_block_var: MemRange.t -> SingleEntryType.SingleVarSet.t)  *)
      (tv_rel: type_rel) : MemRange.t option =
    let find_empty =
      List.find_opt (
        fun (x, _) ->
          match x with
          | MemRange.RangeConst [] -> true
          | _ -> false
      ) tv_rel.subtype_list
    in
    if find_empty <> None then
      let find_not_val =
        List.find_opt (
          fun (x, _) -> (* not (SingleEntryType.SingleVarSet.is_empty (get_block_var x)) *)
            not (MemRange.is_val input_var_set x)
            (* match x with
            | MemRange.RangeExp _ -> not (SingleEntryType.SingleVarSet.is_empty (get_block_var x))
            | _ -> false *)
        ) tv_rel.subtype_list
      in
      if find_not_val <> None then None
      else Some (RangeConst [])
    else None

  let try_solve_non_val
      (smt_ctx: SmtEmitter.t)
      (get_block_var: MemRange.t -> SingleEntryType.SingleVarSet.t)
      (block_subtype_list: ArchType.block_subtype_t list)
      (* (find_type_pair: int -> int -> IsaBasic.imm_var_id -> SingleEntryType.t * SingleEntryType.t) *)
      (tv_rel: type_rel) : MemRange.t option =
    let find_type_pair 
        (sup_pc: int) (sub_pc: int) (sub_var_idx: IsaBasic.imm_var_id) :
        (SingleEntryType.t * SingleEntryType.t) list =
      let sup_block, sub_block_list = 
        List.find (fun (entry: ArchType.block_subtype_t) -> let x, _ = entry in x.pc = sup_pc) block_subtype_list 
      in
      let sub_block =
        List.find (fun (x: ArchType.t) -> x.pc = sub_pc) sub_block_list
      in
      let helper 
          (acc: (SingleEntryType.t * SingleEntryType.t) list)
          (entry1: SingleEntryType.t) (entry2: SingleEntryType.t) :
          (SingleEntryType.t * SingleEntryType.t) list =
        if SingleEntryType.SingleVarSet.is_empty 
            (SingleEntryType.SingleVarSet.inter (SingleEntryType.get_vars entry1) sup_block.useful_var) then
          acc
        else
          let entry2 = SingleEntryType.repl_local_var sub_block.local_var_map entry2 in
          (* TODO: Need to fix here to support different loops *)
          if SingleEntryType.SingleVarSet.mem sub_var_idx (SingleEntryType.get_vars entry2) && 
              SingleEntryType.cmp (SingleVar sub_var_idx) entry2 <> 0 then
            (entry1, entry2) :: acc
          else acc
      in
      let find_list = List.fold_left2 helper [] sup_block.reg_type sub_block.reg_type in
      ArchType.MemType.fold_left2 helper find_list sup_block.mem_type sub_block.mem_type
    in
    match tv_rel.subtype_list with
    | [RangeConst base_range, _; RangeExp (_, [ add_off ]), br_pc]
    | [RangeExp (_, [ add_off ]), br_pc; RangeConst base_range, _] ->
      if SingleEntryType.SingleVarSet.is_empty (get_block_var (RangeConst base_range)) then begin
        let block_var_list = SingleEntryType.SingleVarSet.to_list (get_block_var (RangeConst [ add_off ])) in
        begin match block_var_list with
        | [ br_var_idx ] ->
          let _, sup_pc = tv_rel.var_idx in
          let find_list = find_type_pair sup_pc br_pc br_var_idx in
          begin match find_list with
          | [ sup_exp, br_exp ] -> 
            let off_l, off_r = add_off in
            let full_l, full_r = tv_rel.off in
            let map_to_next = (br_var_idx, br_exp) in
            let map_to_sup = (br_var_idx, sup_exp) in
            let off_l_next = SingleEntryType.eval (SingleEntryType.repl_var_exp off_l map_to_next) in
            let off_r_next = SingleEntryType.eval (SingleEntryType.repl_var_exp off_r map_to_next) in
            if SingleEntryType.cmp off_r off_l_next = 0 then
              Some (MemRange.merge smt_ctx (RangeConst base_range)
                (RangeConst [ full_l, SingleEntryType.eval (SingleEntryType.repl_var_exp off_l map_to_sup) ])
              )
            else if SingleEntryType.cmp off_l off_r_next = 0 then
              Some (MemRange.merge smt_ctx (RangeConst base_range)
                (RangeConst [ SingleEntryType.eval (SingleEntryType.repl_var_exp off_r map_to_sup), full_r ])
              )
            else
              None
          | _ -> None
          end
        | _ -> None
        end
      end else None
    | _ -> None

  let update_br_context_helper
      (smt_ctx: SmtEmitter.t)
      (block_subtype_list: ArchType.block_subtype_t list)
      (sup_pc: int) (sub_pc: int) : unit =
    let sub_block_list = 
      List.find_map (
        fun (entry: ArchType.block_subtype_t) -> 
          let sup_block, sub_list = entry in
          if sup_block.pc = sup_pc then Some sub_list else None
      ) block_subtype_list
      |> Option.get
    in
    let sub_block = List.find (fun (block: ArchType.t) -> block.pc = sub_pc) sub_block_list in
    SingleContext.add_assertions smt_ctx sub_block.context;
    SingleCondType.add_assertions smt_ctx (List.split sub_block.branch_hist |> fst)

  let try_solve_extra_slot
      (smt_ctx: SmtEmitter.t)
      (sub_sol_single_to_range_helper: SingleEntryType.t * int -> RangeExp.t)
      (block_subtype_list: ArchType.block_subtype_t list)
      (input_var_set: IntSet.t)
      (ctx_map_map: ArchContextMap.t)
      (tv_rel: type_rel) : MemRange.t option =
    let get_context_map = ArchContextMap.get_context_map ctx_map_map in
    let get_reverse_map = ArchContextMap.get_reverse_map ctx_map_map in
    let is_loop_extra_off (extra_off: MemOffset.t) (extra_off_pc: int) : bool =
      let l, r = extra_off in
      match sub_sol_single_to_range_helper (l, extra_off_pc) with
      | Range _ ->
        begin match sub_sol_single_to_range_helper (r, extra_off_pc) with
        | Range _ -> true
        | _ -> false
        end
      | _ -> false
    in
    let guess_sol_template
        (subtype_exp: type_exp_t) : (int * MemOffset.t) list =
      let sub_exp, br_pc = subtype_exp in
      match sub_exp with
      | RangeConst _ -> []
        (* if MemRange.is_val known_var_set (RangeConst off_list) then
          Some (br_pc, List.map (MemOffset.repl_var reverse_map) off_list)
        else None *)
      | RangeVar _ -> []
      | RangeExp (_, [ extra_off ]) ->
        if is_loop_extra_off extra_off br_pc then begin
          let reverse_map = get_reverse_map br_pc in
          let known_var_set = SingleEntryType.get_mapped_var_set reverse_map |> IntSet.union input_var_set in
          Printf.printf "known_var_set\n%s\n" (Sexplib.Sexp.to_string_hum (IntSet.sexp_of_t known_var_set));
          Printf.printf "br_pc: %d extra offset %s\n" br_pc (MemOffset.to_string extra_off);
          if MemOffset.is_val known_var_set extra_off then
            let extra_l, extra_r = MemOffset.repl_var reverse_map extra_off in
            let off_l, off_r = tv_rel.off in
            [
              br_pc, (off_l, extra_r); (* off U extra_off = [0, k] U [k, k+1] *)
              br_pc, (extra_l, off_r); (* off U extra_off = [k + 1, n] U [k, k+1] *)
            ]
          else []
        end else []
      | RangeExp _ -> []
    in
    let sup_pc = snd tv_rel.var_idx in
    let filter_sat_sol_template 
        (acc_sol_template_list: (int * MemOffset.t) list)
        (sub_exp_pc: type_exp_t) : (int * MemOffset.t) list =
      let sub_exp, sub_pc = sub_exp_pc in
      let test_one_sol_template (br_sol_template: int * MemOffset.t) : bool =
        (* if test success, return true!!! *)
        let br_pc, sol_template = br_sol_template in (* br_pc is used to skip unnecessary tests on the same source. *)
        if sub_pc = br_pc then true else
        let sub_context_map = get_context_map sub_pc in
        let sub_sol = MemOffset.repl_var sub_context_map sol_template in
        Printf.printf "sub_sol %s\nsub_exp %s%!\n" (MemOffset.to_string sub_sol) (MemRange.to_string sub_exp);
        (* Check sub_sol is subset of sub_exp *)
        match Constraint.gen_off_subset smt_ctx sub_sol sub_exp tv_rel.off with
        | [] -> true
        | remain_off_list -> 
          Printf.printf "remain off %s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list Constraint.sexp_of_t remain_off_list));
          false
      in
      SmtEmitter.push smt_ctx;
      update_br_context_helper smt_ctx block_subtype_list sup_pc sub_pc;
      let result = List.filter test_one_sol_template acc_sol_template_list in
      SmtEmitter.pop smt_ctx 1;
      result
    in
    let possible_sol_template_list = List.concat_map guess_sol_template tv_rel.subtype_list in
    let possible_sol_template_list = List.fold_left filter_sat_sol_template possible_sol_template_list tv_rel.subtype_list in
    match possible_sol_template_list with
    | [] -> None
    | (_, hd) :: _ -> Some (RangeConst [hd])
  
  type offset_pc_t = MemOffset.t * int
  [@@deriving sexp]

  let try_solve_const_slot
      (smt_ctx: SmtEmitter.t)
      (sub_sol_single_to_range_helper: SingleEntryType.t * int -> RangeExp.t)
      (block_subtype_list: ArchType.block_subtype_t list)
      (tv_rel: type_rel) : MemRange.t option =
    let naive_sub_list =
      List.filter_map (
        fun (sub, sub_pc) ->
          match sub with
          | MemRange.RangeConst c_list ->
            begin match c_list with
            | [] -> Some (None, sub_pc)
            | [ off ] -> Some (Some off, sub_pc)
            | _ -> None
            end
          | _ -> None
      ) tv_rel.subtype_list
    in
    if List.length tv_rel.subtype_list > List.length naive_sub_list then None else
    (* Replace single solution *)
    let single_naive_sub_list =
      List.filter_map (
        fun (sub, sub_pc) ->
          match sub with
          | None -> Some (sub, sub_pc)
          | Some (l, r) ->
            begin match sub_sol_single_to_range_helper (l, sub_pc) with
            | Single simp_l ->
              begin match sub_sol_single_to_range_helper (r, sub_pc) with
              | Single simp_r -> Some (Some (simp_l, simp_r), sub_pc)
              | _ -> None
              end
            | _ -> None
            end
      ) naive_sub_list
    in
    if List.length naive_sub_list > List.length single_naive_sub_list then None else
    (* Get helper for update block context*)
    let sup_pc = snd tv_rel.var_idx in
    (* Get sol template *)
    let get_sat_off_set
        (sol_off_candidate: int MemOffsetMap.t)
        (sub_range_pc: MemOffset.t option * int) : int MemOffsetMap.t =
      let sub_range, sub_pc = sub_range_pc in
      let check_subset (sol_candidate_off: MemOffset.t) : MemOffset.off_rel_t =
        match sub_range with
        | None ->
          let l, r = sol_candidate_off in 
          if SingleCondType.check true smt_ctx [ Eq, l, r ] = SatYes then Eq else Other
        | Some sub_off -> 
          MemOffset.offset_quick_cmp smt_ctx sol_candidate_off sub_off MemOffset.CmpEqSubset
      in
      SmtEmitter.push smt_ctx;
      update_br_context_helper smt_ctx block_subtype_list sup_pc sub_pc;
      let sol_off_candidate = List.fold_left (
        fun (acc: int MemOffsetMap.t) (other_sub, other_pc) ->
          match other_sub with
          | None -> acc
          | Some other_sub_off ->
            let add_one () : int MemOffsetMap.t =
              MemOffsetMap.update other_sub_off (
                fun (count_opt: int option) -> Option.map (Int.add 1) count_opt
              ) acc
            in
            if other_pc = sub_pc then add_one ()
            else
              if MemOffsetMap.mem other_sub_off acc then begin
                let check_subset_result = check_subset other_sub_off in
                if check_subset_result = Eq then add_one ()
                else if check_subset_result = Subset then acc
                else MemOffsetMap.remove other_sub_off acc
              end else acc
      ) sol_off_candidate single_naive_sub_list
      in
      SmtEmitter.pop smt_ctx 1;
      sol_off_candidate
    in
    let sol_off_candidate = List.filter_map fst single_naive_sub_list |> List.map (fun x -> x, 0) in
    let sol_off_candidate = 
      List.fold_left get_sat_off_set (MemOffsetMap.of_list sol_off_candidate) single_naive_sub_list
      |> MemOffsetMap.to_list
    in
    match sol_off_candidate with
    | [] -> None
    | (hd, _) :: [] -> Some (RangeConst [hd])
    | _ ->
      let hd, _ =
        List.sort (
          fun (off_a, count_a) (off_b, count_b) ->
            let cmp_count = Int.neg (Int.compare count_a count_b) in
            if cmp_count = 0 then MemOffset.cmp off_a off_b
            else cmp_count
        ) sol_off_candidate |> List.hd
      in
      Some (RangeConst [hd])

  let solve_one_var
      (smt_ctx: SmtEmitter.t)
      (sub_sol_single_to_range_helper: SingleEntryType.t * int -> RangeExp.t)
      (* (single_sol_repl_to_range_helper: (SingleEntryType.t * int) -> SingleEntryType.t) *)
      (input_var_set: IntSet.t)
      (ctx_map_map: ArchContextMap.t)
      (* (get_block_var: MemRange.t -> SingleEntryType.SingleVarSet.t) *)
      (block_subtype_list: ArchType.block_subtype_t list)
      (new_sol_list: (var_idx_t * MemRange.t) list) (tv_rel: type_rel) : 
      ((var_idx_t * MemRange.t) list) * type_rel =
    let rule_list = [
      try_solve_extra_slot smt_ctx sub_sol_single_to_range_helper block_subtype_list input_var_set ctx_map_map;
      try_solve_const_slot smt_ctx sub_sol_single_to_range_helper block_subtype_list;
      try_solve_full;
      try_solve_empty input_var_set;
      (* try_solve_non_val smt_ctx get_block_var block_subtype_list; *)
    ] in
    match tv_rel.sol with
    | Some _ -> new_sol_list, tv_rel
    | None ->
      (* let filter_tv_rel = filter_self_subtype tv_rel in *)
      begin match List.find_map (fun rule -> rule tv_rel) rule_list with
      | Some sol -> 
        (tv_rel.var_idx, sol) :: new_sol_list,
        { tv_rel with sol = Some sol }
      | None -> new_sol_list, tv_rel
      end

  let find_br_map
      (block_subtype_list: ArchType.block_subtype_t list)
      (blk_pc: int) (br_pc: int)
      (block_var_set: IntSet.t) : SingleEntryType.local_var_map_t option =
    let sup_block, sub_block_list =
      List.find (
        fun (entry: ArchType.block_subtype_t) ->
          let block, _ = entry in
          block.pc = blk_pc
      ) block_subtype_list
    in
    let sub_block_opt =
      List.find_opt (
        fun (a_type: ArchType.t) -> a_type.pc = br_pc
      ) sub_block_list
    in
    match sub_block_opt with
    | None -> range_subtype_error (Printf.sprintf "Cannot find blk_pc %d br_pc %d" blk_pc br_pc)
    | Some sub_block -> begin
      let helper 
            (acc: IntSet.t * SingleEntryType.local_var_map_t)
            (entry1: SingleEntryType.t) (entry2: SingleEntryType.t) :
            IntSet.t * SingleEntryType.local_var_map_t =
          if SingleEntryType.SingleVarSet.is_empty 
              (SingleEntryType.SingleVarSet.inter (SingleEntryType.get_vars entry1) sup_block.useful_var) then
            acc
          else
            let entry2 = SingleEntryType.repl_local_var sub_block.local_var_map entry2 in
            (* TODO: Need to fix here to support different loops *)
            let remain_var_set, var_map = acc in
            match entry2 with
            | SingleVar v2 ->
              if IntSet.mem v2 remain_var_set then
                IntSet.remove v2 remain_var_set,
                (v2, entry1) :: var_map
              else acc
            | _ -> acc
      in
      let acc = List.fold_left2 helper (block_var_set, []) sup_block.reg_type sub_block.reg_type in
      let remain_var_set, var_map = ArchType.MemType.fold_left2 helper acc sup_block.mem_type sub_block.mem_type in
      if IntSet.is_empty remain_var_set then Some var_map
      else None
    end

  let repl_br_map_sol
      (* (single_sol_repl_helper: (SingleEntryType.t * int) -> SingleEntryType.t) *)
      (get_block_var: MemRange.t -> SingleEntryType.SingleVarSet.t)
      (block_subtype_list: ArchType.block_subtype_t list)
      (blk_pc: int) (sol_exp: type_exp_t) : type_exp_t =
    (* TODO: Use the new arch_context_map here *)
    let sol, br_pc = sol_exp in
    match sol with
    | RangeConst _ ->
      let block_var_set = get_block_var sol in
      if IntSet.is_empty block_var_set then sol, blk_pc
      else begin
        match find_br_map block_subtype_list blk_pc br_pc block_var_set with
        | Some var_map -> 
          let curr_block_sol = MemRange.repl_var var_map sol in
          curr_block_sol, blk_pc
          (* let simp_sol = MemRange.repl single_sol_repl_helper blk_pc curr_block_sol in
          simp_sol, blk_pc *)
        | None -> sol_exp
      end
    | _ -> sol_exp

  let repl_range_val_sol
      (smt_ctx: SmtEmitter.t)
      (* (single_sol_repl_helper: (SingleEntryType.t * int) -> SingleEntryType.t) *)
      (get_block_var: MemRange.t -> SingleEntryType.SingleVarSet.t)
      (block_subtype_list: ArchType.block_subtype_t list)
      (* (range_is_val: MemRange.t -> bool) *)
      (range_sol: ((MemRange.range_var_id * int) * MemRange.t) list)
      (tv_rel_list: t) : t =
    (* let range_sol =
      List.filter (
        fun (_, r) -> range_is_val r
      ) range_sol
    in *)
    let rec helper
        (tv_rel_list: t) (range_sol: ((MemRange.range_var_id * int) * MemRange.t) list) : t =
      let tv_rel_list, range_sol_opt = 
        List.map (
          fun (x: type_rel) ->
            if x.sol = None then
              if List.is_empty x.subtype_list then
                let sol =
                  List.find_map (
                    fun ((range_var, _), range_sol) ->
                      if IntSet.is_empty (get_block_var range_sol) then
                        if IntSet.mem range_var x.equal_var_set then
                          Some range_sol
                        else None
                      else
                        let subtype_pc_opt =
                          List.find_map (
                            fun (subtype_var_id, subtype_pc) ->
                              if subtype_var_id = range_var then Some subtype_pc
                              else None
                          ) x.equal_subtype_list
                        in
                        match subtype_pc_opt with
                        | Some br_pc ->
                          let _, blk_pc = x.var_idx in
                          let sol, sub_sol_pc = repl_br_map_sol get_block_var block_subtype_list blk_pc (range_sol, br_pc) in
                          if sub_sol_pc = blk_pc then Some sol
                          else None
                        | None -> None
                  ) range_sol
                in
                match sol with
                | Some s ->
                  { x with sol = sol }, Some (x.var_idx, s)
                | None -> x, None
              else
              (* TODO: Possible issue: the smt_ctx here may not contain enough constraints on block variables. *)
                let subtype_list =
                  List.map (
                    fun (sub, pc) -> 
                      if pc = (snd x.var_idx) then sub, pc
                      else
                        let sub = MemRange.repl_range_sol smt_ctx range_sol sub in
                        sub, pc
                        (* We should not change the subtype's context here!!! *)
                        (* repl_br_map_sol single_sol_repl_helper get_block_var block_subtype_list (snd x.var_idx) (sub, pc) *)
                  ) x.subtype_list
                in
                { x with subtype_list = subtype_list }, None
            else x, None
        ) tv_rel_list |> List.split
      in
      let range_sol = List.filter_map (fun x -> x) range_sol_opt in
      if List.is_empty range_sol then
        tv_rel_list
      else helper tv_rel_list range_sol
    in
    helper tv_rel_list range_sol

  let solve
      (smt_ctx: SmtEmitter.t)
      (single_sol: SingleSubtype.t)
      (* (single_sol_repl_to_range_helper: (SingleEntryType.t * int) -> SingleEntryType.t) *)
      (input_var_set: IntSet.t)
      (* (range_is_val: MemRange.t -> bool)  *)
      (* (get_block_var: MemRange.t -> SingleEntryType.SingleVarSet.t) *)
      (block_subtype_list: ArchType.block_subtype_t list)
      (tv_rel_list: t) (iter: int) : t =
    (* let single_sol_repl_helper = SingleSubtype.subsititue_one_exp_subtype_list single_sol in *)
    let sub_sol_single_to_range_helper = SingleSubtype.sub_sol_single_to_range (fun x -> x) single_sol input_var_set in
    let ctx_map_map = ArchContextMap.init input_var_set block_subtype_list in
    Printf.printf "ctx_map_map\n%s\n" (Sexplib.Sexp.to_string_hum (ArchContextMap.sexp_of_t ctx_map_map));
    let get_block_var = fun r -> SingleEntryType.SingleVarSet.diff (MemRange.get_vars r) input_var_set in
    let rec helper (tv_rel_list: t) (iter: int) : t =
      if iter <= 0 then tv_rel_list
      else begin
        (* Printf.printf "===========%d===========" iter;
        pp_range_subtype 0 tv_rel_list; *)
        (* let tv_rel_list = 
          simplify_subtype_range single_sol_repl_helper tv_rel_list 
        in *)
        let new_sol, tv_rel_list =
          List.fold_left_map 
            (solve_one_var smt_ctx sub_sol_single_to_range_helper input_var_set ctx_map_map block_subtype_list) 
            [] tv_rel_list
        in
        if List.length new_sol = 0 then tv_rel_list
        else
          let tv_rel_list = repl_range_val_sol smt_ctx get_block_var block_subtype_list new_sol tv_rel_list in
          helper tv_rel_list (iter - 1)
      end
    in
    let result = helper tv_rel_list iter in
    result

  let repl_sol_arch_type
      (tv_rel_list: t)
      (a_type: ArchType.t) : ArchType.t =
    let helper (entry: MemOffset.t * MemRange.t * 'a) : MemOffset.t * MemRange.t * 'a =
      let off, range, e_type = entry in
      match range with
      | RangeConst _ -> entry
      | RangeVar v ->
        let sol_opt =
          List.find_map (
            fun (x: type_rel) ->
              let idx, _ = x.var_idx in
              if v = idx then x.sol
              else None
          ) tv_rel_list
        in
        begin match sol_opt with
        | Some sol -> off, sol, e_type
        | _ -> entry
        end
      | _ -> range_subtype_error (Printf.sprintf "We do not support repl sol for range exp %s" (MemRange.to_string range))
    in
    { a_type with mem_type = ArchType.MemType.map_full helper a_type.mem_type }

  let get_unresolved_var
      (tv_rel_list: t) : IntSet.t =
    List.fold_left (
      fun (acc: IntSet.t) (tv_rel: type_rel) ->
        match tv_rel.sol with
        | Some _ -> acc
        | None -> IntSet.add (fst tv_rel.var_idx) acc
    ) IntSet.empty tv_rel_list

end
