open Isa_basic
open Single_entry_type
open Cond_type_new
open Arch_type
open Set_sexp
open Sexplib.Std
open Sexplib
open Single_context
open Smt_emitter

module SingleInputVarCondSubtype = struct
  exception SingleInputVarCondSubtypeError of string
  let single_input_var_cond_subtype_error msg = raise (SingleInputVarCondSubtypeError ("[Single Input Var Cond Subtype Error] " ^ msg))

  module CondType = CondType (SingleEntryType)
  module ArchType = ArchType (SingleEntryType)

  module CondSet = struct
    include Set.Make (
      struct
        let compare = CondType.cmp
        type t = CondType.t
      end
    )

    let t_of_sexp (s_exp: Sexp.t) : t = 
      of_list (list_of_sexp CondType.t_of_sexp s_exp)
  
    let sexp_of_t (s: t) : Sexp.t = 
      sexp_of_list CondType.sexp_of_t (elements s)
  end

  type pc_cond_t = (IsaBasic.label * int) * CondSet.t
  [@@deriving sexp]

  type pc_cond_subtype_t = (IsaBasic.label * int) * (pc_cond_t list)
  [@@deriving sexp]

  let find_label_full (label: IsaBasic.label) (pc_sth_list: ((IsaBasic.label * int) * 'a) list) : (IsaBasic.label * int) * 'a =
    List.find (fun ((x, _), _) -> label = x) pc_sth_list

  let find_label (label: IsaBasic.label) (pc_sth_list: ((IsaBasic.label * int) * 'a) list) : 'a =
    let _, result = find_label_full label pc_sth_list in result

  let get_not_taken_branch_hist
      (sub_single_helper: SingleEntryType.t -> SingleEntryType.t option)
      (block_subtype_list: ArchType.block_subtype_t list) : ((IsaBasic.label * int) * ((CondType.t * (CondType.t option)) list)) list =
    let sub_cond_helper (cond_pc: CondType.t * int) : CondType.t * CondType.t option =
      let cond, _ = cond_pc in
      let c, l, r = cond in
      match sub_single_helper l with
      | None | Some SingleTop -> cond, None
      | Some simp_l ->
        begin match sub_single_helper r with
        | None | Some SingleTop -> cond, None
        | Some simp_r -> cond, Some (c, simp_l, simp_r)
        end
    in
    List.map (
      fun (entry: ArchType.block_subtype_t) ->
        let block, _ = entry in
        (block.label, block.pc),
        List.map sub_cond_helper block.full_not_taken_hist
    ) block_subtype_list

  let get_simp_branch_hist_from_full_not_taken_hist
      (full_not_taken_hist_list: ((IsaBasic.label * int) * ((CondType.t * (CondType.t option)) list)) list)
      (arch_type: ArchType.t) : pc_cond_t =
    let rec helper 
        (simp_hist: CondType.t list)
        (branch_hist: CondType.t list)
        (full_not_taken_hist: (CondType.t * (CondType.t option)) list) : 
        CondType.t list =
      match branch_hist, full_not_taken_hist with
      | [], _ -> simp_hist
      | br_hd :: tl, (orig, Some simp) :: not_taken_tl ->
        if CondType.cmp br_hd orig = 0 then
          helper (simp :: simp_hist) tl not_taken_tl
        else if CondType.cmp br_hd (CondType.not_cond_type orig) = 0 then
          helper ((CondType.not_cond_type simp) :: simp_hist) tl not_taken_tl
        else single_input_var_cond_subtype_error "Branch hist does not match not taken hist"
      | _ :: tl, (_, None) :: not_taken_tl ->
        helper simp_hist tl not_taken_tl
      | _, [] -> single_input_var_cond_subtype_error "full_not_taken_hist is shorter than branch hist"
    in
    (* Printf.printf "Arch pc %d%!\n" arch_type.pc; *)
    let full_not_taken_hist = find_label arch_type.label full_not_taken_hist_list in
    (arch_type.label, arch_type.pc),
    CondSet.of_list (helper [] (List.rev (List.map fst arch_type.branch_hist)) (List.rev full_not_taken_hist))

  let init_input_var_cond_subtype
      (sub_single_helper: SingleEntryType.t -> SingleEntryType.t option)
      (block_subtype_list: ArchType.block_subtype_t list) : pc_cond_subtype_t list =
    let full_not_taken_branch_hist_list = get_not_taken_branch_hist sub_single_helper block_subtype_list in
    (* Printf.printf "full_not_taken_branch_hist_list\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list sexp_of_int (List.map fst full_not_taken_branch_hist_list))); *)
    List.map (
      fun (entry: ArchType.block_subtype_t) ->
        let block, sub_block_list = entry in
        (block.label, block.pc),
        List.map (get_simp_branch_hist_from_full_not_taken_hist full_not_taken_branch_hist_list) sub_block_list
    ) block_subtype_list

  let solve_one_var
      (var_sol_list: pc_cond_t list)
      (var_subtype: pc_cond_subtype_t) :
      bool * (pc_cond_t list) =
    let var_pc, sub_list = var_subtype in
    (* 1. Substitute current sol to get full cond of each sub block *)
    let sub_cond_list =
      List.map (
        fun ((label, _), cond) ->
          CondSet.union (find_label label var_sol_list) cond
      ) sub_list
    in
    (* 2. Find intersect of all sub's cond set, which is a conservative or operation of all cond *)
    let cond_sol =
      match sub_cond_list with
      | [] -> CondSet.empty
      | hd :: tl -> 
        List.fold_left (
          fun (acc: CondSet.t) (entry: CondSet.t) ->
            CondSet.inter acc entry
        ) hd tl
    in
    (* 3. Check whether sol is stronger and update sol if so *)
    let cond_sol_size = CondSet.cardinal cond_sol in
    List.fold_left_map (
      fun (acc: bool) (entry: pc_cond_t) ->
        let pc, old_sol = entry in
        if acc then true, entry
        else if pc = var_pc && CondSet.cardinal old_sol < cond_sol_size then 
          true, (pc, cond_sol)
        else false, entry
    ) false var_sol_list

  let solve_vars_one_iter
      (always_solve: bool)
      (update_list: IntSet.t)
      (var_sol_list: pc_cond_t list)
      (var_subtype_list: pc_cond_subtype_t list) :
      IntSet.t * (pc_cond_t list) =
    List.fold_left (
      fun (acc: IntSet.t * (pc_cond_t list)) (entry: pc_cond_subtype_t) ->
        let acc_update_list, acc_sol = acc in
        let (var_label, var_pc), sub_cond_list = entry in
        let need_solve =
          if always_solve then true
          else
            List.find_opt (
              fun ((label, pc), _) ->
                label != var_label && pc > var_pc && IntSet.mem pc update_list
            ) sub_cond_list != None
        in
        if need_solve then
          let need_update, acc_sol = solve_one_var acc_sol entry in
          if need_update then
            IntSet.add var_pc acc_update_list, acc_sol
          else acc_update_list, acc_sol
        else
          acc
    ) (IntSet.empty, var_sol_list) var_subtype_list

  let check_cond_list
      (smt_ctx: SmtEmitter.t)
      (pc_cond_map: pc_cond_t list) : unit =
    let helper (pc_cond: pc_cond_t) : unit =
      SmtEmitter.push smt_ctx;
      let ctx, solver = smt_ctx in
      let cond_list = CondSet.to_list (snd pc_cond) in
      List.iter (
        fun (x: CondType.t) ->
          SingleContext.add_assertions smt_ctx [ SingleContext.Cond x ];
          let open Z3 in
          match Z3.Solver.check solver [ Boolean.mk_true ctx ] with
          | UNSATISFIABLE -> 
            Printf.printf "pc_cond\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_pc_cond_t pc_cond));
            Printf.printf "Unsat cond\n%s\n" (Sexplib.Sexp.to_string_hum (CondType.sexp_of_t x));
            SmtEmitter.pp_smt_ctx 0 smt_ctx;
            single_input_var_cond_subtype_error "cond UNSATISFIABLE"
          | _ -> ()
      ) cond_list;
      SmtEmitter.pop smt_ctx 1
    in
    List.iter helper pc_cond_map

  let solve 
      (smt_ctx: SmtEmitter.t)
      (sub_single_helper: SingleEntryType.t -> SingleEntryType.t option)
      (block_subtype_list: ArchType.block_subtype_t list) :
      pc_cond_t list =
    let var_subtype_list = init_input_var_cond_subtype sub_single_helper block_subtype_list in
    let rec helper
        (always_solve: bool)
        (update_list: IntSet.t)
        (var_sol_list: pc_cond_t list)
        (iter: int) : IntSet.t * (pc_cond_t list) =
      if iter = 0 then begin
        (if IntSet.cardinal update_list > 0 then
          Printf.printf "Did not solve all update list"
        else ());
        update_list, var_sol_list
      end else
        let update_list, var_sol_list = 
          solve_vars_one_iter always_solve update_list var_sol_list var_subtype_list 
        in
        helper false update_list var_sol_list (iter - 1)
    in
    let init_sol = List.map (fun (pc, _) -> pc, CondSet.empty) var_subtype_list in
    let _, pc_cond_map = helper true IntSet.empty init_sol 10 in
    Printf.printf "pc_cond_map\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list sexp_of_pc_cond_t pc_cond_map));
    check_cond_list smt_ctx pc_cond_map;
    pc_cond_map
    (* Printf.printf "pc_cond_map\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list sexp_of_pc_cond_t pc_cond_map));
    List.map2 (
      fun (pc_cond: pc_cond_t) (block_subtype: ArchType.block_subtype_t) ->
        let (label, _), cond_set = pc_cond in
        let arch_type, _ = block_subtype in
        if label != arch_type.label then single_input_var_cond_subtype_error "label does not match"
        else
          (* Note we clear old context since it is also comes from the input var cond *)
          let map (c: CondType.t) : SingleCondType.t = (* Dirty fix on type *)
            let cond, l, r = c in
            let cond : SingleCondType.cond =
              match cond with
              | Eq -> Eq | Ne -> Ne
              | Le -> Le | Lt -> Lt
              | Be -> Be | Bt -> Bt
            in
            cond, l, r
          in
          if List.length arch_type.context <> 0 then begin 
            Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list SingleContext.sexp_of_t arch_type.context));
            single_input_var_cond_subtype_error "clear out arch_type context"
          end else
          { arch_type with
            context = List.map (fun (x: CondType.t) -> SingleContext.Cond (map x)) (CondSet.to_list cond_set)  
          }
    ) pc_cond_map block_subtype_list *)
        
end
