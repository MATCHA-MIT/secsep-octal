open Single_exp
open Single_entry_type
open Cond_type_new
open Arch_type
open Set_sexp
open Smt_emitter


module SingleIteEval = struct
  exception SingleIteEvalError of string
  let string_ite_eval_error msg = raise (SingleIteEvalError ("[Single Ite Eval] " ^ msg))

  module BrSingleCondMap = IntMapSexp (
    struct
      type t = SingleCondSet.t * SingleCondSet.t
      [@@deriving sexp]
    end
  )

  type t = BrSingleCondMap.t
  [@@deriving sexp]

  module BrTargetMap = Map.Make (
    struct
      type t = int * int
      let compare = compare
    end
  )

  module ArchType = ArchType (SingleEntryType)

  let init
      (smt_ctx: SmtEmitter.t)
      (update_block_smt_ctx: SmtEmitter.t -> ArchType.t -> unit)
      (single_subtype_list: (int * (SingleExp.t * int) list) list)
      (block_subtype_list: ArchType.block_subtype_t list) : t =
    let rec collect_ite_cond (e: SingleExp.t) : SingleCondSet.t =
      match e with
      | SingleITE (cond, e1, e2) ->
        let s1 = collect_ite_cond e1 in
        let s2 = collect_ite_cond e2 in
        let s = SingleCondSet.union s1 s2 in
        if SingleCondType.has_top cond then s
        else SingleCondSet.add cond s
        (* SingleCondSet.add cond (SingleCondSet.union s1 s2) *)
      | _ -> SingleCondSet.empty
    in
    let collect_subtype_helper
        (acc: SingleCondSet.t BrTargetMap.t)
        (single_subtype: int * (SingleExp.t * int) list) :
        SingleCondSet.t BrTargetMap.t =
      let target_pc, subtype_list = single_subtype in
      List.fold_left (
        fun (acc: SingleCondSet.t BrTargetMap.t) (e_pc: SingleExp.t * int) ->
          let e, br_pc = e_pc in
          let e_cond_set = collect_ite_cond e in
          BrTargetMap.update (target_pc, br_pc) (
            fun (s_opt: SingleCondSet.t option) ->
              match s_opt with
              | None -> Some e_cond_set
              | Some s -> Some (SingleCondSet.union e_cond_set s)
          ) acc
      ) acc subtype_list
    in
    let eval_cond_set
        (pc_cond_set: ((int * int) * SingleCondSet.t)) : 
        int * (SingleCondSet.t * SingleCondSet.t) =
      let (target_pc, br_pc), all_cond_set = pc_cond_set in
      let br_block = ArchType.get_branch_block block_subtype_list (target_pc, br_pc) in
      SmtEmitter.push smt_ctx;
      update_block_smt_ctx smt_ctx br_block;
      let cond_result_list =
        List.filter_map (
          fun x ->
            match SingleCondType.check true smt_ctx [x] with
            | SatUnknown -> None
            | check_result -> Some (x, check_result = SatYes)
        ) (SingleCondSet.to_list all_cond_set)
      in
      let cond_true_list, cond_false_list =
        List.partition_map (
          fun (x, check_result) ->
            if check_result then Left x else Right x
        ) cond_result_list
      in
      SmtEmitter.pop smt_ctx 1;
      br_pc, (SingleCondSet.of_list cond_true_list, SingleCondSet.of_list cond_false_list)
    in
    let br_sub_cond_map = List.fold_left collect_subtype_helper BrTargetMap.empty single_subtype_list in
    List.map eval_cond_set (BrTargetMap.to_list br_sub_cond_map) |> BrSingleCondMap.of_list

  let eval 
      (br_single_cond_map: t)
      (e_pc: SingleExp.t * int) : SingleExp.t =
    let e, pc = e_pc in
    match BrSingleCondMap.find_opt pc br_single_cond_map with
    | None -> e
    | Some (m_true, m_false) ->
      let rec helper (e: SingleExp.t) : SingleExp.t =
        match e with
        | SingleITE (cond, e1, e2) ->
          let e1 = helper e1 in
          let e2 = helper e2 in
          if SingleCondSet.mem cond m_true then e1
          else if SingleCondSet.mem cond m_false then e2
          else SingleITE (cond, e1, e2)
        | _ -> e
      in
      helper e

end

