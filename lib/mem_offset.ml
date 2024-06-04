open Single_exp
open Pretty_print

module MemOffset = struct
  exception MemOffsetError of string
  let mem_offset_error msg = raise (MemOffsetError ("[Mem Offset Error] " ^ msg))

  type t = SingleExp.t * SingleExp.t

  module ConstraintSet = Set.Make(
    struct
      let compare = SingleExp.cmp
      type t = SingleExp.t
    end
  )

  let constraint_union (constraint_list: ConstraintSet.t list) : ConstraintSet.t =
    let helper (acc: ConstraintSet.t) (entry: ConstraintSet.t) : ConstraintSet.t =
      ConstraintSet.union acc entry
    in
    List.fold_left helper ConstraintSet.empty constraint_list

  let pp_constraint_set (lvl: int) (constraint_set: ConstraintSet.t) =
    PP.print_lvl lvl "Constraint set (>=0):\n";
    List.iteri (
      fun i x ->
        PP.print_lvl (lvl + 1) "<Constraint %d> " i;
        SingleExp.pp_single_exp (lvl + 2) x;
        Printf.printf "\n" 
    ) (ConstraintSet.elements constraint_set)

  let heuristic_cmp (e: SingleExp.t) : int * bool = (* (sign, sign is determined or not) *)
    match e with
    | SingleConst v ->
      if v > 0L then (1, true) else if v = 0L then (0, true) else (-1, true)
    | SingleBExp (SingleAdd, _, SingleConst offset)
    | SingleBExp (SingleAdd, _, SingleBExp (SingleMul, _, SingleConst offset))
    | SingleBExp (SingleAdd, SingleBExp (SingleMul, _, SingleConst offset), _) ->
      if offset > 0L then 
        (1, false) 
      else if offset < 0L then 
        (-1, false)
      else 
        mem_offset_error "single exp +0 -> not fully evaluated"
    | SingleBExp (SingleMul, _, SingleConst coeff) ->
      if coeff > 0L then 
        (1, false) 
      else if coeff < 0L then 
        (-1, false) 
      else 
        mem_offset_error "single exp * -> not fully evaluated"
    | _ -> (1, false)

  let conditional_ge (e1: SingleExp.t) (e2: SingleExp.t) : bool * ConstraintSet.t =
    let diff = SingleExp.eval (SingleBExp (SingleSub, e1, e2)) in
    let cmp_result, cmp_determined = heuristic_cmp diff in
    if cmp_determined then
      (cmp_result >= 0, ConstraintSet.empty)
    else
      (cmp_result >= 0, ConstraintSet.singleton diff)
    (* match diff with
    | SingleConst v ->
      if v >= 0L then (true, ConstraintSet.empty) else (false, ConstraintSet.empty)
    | SingleBExp (SingleAdd, SingleVar _, SingleConst offset)
    | SingleBExp (SingleAdd, SingleBExp (SingleMul, SingleVar _, SingleConst _), SingleConst offset) ->
      if offset >= 0L then (true, ConstraintSet.singleton diff) else (false, ConstraintSet.empty)
    | _ -> (false, ConstraintSet.empty) *)
  (* This might not be true anymore!!! (ge, cond) ge = true means e1 >= e2 if cond is true; ge = false means no conclusion is made from this comparison!!! *)

  let get_conditional_greater (e1: SingleExp.t) (e2: SingleExp.t) : SingleExp.t * ConstraintSet.t =
    let diff = SingleExp.eval (SingleBExp (SingleSub, e1, e2)) in
    let cmp_result, cmp_determined = heuristic_cmp diff in
    if cmp_determined then
      ((if cmp_result >= 0 then e1 else e2), ConstraintSet.empty)
    else begin
      if cmp_result > 0 then
        (e1, ConstraintSet.singleton diff)
      else if cmp_result < 0 then
        (e2, ConstraintSet.singleton (SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleMul, diff, SingleExp.SingleConst (-1L)))))
      else
        mem_offset_error "get_conditional_greater cmp_result = 0 while cmp_determined = false"
    end
    (* match diff with
    | SingleConst v ->
      if v >= 0L then Some (e1, ConstraintSet.empty) else Some (e2, ConstraintSet.empty)
    | SingleBExp (SingleAdd, SingleVar _, SingleConst offset)
    | SingleBExp (SingleAdd, SingleBExp (SingleMul, SingleVar _, SingleConst _), SingleConst offset) ->
      if offset >= 0L then 
        Some (e1, ConstraintSet.singleton diff) 
      else 
        Some (e2, ConstraintSet.singleton (SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleMul, diff, SingleExp.SingleConst (-1L)))))
    | _ -> None *)

  let get_conditional_less (e1: SingleExp.t) (e2: SingleExp.t) : SingleExp.t * ConstraintSet.t =
    let diff = SingleExp.eval (SingleBExp (SingleSub, e1, e2)) in
    let cmp_result, cmp_determined = heuristic_cmp diff in
    if cmp_determined then
      ((if cmp_result <= 0 then e1 else e2), ConstraintSet.empty)
    else begin
      if cmp_result < 0 then
        (e1, ConstraintSet.singleton (SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleMul, diff, SingleExp.SingleConst (-1L)))))
      else if cmp_result > 0 then
        (e2, ConstraintSet.singleton diff)
      else
        mem_offset_error "get_conditional_less cmp_result = 0 while cmp_determined = false"
    end
    (* match diff with
    | SingleConst v ->
      if v <= 0L then Some (e1, ConstraintSet.empty) else Some (e2, ConstraintSet.empty)
    | SingleBExp (SingleAdd, SingleVar _, SingleConst offset)
    | SingleBExp (SingleAdd, SingleBExp (SingleMul, SingleVar _, SingleConst _), SingleConst offset) ->
      if offset <= 0L then 
        Some (e1, ConstraintSet.singleton (SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleMul, diff, SingleExp.SingleConst (-1L))))) 
      else 
        Some (e2, ConstraintSet.singleton diff)
    | _ -> None *)

  let check_offset (offset: t) : ConstraintSet.t =
    let left, right = offset in
    let cond_ge, cond = conditional_ge right left in
    if cond_ge then cond 
    else begin
      Printf.printf "Offset [%s, %s]\n" (SingleExp.string_of_single_exp left) (SingleExp.string_of_single_exp right);
      mem_offset_error "check_offset failed"
    end
  (* TODO: Think about whether here should be greater than instead of greater than or equal to!!! *)

  let cmp_or_merge (o1: t) (o2: t) : ((bool, t) Either.t) * ConstraintSet.t =
    (* let off_cond = ConstraintSet.union (check_offset o1) (check_offset o2) in *)
    let l1, r1 = o1 in
    let l2, r2 = o2 in
    let l2_ge_r1, l2_cond_r1 = conditional_ge l2 r1 in
    if l2_ge_r1 then (Left true, l2_cond_r1)
    else
      let l1_ge_r2, l1_cond_r2 = conditional_ge l1 r2 in
      if l1_ge_r2 then (Left false, l1_cond_r2)
      else
        let l, cond_l = get_conditional_less l1 l2 in
        let r, cond_r = get_conditional_greater r1 r2 in
        (Right (l, r), ConstraintSet.union cond_l cond_r)
        (* match get_conditional_less l1 l2, get_conditional_greater r1 r2 with
        | Some (l, cond_l), Some (r, cond_r) -> 
          (Right (l, r), ConstraintSet.union cond_l cond_r)
        | _ ->
          Printf.printf "Cannot merge [%s, %s] [%s, %s]\n" 
              (SingleExp.string_of_single_exp l1) 
              (SingleExp.string_of_single_exp r1) 
              (SingleExp.string_of_single_exp l2) 
              (SingleExp.string_of_single_exp r2);
          mem_offset_error "cmp_or_merge cannot merge address offset range"  *)

  let equal (o1: t) (o2: t) : bool =
    let l1, r1 = o1 in
    let l2, r2 = o2 in
    SingleExp.cmp l1 l2 = 0 && SingleExp.cmp r1 r2 = 0

  let subset (o1: t) (o2: t) : bool * ConstraintSet.t =
    let l1, r1 = o1 in
    let l2, r2 = o2 in
    let l1_ge_l2, l1_cond_l2 = conditional_ge l1 l2 in
    let r2_ge_r1, r2_cond_r1 = conditional_ge r2 r1 in
    if l1_ge_l2 && r2_ge_r1 then (true, ConstraintSet.union l1_cond_l2 r2_cond_r1)
    else (false, ConstraintSet.empty)

  let cmp (o1: t) (o2: t) : int =
    let l1, r1 = o1 in
    let l2, r2 = o2 in
    let cmp_l = SingleExp.cmp l1 l2 in
    if cmp_l = 0 then
      SingleExp.cmp r1 r2
    else cmp_l

end
