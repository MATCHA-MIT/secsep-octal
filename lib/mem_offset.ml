open Single_exp

module MemOffset = struct
  exception MemOffsetError of string
  let mem_offset_error msg = raise (MemOffsetError ("[Mem Offset Error] " ^ msg))

  type t = SingleExp.t * SingleExp.t

  let conditional_ge (e1: SingleExp.t) (e2: SingleExp.t) : bool * (SingleExp.t list) =
    let diff = SingleExp.eval (SingleBExp (SingleSub, e1, e2)) in
    match diff with
    | SingleConst diff ->
      if diff >= 0L then (true, []) else (false, [])
    | SingleBExp (SingleAdd, SingleVar _, SingleConst offset)
    | SingleBExp (SingleAdd, SingleBExp (SingleMul, SingleVar _, SingleConst _), SingleConst offset) ->
      if offset >= 0L then (true, [diff]) else (false, [])
    | _ -> (false, [])
  (* (ge, cond) ge = true means e1 >= e2 if cond is true; ge = false means no conclusion is made from this comparison!!! *)

  let get_conditional_greater (e1: SingleExp.t) (e2: SingleExp.t) : (SingleExp.t * (SingleExp.t list)) option =
    let diff = SingleExp.eval (SingleBExp (SingleSub, e1, e2)) in
    match diff with
    | SingleConst diff ->
      if diff >= 0L then Some (e1, []) else Some (e2, [])
    | SingleBExp (SingleAdd, SingleVar _, SingleConst offset)
    | SingleBExp (SingleAdd, SingleBExp (SingleMul, SingleVar _, SingleConst _), SingleConst offset) ->
      if offset >= 0L then Some (e1, [diff]) else Some (e2, [diff])
    | _ -> None

  let get_conditional_less (e1: SingleExp.t) (e2: SingleExp.t) : (SingleExp.t * (SingleExp.t list)) option =
    let diff = SingleExp.eval (SingleBExp (SingleSub, e1, e2)) in
    match diff with
    | SingleConst diff ->
      if diff <= 0L then Some (e1, []) else Some (e2, [])
    | SingleBExp (SingleAdd, SingleVar _, SingleConst offset)
    | SingleBExp (SingleAdd, SingleBExp (SingleMul, SingleVar _, SingleConst _), SingleConst offset) ->
      if offset <= 0L then Some (e1, [diff]) else Some (e2, [diff])
    | _ -> None

  let check_offset (offset: t) : SingleExp.t list =
    let left, right = offset in
    let cond_ge, cond = conditional_ge right left in
    if cond_ge then cond 
    else begin
      Printf.printf "Offset [%s, %s]\n" (SingleExp.string_of_single_exp left) (SingleExp.string_of_single_exp right);
      mem_offset_error "check_offset failed"
    end
  (* TODO: Think about whether here should be greater than instead of greater than or equal to!!! *)

  let cmp_or_merge (o1: t) (o2: t) : ((bool, t) Either.t) * (SingleExp.t list) =
    let off_cond = (check_offset o1) @ (check_offset o2) in
    let l1, r1 = o1 in
    let l2, r2 = o2 in
    let l2_ge_r1, l2_cond_r1 = conditional_ge l2 r1 in
    if l2_ge_r1 then (Left true, off_cond @ l2_cond_r1)
    else
      let l1_ge_r2, l1_cond_r2 = conditional_ge l1 r2 in
      if l1_ge_r2 then (Left false, off_cond @ l1_cond_r2)
      else
        match get_conditional_less l1 l2, get_conditional_greater r1 r2 with
        | Some (l, cond_l), Some (r, cond_r) -> (Right (l, r), off_cond @ cond_l @ cond_r)
        | _ ->
          Printf.printf "Cannot merge [%s, %s] [%s, %s]\n" 
              (SingleExp.string_of_single_exp l1) 
              (SingleExp.string_of_single_exp r1) 
              (SingleExp.string_of_single_exp l2) 
              (SingleExp.string_of_single_exp r2);
          mem_offset_error "cmp_or_merge cannot merge address offset range" 

end
