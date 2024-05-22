open Isa

module SingleExp = struct
  exception SingleExpError of string
  let single_exp_error msg = raise (SingleExpError ("[Single Exp Error] " ^ msg))

  type single_bop =
    | SingleAdd
    | SingleSub
    | SingleMul
    | SingleSal
    | SingleSar
    | SingleXor
    | SingleAnd
    | SingleOr

  type single_uop =
    | SingleNot

  type t =
    | SingleConst of int
    | SingleVar of Isa.imm_var_id
    | SingleBExp of single_bop * t * t
    | SingleUExp of single_uop * t

  let cmp_single_bop (op1: single_bop) (op2: single_bop) : int =
    match op1, op2 with
    | SingleAdd, SingleAdd -> 0
    | SingleAdd, _ -> -1
    | SingleSub, SingleAdd -> 1
    | SingleSub, SingleSub -> 0
    | SingleSub, _ -> -1
    | SingleMul, SingleAdd
    | SingleMul, SingleSub -> 1
    | SingleMul, SingleMul -> 0
    | SingleMul, _ -> -1
    | SingleSal, SingleAdd
    | SingleSal, SingleSub
    | SingleSal, SingleMul -> 1
    | SingleSal, SingleSal -> 0
    | SingleSal, _ -> -1
    | SingleSar, SingleOr
    | SingleSar, SingleAnd
    | SingleSar, SingleXor -> -1
    | SingleSar, SingleSar -> 0
    | SingleSar, _ -> 1
    | SingleXor, SingleOr
    | SingleXor, SingleAnd -> -1
    | SingleXor, SingleXor -> 0
    | SingleXor, _ -> 1
    | SingleAnd, SingleOr -> -1
    | SingleAnd, SingleAnd -> 0
    | SingleAnd, _ -> 1
    | SingleOr, SingleOr -> 0
    | SingleOr, _ -> 1

  let cmp_single_uop (op1: single_uop) (op2: single_uop) : int =
    match op1, op2 with
    | SingleNot, SingleNot -> 0

  let rec cmp_single_exp (e1: t) (e2: t) : int =
    match e1, e2 with
    | SingleConst c1, SingleConst c2 ->
      if c1 = c2 then 0 else if c1 > c2 then 1 else -1
    | SingleConst _, _ -> -1
    | SingleVar v1, SingleVar v2 -> 
      if v1 = v2 then 0 else if v1 > v2 then 1 else -1
    | SingleVar _, SingleConst _ -> 1
    | SingleVar _, _ -> -1
    | SingleBExp (bop1, l1, r1), SingleBExp (bop2, l2, r2) -> 
      let c_bop = cmp_single_bop bop1 bop2 in
      let c_l = cmp_single_exp l1 l2 in
      let c_r = cmp_single_exp r1 r2 in
      if c_bop = 0 then
        if c_l = 0 then c_r
        else c_l
      else c_bop
    | SingleBExp _, _ -> 1
    | SingleUExp (uop1, l1), SingleUExp (uop2, l2) -> 
      let c_uop = cmp_single_uop uop1 uop2 in
      let c_l = cmp_single_exp l1 l2 in
      if c_uop = 0 then c_l else c_uop
    | SingleUExp _, SingleBExp _ -> -1
    | SingleUExp _, _ -> 1


  (* type t_exp =
    | Mul of (t list)
    | Add of (t_exp list)

  let rec try_split (e: t) : t_exp =
    match e with
    | SingleBExp (SingleAdd, l, r) ->
      let split_l = try_split l in
      let split_r = try_split r in
      begin match (split_l, split_r) with
      | (Mul m1, Mul m2) -> Add [Mul m1, Mul m2]
      | (SingleAdd, Mul)
      end

  let rec eval_t_exp (e: t_exp) : t_exp =
    match e with
    | Mul (coeff, terms) ->  *)

  let rec unroll_t (e: t) : t list list =
    match e with
    | SingleBExp (SingleAdd, l, r) ->
      (unroll_t l) @ (unroll_t r)
    | SingleBExp (SingleSub, l, r) ->
      let ul = unroll_t l in
      let ur = unroll_t r in
      let neg_ur = List.map (fun x -> (SingleConst (-1)) :: x) ur in
      ul @ neg_ur
    | SingleBExp (SingleMul, l, r) ->
      let ul = unroll_t l in
      let ur = unroll_t r in
      let helper (x: t list) : t list list =
        List.map (fun y -> x @ y) ur
      in
      List.concat_map helper ul
    | SingleBExp (SingleSal, l, SingleConst r) ->
      let ul = unroll_t l in
      List.map (fun x -> (SingleConst (Int.shift_left 1 r)) :: x) ul
    | _ -> [ [e] ]
      

end