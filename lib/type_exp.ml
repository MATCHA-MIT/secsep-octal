open Single_exp
open Pretty_print

module TypeExp = struct
  exception TypeExpError of string
  let type_exp_error msg = raise (TypeExpError ("[Type Exp Error] " ^ msg))

  type type_var_id = int

  type type_bop =
    | TypeAdd
    | TypeSub
    | TypeMul
    | TypeSal
    | TypeShr
    | TypeSar
    | TypeXor
    | TypeAnd
    | TypeOr
    | TypeInter
    | TypeUnion
    | TypeDiff

  type type_uop =
    | TypeNot
    | TypeComp

  type t =
    | TypeSingle of SingleExp.t
    | TypeRange of SingleExp.t * bool * SingleExp.t * bool * int64 (* begin, inc, end, inc, step *)
    | TypeVar of type_var_id
    | TypeTop
    | TypeBot
    | TypeBExp of type_bop * t * t
    | TypeUExp of type_uop * t

  let cmp_type_bop (op1: type_bop) (op2: type_bop) : int =
    match op1, op2 with
    | TypeAdd, TypeAdd -> 0
    | TypeAdd, _ -> -1
    | TypeSub, TypeAdd -> 1
    | TypeSub, TypeSub -> 0
    | TypeSub, _ -> -1
    | TypeMul, TypeAdd | TypeMul, TypeSub -> 1
    | TypeMul, TypeMul -> 0
    | TypeMul, _ -> -1
    | TypeSal, TypeAdd | TypeSal, TypeSub | TypeSal, TypeMul -> 1
    | TypeSal, TypeSal -> 0
    | TypeSal, _ -> -1
    | TypeShr, TypeAdd | TypeShr, TypeSub | TypeShr, TypeMul | TypeShr, TypeSal -> 1
    | TypeShr, TypeShr -> 0
    | TypeShr, _ -> -1
    | TypeSar, TypeAdd | TypeSar, TypeSub | TypeSar, TypeMul | TypeSar, TypeSal | TypeSar, TypeShr -> 1
    | TypeSar, TypeSar -> 0
    | TypeSar, _ -> -1
    | TypeXor, TypeDiff | TypeXor, TypeUnion | TypeXor, TypeInter | TypeXor, TypeOr | TypeXor, TypeAnd -> -1
    | TypeXor, TypeXor -> 0
    | TypeXor, _ -> 1
    | TypeAnd, TypeDiff | TypeAnd, TypeUnion | TypeAnd, TypeInter | TypeAnd, TypeOr -> -1
    | TypeAnd, TypeAnd -> 0
    | TypeAnd, _ -> 1
    | TypeOr, TypeDiff | TypeOr, TypeUnion | TypeOr, TypeInter -> -1
    | TypeOr, TypeOr -> 0
    | TypeOr, _ -> 1
    | TypeInter, TypeDiff | TypeInter, TypeUnion -> -1
    | TypeInter, TypeInter -> 0
    | TypeInter, _ -> 1
    | TypeUnion, TypeDiff -> -1
    | TypeUnion, TypeUnion -> 0
    | TypeUnion, _ -> 1
    | TypeDiff, TypeDiff -> 0
    | TypeDiff, _ -> 1

  let cmp_type_uop (op1: type_uop) (op2: type_uop) : int =
    if op1 = op2 then 0
    else if op1 = TypeNot then -1
    else 1

  let rec cmp (e1: t) (e2: t) : int =
    match e1, e2 with
    | TypeBot, TypeBot -> 0
    | TypeBot, _ -> -1
    | TypeTop, TypeBot -> 1
    | TypeTop, TypeTop -> 0
    | TypeTop, _ -> -1
    | TypeSingle _, TypeBot | TypeSingle _, TypeTop -> 1
    | TypeSingle s1, TypeSingle s2 -> SingleExp.cmp s1 s2
    | TypeSingle _, _ -> -1
    | TypeRange _, TypeBot | TypeRange _, TypeTop | TypeRange _, TypeSingle _ -> 1
    | TypeRange (a1, _, b1, _, s1), TypeRange (a2, _, b2, _, s2) ->
      let cmp_a = SingleExp.cmp a1 a2 in
      if cmp_a = 0 then
        let cmp_b = SingleExp.cmp b1 b2 in
        if cmp_b = 0 then if s1 < s2 then -1 else if s1 = s2 then 0 else 1
        else cmp_b
      else cmp_a
    | TypeRange _, _ -> -1
    | TypeVar _, TypeBExp _ | TypeVar _, TypeUExp _ -> -1
    | TypeVar v1, TypeVar v2 ->
      if v1 < v2 then -1 else if v1 = v2 then 0 else 1
    | TypeVar _, _ -> 1
    | TypeBExp (bop1, l1, r1), TypeBExp (bop2, l2, r2) ->
      let c_bop = cmp_type_bop bop1 bop2 in
      let c_l = cmp l1 l2 in
      let c_r = cmp r1 r2 in
      if c_bop = 0 then
        if c_l = 0 then c_r else c_l
      else c_bop
    | TypeBExp _, _ -> 1
    | TypeUExp (uop1, l1), TypeUExp (uop2, l2) ->
      let c_uop = cmp_type_uop uop1 uop2 in
      let c_l = cmp l1 l2 in
      if c_uop = 0 then c_l else c_uop
    | TypeUExp _, TypeBExp _ -> -1
    | TypeUExp _, _ -> 1

  (* let rec cmp (e1: t) (e2: t) : bool =
    match e1, e2 with
    | TypeSingle s1, TypeSingle s2 -> SingleExp.cmp s1 s2 = 0
    | TypeRange (l1, bl1, r1, br1, s1), TypeRange (l2, bl2, r2, br2, s2) ->
      (SingleExp.cmp l1 l2 = 0) && (bl1 = bl2) && (SingleExp.cmp r1 r2 = 0) && (br1 = br2) && (s1 = s2)
    | TypeVar v1, TypeVar v2 -> v1 = v2
    | TypeTop, TypeTop -> true
    | TypeBot, TypeBot -> true
    | TypeBExp (bop1, l1, r1), TypeBExp (bop2, l2, r2) ->
      (bop1 = bop2) && (cmp l1 l2) && (cmp r1 r2)
    | TypeUExp (uop1, l1), TypeUExp (uop2, l2) ->
      (uop1 = uop2) && (cmp l1 l2)
    | _ -> false *)

  (* let rec strip_type_exp (e: t list) : t list =
    match e with
    | [] -> e
    | TypeBot :: tl | TypeTop :: tl | TypeSingle _ :: tl | TypeRange _ :: tl -> strip_type_exp tl
    | _ -> e

  let rec cmp_list_helper (e1: t list) (e2: t list) : int =
    match e1, e2 with
    | [], [] -> 0
    | [], _ :: _ -> -1
    | _ :: _, [] -> 1
    | hd1 :: tl1, hd2 :: tl2 ->
      let cmp_hd = cmp hd1 hd2 in
      if cmp_hd = 0 then cmp_list_helper tl1 tl2 else cmp_hd
    
  let cmp_list (e1: t list) (e2: t list) : int =
    cmp_list_helper (strip_type_exp e1) (strip_type_exp e2)

  let convert_t (e: t list list) : t =
    let rec helper_mul (x: t list) =
      match x with
      | [] -> TypeBot
      | hd :: [] -> hd
      | hd :: tl -> TypeBExp (TypeMul, helper_mul tl, hd)
    in
    let rec helper_add (x: t list) =
      match x with
      | [] -> TypeBot
      | hd :: [] -> hd
      | hd :: tl -> TypeBExp (TypeAdd, helper_add tl, hd)
    in
    helper_add (List.map helper_mul e)

  let mul_const (coeff: int64) (e: t list) : t list =
    match e with
    | [] -> []
    | TypeBot :: _ -> [ TypeBot ]
    | TypeTop :: _ -> [ TypeTop ]
    | (TypeSingle c) :: tl -> (TypeSingle (SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleMul, SingleConst coeff, c)))) :: tl
    | tl -> TypeSingle (SingleConst coeff) :: tl

  let mul_t (e1: t list) (e2: t list) : t list =
    let x = List.sort cmp (e1 @ e2) in
    match x with
    | TypeBot :: _ -> [ TypeBot ]
    | TypeTop :: _ -> [ TypeTop ]
    | TypeSingle c1 :: TypeSingle c2 :: tl ->
      let c12 = SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleMul, c1, c2)) in
      begin match c12 with
      | SingleConst c -> 
        if c = 0L then [ TypeSingle (SingleConst 0L) ]
        else if c = 1L then tl
        else (TypeSingle c12) :: tl
      | _ -> (TypeSingle c12) :: tl
      end
    | TypeSingle (SingleConst 0L) :: _ -> [ TypeSingle (SingleConst 0L) ]
    (* | SingleConst c :: _ ->
      if c = 0L then [] else x *)
    | _ -> x *)

  let rec eval (e: t) : t =
    match e with
    | TypeBExp (tbop, e1, e2) -> 
      let ee1 = eval e1 in
      let ee2 = eval e2 in
      let default_type = TypeBExp (tbop, ee1, ee2) in
      begin match ee1, ee2 with
      | TypeSingle s1, TypeSingle s2 -> 
        begin match tbop with
        | TypeAdd -> TypeSingle (SingleExp.eval (SingleBExp (SingleAdd, s1, s2)))
        | TypeSub -> TypeSingle (SingleExp.eval (SingleBExp (SingleSub, s1, s2)))
        | TypeMul -> TypeSingle (SingleExp.eval (SingleBExp (SingleMul, s1, s2)))
        | TypeSal -> TypeSingle (SingleExp.eval (SingleBExp (SingleSal, s1, s2)))
        | TypeSar -> TypeSingle (SingleExp.eval (SingleBExp (SingleSal, s1, s2)))
        | TypeXor -> TypeSingle (SingleExp.eval (SingleBExp (SingleXor, s1, s2)))
        | TypeAnd -> TypeSingle (SingleExp.eval (SingleBExp (SingleAnd, s1, s2)))
        | TypeOr -> TypeSingle (SingleExp.eval (SingleBExp (SingleOr, s1, s2)))
        | TypeInter -> 
          begin match s1, s2 with
          | SingleConst v1, SingleConst v2 -> if v1 = v2 then TypeSingle (SingleConst v1) else TypeBot
          | SingleVar v1, SingleVar v2 -> if v1 = v2 then TypeSingle (SingleVar v1) else e (* TODO Double check this! *)
          | _ -> default_type
          end
        | TypeDiff -> 
          begin match s1, s2 with
          | SingleConst v1, SingleConst v2 -> if v1 = v2 then TypeBot else TypeSingle (SingleConst v1)
          | SingleVar v1, SingleVar v2 -> if v1 = v2 then TypeBot else TypeSingle (SingleVar v1)
          | _ -> default_type
          end
        | _ -> default_type
        end
      | TypeSingle s0, TypeRange (s1, b1, s2, b2, step) ->
        begin match tbop with
        | TypeAdd -> TypeRange ((SingleExp.eval (SingleBExp (SingleAdd, s0, s1))), b1,
                                (SingleExp.eval (SingleBExp (SingleAdd, s0, s2))), b2, step)
        | TypeSub -> TypeRange ((SingleExp.eval (SingleBExp (SingleSub, s0, s2))), b2,
                                (SingleExp.eval (SingleBExp (SingleSub, s0, s1))), b1, step)
        | TypeMul -> 
          begin match s0 with
          | SingleConst i0 -> 
            if i0 = 0L then TypeSingle (SingleConst 0L)
            else if i0 > 0L then
              TypeRange ((SingleExp.eval (SingleBExp (SingleMul, s0, s1))), b1,
                        (SingleExp.eval (SingleBExp (SingleMul, s0, s2))), b2, Int64.mul step i0)
            else
              TypeRange ((SingleExp.eval (SingleBExp (SingleMul, s0, s2))), b2, 
                        (SingleExp.eval (SingleBExp (SingleMul, s0, s1))), b1, Int64.neg (Int64.mul step i0))
          | _ -> default_type
            (* TypeRange ((eval (SingleBExp (SingleMul, s1, s0))), b1,
                            (eval (SingleBExp (SingleMul, s2, s0))), b2, step) *)
          end
        (* TODO: TypeXor TypeAnd TypeOr *)
        | _ -> default_type
        end
      | TypeRange (s1, b1, s2, b2, step), TypeSingle s0 -> 
        begin match tbop with
        | TypeAdd -> TypeRange ((SingleExp.eval (SingleBExp (SingleAdd, s0, s1))), b1,
                                (SingleExp.eval (SingleBExp (SingleAdd, s0, s2))), b2, step)
        | TypeSub -> TypeRange ((SingleExp.eval (SingleBExp (SingleSub, s1, s0))), b1,
                                (SingleExp.eval (SingleBExp (SingleSub, s2, s0))), b2, step)
        | TypeMul -> 
          begin match s0 with
          | SingleConst i0 -> 
            if i0 = 0L then TypeSingle (SingleConst 0L)
            else if i0 > 0L then
              TypeRange ((SingleExp.eval (SingleBExp (SingleMul, s0, s1))), b1,
                        (SingleExp.eval (SingleBExp (SingleMul, s0, s2))), b2, Int64.mul step i0)
            else
              TypeRange ((SingleExp.eval (SingleBExp (SingleMul, s0, s2))), b2, 
                        (SingleExp.eval (SingleBExp (SingleMul, s0, s1))), b1, Int64.neg (Int64.mul step i0))
          | _ -> default_type
            (* TypeRange ((eval (SingleBExp (SingleMul, s1, s0))), b1,
                            (eval (SingleBExp (SingleMul, s2, s0))), b2, step) *)
          end
        | TypeSal ->
          begin match s0 with
          | SingleConst i0 -> TypeRange ((SingleExp.eval (SingleBExp (SingleSal, s1, s0))), b1,
                                         (SingleExp.eval (SingleBExp (SingleSal, s2, s0))), b2, Int64.shift_left step (Int64.to_int i0))
          | _ -> default_type
          end
        | TypeSar ->
          begin match s0 with
          | SingleConst i0 -> TypeRange ((SingleExp.eval (SingleBExp (SingleSar, s1, s0))), b1,
                                          (SingleExp.eval (SingleBExp (SingleSar, s2, s0))), b2, Int64.shift_right step (Int64.to_int i0))
          | _ -> default_type
          end
        (* TODO: TypeXor TypeAnd TypeOr *)
        | _ -> default_type
        end
      | TypeRange (s1, true, s2, true, step), TypeRange (s1', true, s2', true, step') ->
        begin match tbop with
        | TypeAdd -> 
          TypeRange (
            SingleExp.eval (SingleBExp (SingleAdd, s1, s1')), true,
            SingleExp.eval (SingleBExp (SingleAdd, s2, s2')), true,
            if Int64.rem step step' = 0L then step' 
            else if Int64.rem step' step = 0L then step 
            else type_exp_error ("steps not divide by each other " ^ (Int64.to_string step) ^ " " ^ (Int64.to_string step'))
          )
        | _ -> default_type
        end
      | TypeTop, _ ->
        begin match tbop with
        | TypeInter -> ee2
        | TypeDiff -> TypeUExp (TypeComp, ee2)
        | _ -> TypeTop
        end
      | _, TypeTop ->
        begin match tbop with
        | TypeInter -> ee1
        | TypeDiff -> TypeBot
        | _ -> TypeTop
        end
      | _, TypeSingle s ->
        begin match tbop with
        | TypeSub -> TypeBExp (TypeAdd, ee1, TypeSingle (SingleExp.eval (SingleBExp (SingleExp.SingleMul, SingleConst (-1L), s))))
        | _ -> default_type
        end
      | _ -> default_type
      end
    | TypeUExp (tuop, e) ->
      let ee = eval e in
      let default_type = TypeUExp (tuop, ee) in
      begin match ee with
      | TypeSingle s ->
        begin match tuop with
        | TypeNot -> TypeSingle (SingleExp.eval (SingleUExp (SingleNot, s)))
        | _ -> default_type
        end
      | _ -> default_type
      end
    | _ -> e

  let merge (e1: t) (e2: t) : t option =
    match e1, e2 with
    | TypeSingle s, TypeRange (l, lb, r, rb, step)
    | TypeRange (l, lb, r, rb, step), TypeSingle s ->
      if lb && (SingleExp.cmp (SingleExp.eval (SingleBExp (SingleAdd, s, SingleConst step))) l = 0) then
        Some (TypeRange (s, lb, r, rb, step))
      else if rb && (SingleExp.cmp (SingleExp.eval (SingleBExp (SingleAdd, r, SingleConst step))) s = 0) then
        Some (TypeRange (l, lb, s, rb, step))
      else if SingleExp.cmp s l = 0 then
        Some (TypeRange (l, true, r, rb, step))
      else if SingleExp.cmp r s = 0 then
        Some (TypeRange (l, lb, r, true, step))
      else None
    | _ -> None
  
  let is_val (e: t) : bool =
    match e with
    | TypeSingle _
    | TypeRange _
    | TypeTop
    | TypeBot -> true
    | _ -> false

  let rec repl_type_exp (sol: type_var_id * t) (e: t) : t =
    let idx, re = sol in
    match e with
    | TypeVar v -> if v = idx then re else e
    | TypeBExp (op, l, r) ->
      let l' = repl_type_exp sol l in
      let r' = repl_type_exp sol r in
      eval (TypeBExp (op, l', r'))
    | TypeUExp (op, ee) ->
      let ee' = repl_type_exp sol ee in
      eval (TypeUExp (op, ee'))
    | _ -> e

  let rec string_of_type_exp (t: t) =
    match t with
    | TypeSingle ts -> SingleExp.string_of_single_exp ts
    | TypeRange (bg, bgi, ed, edi, step) -> 
      let bg_str = if bgi then "[" else "(" in
      let ed_str = if edi then "]" else ")" in
      bg_str ^ (SingleExp.string_of_single_exp bg) ^ ", " ^ (SingleExp.string_of_single_exp ed) ^ ed_str ^ " step = " ^ (Int64.to_string step)
    | TypeVar v -> "TypeVar " ^ (string_of_int v)
    | TypeTop -> "Top"
    | TypeBot -> "Bottom"
    | TypeBExp (op, l, r) -> 
      let op_str = match op with
      | TypeAdd -> "Add"
      | TypeSub -> "Sub"
      | TypeMul -> "Mul"
      | TypeSal -> "Sal"
      | TypeShr -> "Shr"
      | TypeSar -> "Sar"
      | TypeXor -> "Xor"
      | TypeAnd -> "And"
      | TypeOr -> "Or"
      | TypeInter -> "Inter"
      | TypeUnion -> "Union"
      | TypeDiff -> "Diff"
      in
      "BinaryExp (" ^ op_str ^ ", " ^ (string_of_type_exp l) ^ ", " ^ (string_of_type_exp r) ^ ")"
    | TypeUExp (op, e) ->
      let op_str = match op with
      | TypeNot -> "Not"
      | TypeComp -> "Comp"
      in
      "UnaryExp (" ^ op_str ^ ", " ^ (string_of_type_exp e) ^ ")"

  let pp_type_exp (lvl: int) (s: t) =
    PP.print_lvl lvl "%s" (string_of_type_exp s)

end
