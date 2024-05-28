open Isa
open Pretty_print

module SingleExp = struct
  exception SingleExpError of string
  let single_exp_error msg = raise (SingleExpError ("[Single Exp Error] " ^ msg))

  module SingleVarSet = Set.Make(Int)

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
    | SingleConst of int64
    | SingleVar of Isa.imm_var_id
    | SingleBExp of single_bop * t * t
    | SingleUExp of single_uop * t

  let rec string_of_single_exp (se: t) =
    match se with
    | SingleConst v -> "Const " ^ (Int64.to_string v)
    | SingleVar v -> "SymImm " ^ (string_of_int v)
    | SingleBExp (op, l, r) -> 
      let op_str = match op with
      | SingleAdd -> "Add"
      | SingleSub -> "Sub"
      | SingleMul -> "Mul"
      | SingleSal -> "Sal"
      | SingleSar -> "Sar"
      | SingleXor -> "Xor"
      | SingleAnd -> "And"
      | SingleOr -> "Or"
      in
      "S-BinaryExp (" ^ op_str ^ ", " ^ (string_of_single_exp l) ^ ", " ^ (string_of_single_exp r) ^ ")"
    | SingleUExp (op, e) ->
      let op_str = match op with
      | SingleNot -> "Not"
      in
      "S-UnaryExp (" ^ op_str ^ ", " ^ (string_of_single_exp e) ^ ")"

  let pp_single_exp (lvl: int) (s: t) =
    PP.print_lvl lvl "%s" (string_of_single_exp s)

  let pp_split_exp (lvl: int) (s: t list list) =
    List.iter (fun x -> 
      PP.print_lvl (lvl + 1) "+\n";
      List.iter (fun y ->
        PP.print_lvl (lvl + 2) "*";
        pp_single_exp (lvl + 2) y;
        Printf.printf "\n"
        ) x;
      ) s

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

  let rec cmp (e1: t) (e2: t) : int =
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
      let c_l = cmp l1 l2 in
      let c_r = cmp r1 r2 in
      if c_bop = 0 then
        if c_l = 0 then c_r
        else c_l
      else c_bop
    | SingleBExp _, _ -> 1
    | SingleUExp (uop1, l1), SingleUExp (uop2, l2) -> 
      let c_uop = cmp_single_uop uop1 uop2 in
      let c_l = cmp l1 l2 in
      if c_uop = 0 then c_l else c_uop
    | SingleUExp _, SingleBExp _ -> -1
    | SingleUExp _, _ -> 1

  let rec cmp_list_helper (e1: t list) (e2: t list) : int =
    match e1, e2 with
    | [], [] -> 0
    | [], _ :: _ -> -1
    | _ :: _, [] -> 1
    | hd1 :: tl1, hd2 :: tl2 ->
      let cmp_hd = cmp hd1 hd2 in
      if cmp_hd = 0 then cmp_list_helper tl1 tl2 else cmp_hd

  let cmp_list (e1: t list) (e2: t list) : int =
    match e1, e2 with
    | (SingleConst _) :: ee1, (SingleConst _) :: ee2
    | SingleConst _ :: ee1, ee2
    | ee1, SingleConst _ :: ee2
    | ee1, ee2 -> cmp_list_helper ee1 ee2

  let convert_t (e: t list list) : t =
    let rec helper_mul (x: t list) =
      match x with
      | [] -> SingleConst 0L
      | hd :: [] -> hd
      | hd :: tl -> SingleBExp (SingleMul, helper_mul tl, hd)
    in
    let rec helper_add (x: t list) =
      match x with
      | [] -> SingleConst 0L
      | hd :: [] -> hd
      | hd :: tl -> SingleBExp (SingleAdd, helper_add tl, hd)
    in
    helper_add (List.map helper_mul e)

  let mul_const (coeff: int64) (e: t list) : t list =
    match e with
    | [] -> []
    | (SingleConst c) :: tl -> (SingleConst (Int64.mul coeff c)) :: tl
    | tl -> SingleConst coeff :: tl

  let mul_t (e1: t list) (e2: t list) : t list =
    let x = List.sort cmp (e1 @ e2) in
    match x with
    | SingleConst c1 :: SingleConst c2 :: tl ->
      let c12 = Int64.mul c1 c2 in
      if c12 = 0L then []
      else if c12 = 1L then tl
      else SingleConst c12 :: tl
    | SingleConst c :: _ ->
      if c = 0L then [] else x
    | _ -> x

  let add_t (e1: t list) (e2: t list) : (t list, bool) Either.t =
    (* Printf.printf "=======================\n@@@";
    pp_split_exp 0 [e1; e2];
    Printf.printf "\n=======================\n"; *)
    (* let equal (l: t) (r: t) : bool = if cmp l r = 0 then true else false in *)
    match e1, e2 with
    | [], [] -> Either.left []
    | [], hd :: tl | hd :: tl, [] -> Either.left (hd :: tl)
    | [SingleConst 0L], e | e, [SingleConst 0L] -> Either.left e
    | SingleConst c1 :: tl1, SingleConst c2 :: tl2 ->
      let cmp_tl = cmp_list_helper tl1 tl2 in
      if cmp_tl = 0 then 
        let c12 = Int64.add c1 c2 in
        if c12 = 0L then Either.left []
        else Either.left (SingleConst c12 :: tl1)
      else if cmp_tl = 1 then Either.right true
      else Either.right false
      (* if List.equal equal tl1 tl2 
      then Some (SingleConst (c1 + c2) :: tl1)
      else None *)
    | SingleConst c :: tl1, tl2 | tl1, SingleConst c :: tl2 ->
      let cmp_tl = cmp_list_helper tl1 tl2 in
      if cmp_tl = 0 then 
        let c1 = Int64.add c 1L in
        if c1 = 0L then Either.left []
        else Either.left (SingleConst (Int64.add c 1L) :: tl1)
      else if cmp_tl = 1 then Either.right true
      else Either.right false
      (* if List.equal equal tl1 tl2 
      then Some (SingleConst (c + 1) :: tl1)
      else None *)
    | _, _ ->
      let cmp_tl = cmp_list_helper e1 e2 in
      if cmp_tl = 0 then Either.left (SingleConst 2L :: e1)
      else if cmp_tl = 1 then Either.right true
      else Either.right false
    (* | hd1 :: tl1, hd2 :: tl2 ->
      if List.equal equal (hd1 :: tl1) (hd2 :: tl2) 
      then Some (SingleConst 2 :: hd1 :: tl1)
      else None *)

  let add_t_list (e1: t list list) (e2: t list list) : t list list =
    let rec helper (x_list: t list list) (y: t list) : t list list =
      match x_list with
      | [] -> [ y ]
      | hd :: tl ->
        begin match add_t y hd with
        | Left [] -> tl
        | Left e -> e :: tl
        | Right larger ->
          if larger then hd :: (helper tl y)
          else y :: hd :: tl
        end
    in
    (* let helper0 (acc: (t list) option) (x: t list) : (t list) option * t list =
      match acc with
      | None -> (None, x)
      | Some v ->
        begin match add_t v x with
        | Some e -> (None, e)
        | None -> (acc, x)
        end
    in
    let helper (acc: t list list) (y: t list) : t list list =
      let new_y, new_x = List.fold_left_map helper0 (Some y) acc in
      match new_y with
      | Some v -> v :: new_x
      | None -> new_x
    in *)
    List.fold_left helper e1 e2

  let rec eval_t (e: t) : t list list =
    match e with
    | SingleBExp (SingleAdd, l, r) ->
      add_t_list (eval_t l) (eval_t r)
    | SingleBExp (SingleSub, l, r) ->
      let ul = eval_t l in
      let ur = eval_t r in
      let neg_ur = List.map (mul_const (-1L)) ur in
      add_t_list ul neg_ur
    | SingleBExp (SingleMul, l, r) ->
      let ul = eval_t l in
      let ur = eval_t r in
      let helper (x: t list) : t list list =
        List.map (mul_t x) ur
      in
      let helper2 (acc: t list list) (x: t list) : t list list =
        add_t_list acc (helper x)
      in
      List.fold_left helper2 [] ul
    | SingleBExp (SingleSal, l, r) ->
      let ul = eval_t l in
      let ur = eval_t r in
      begin match convert_t ur with
      | SingleConst c -> List.map (mul_const (Int64.shift_left 1L (Int64.to_int c))) ul
      | merged_r -> [ [SingleBExp (SingleSal, convert_t ul, merged_r)] ]
      end
    | SingleBExp (bop, l, r) ->
      let eval_l = convert_t (eval_t l) in
      let eval_r = convert_t (eval_t r) in
      begin match eval_l, eval_r with
      | SingleConst v1, SingleConst v2 ->
        let x = 
          begin match bop with
          | SingleAdd -> SingleConst (Int64.add v1 v2)
          | SingleSub -> SingleConst (Int64.sub v1 v2)
          | SingleMul -> SingleConst (Int64.mul v1 v2) (* These three cases are not needed here *)
          | SingleSal -> SingleConst (Int64.shift_left v1 (Int64.to_int v2))
          | SingleSar -> SingleConst (Int64.shift_right v1 (Int64.to_int v2))
          | SingleXor -> SingleConst (Int64.logxor v1 v2)
          | SingleAnd -> SingleConst (Int64.logand v1 v2)
          | SingleOr -> SingleConst (Int64.logor v1 v2)
          end
        in [ [x] ]
      | _ -> [ [SingleBExp (bop, convert_t (eval_t l), convert_t (eval_t r))] ]
      end
    | SingleUExp (uop, l) ->
      [ [SingleUExp (uop, convert_t (eval_t l))] ]
    | _ -> [ [e] ]

  let eval (e: t) : t =
    (* Printf.printf "====================\n";
    pp_single_exp 0 e;
    Printf.printf "\n--------------------\n";
    pp_split_exp 0 (eval_t e);
    Printf.printf "\n--------------------\n";
    pp_single_exp 0 (convert_t (eval_t e));
    Printf.printf "\n====================\n"; *)
    convert_t (eval_t e)

  let must_ge (e1: t) (e2: t) : bool =
    match eval (SingleBExp (SingleSub, e1, e2)) with
    | SingleConst diff ->
      if diff >= 0L then true else false
    | _ -> false

  let get_greater (e1: t) (e2: t) : t option =
    match eval (SingleBExp (SingleSub, e1, e2)) with
    | SingleConst diff ->
      if diff >= 0L then Some e1 else Some e2
    | _ -> None

  let get_less (e1: t) (e2: t) : t option =
    match eval (SingleBExp (SingleSub, e1, e2)) with
    | SingleConst diff ->
      if diff <= 0L then Some e1 else Some e2
    | _ -> None

end