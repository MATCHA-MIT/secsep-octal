open Isa_basic
open Single_exp_basic
open Smt_emitter
open Pretty_print
open Sexplib.Std
open Sexplib

module SingleExp = struct
include SingleExpBasic

  type local_var_map_t = (IsaBasic.imm_var_id * t) list
  [@@deriving sexp]

  let get_empty_var_map : local_var_map_t = []

  let get_empty_var_map_from_init_single_var_map (map: local_var_map_t) : local_var_map_t = map

  let rec to_string (se: t) =
    match se with
    | SingleTop -> "Top"
    | SingleConst v -> "Const " ^ (Int64.to_string v)
    | SingleVar v -> "SymImm " ^ (string_of_int v)
    | SingleBExp (op, l, r) -> 
      let op_str = match op with
      | SingleAdd -> "Add"
      | SingleSub -> "Sub"
      | SingleMul -> "Mul"
      | SingleSal -> "Sal"
      | SingleSar -> "Sar"
      | SingleShr -> "Shr"
      | SingleXor -> "Xor"
      | SingleAnd -> "And"
      | SingleOr -> "Or"
      | SingleMod -> "Mod"
      in
      "S-BinaryExp (" ^ op_str ^ ", " ^ (to_string l) ^ ", " ^ (to_string r) ^ ")"
    | SingleUExp (op, e) ->
      let op_str = match op with
      | SingleNot -> "Not"
      in
      "S-UnaryExp (" ^ op_str ^ ", " ^ (to_string e) ^ ")"
    | SingleITE ((cond, cond_l_exp, cond_r_exp), then_exp, else_exp) ->
      let cond_str = CondTypeBase.to_string cond in
      Printf.sprintf "S-ITE ((%s, %s, %s), %s, %s)"
        cond_str (to_string cond_l_exp) (to_string cond_r_exp)
        (to_string then_exp) (to_string else_exp)

  let rec to_ocaml_string (se: t) =
    match se with
    | SingleTop -> "SingleTop"
    | SingleConst v -> Printf.sprintf "SingleConst (%LdL)" v
    | SingleVar v -> Printf.sprintf "SingleVar (%d)" v
    | SingleBExp (op, l, r) -> 
      let op_str = match op with
      | SingleAdd -> "SingleAdd"
      | SingleSub -> "SingleSub"
      | SingleMul -> "SingleMul"
      | SingleSal -> "SingleSal"
      | SingleSar -> "SingleSar"
      | SingleShr -> "SingleShr"
      | SingleXor -> "SingleXor"
      | SingleAnd -> "SingleAnd"
      | SingleOr -> "SingleOr"
      | SingleMod -> "SingleMod"
      in
      Printf.sprintf "SingleBExp (%s, %s, %s)" op_str (to_ocaml_string l) (to_ocaml_string r)
    | SingleUExp (op, e) ->
      let op_str = match op with
      | SingleNot -> "SingleNot"
      in
      Printf.sprintf "SingleUExp (%s, %s)" op_str (to_ocaml_string e)
    | SingleITE ((cond, cond_l_exp, cond_r_exp), then_exp, else_exp) ->
      let cond_str = CondTypeBase.to_string cond in
      Printf.sprintf "SingleITE ((%s, %s, %s), %s, %s)"
        cond_str (to_ocaml_string cond_l_exp) (to_ocaml_string cond_r_exp)
        (to_ocaml_string then_exp) (to_ocaml_string else_exp)

  let empty_var_map_to_ocaml_string = "[]"

  let var_set_to_ocaml_string (var_set: SingleVarSet.t) : string =
    Printf.sprintf "SingleExp.SingleVarSet.of_list [%s]"
      (String.concat "; " (List.map string_of_int (SingleVarSet.elements var_set)))

  let pp_single_exp (lvl: int) (s: t) =
    PP.print_lvl lvl "%s" (to_string s)

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
    | SingleSar, SingleMod
    | SingleSar, SingleOr
    | SingleSar, SingleAnd
    | SingleSar, SingleXor
    | SingleSar, SingleShr -> -1
    | SingleSar, SingleSar -> 0
    | SingleSar, _ -> 1
    | SingleShr, SingleMod
    | SingleShr, SingleOr
    | SingleShr, SingleAnd -> -1
    | SingleShr, SingleXor -> -1
    | SingleShr, SingleShr -> 0
    | SingleShr, _ -> 1
    | SingleXor, SingleMod
    | SingleXor, SingleOr
    | SingleXor, SingleAnd -> -1
    | SingleXor, SingleXor -> 0
    | SingleXor, _ -> 1
    | SingleAnd, SingleMod
    | SingleAnd, SingleOr -> -1
    | SingleAnd, SingleAnd -> 0
    | SingleAnd, _ -> 1
    | SingleOr, SingleMod -> -1
    | SingleOr, SingleOr -> 0
    | SingleOr, _ -> 1
    | SingleMod, SingleMod -> 0
    | SingleMod, _ -> 1

  let cmp_single_uop (op1: single_uop) (op2: single_uop) : int =
    match op1, op2 with
    | SingleNot, SingleNot -> 0

  let rec cmp (e1: t) (e2: t) : int =
    match e1, e2 with
    | SingleTop, SingleTop -> 0
    | SingleTop, _ -> -1
    | SingleConst _, SingleTop -> 1
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
    | SingleITE ((cond1, cond1_l, cond1_r), then1, else1), SingleITE ((cond2, cond2_l, cond2_r), then2, else2) ->
      let c_cond = CondTypeBase.compare cond1 cond2 in
      let c_cond_l = cmp cond1_l cond2_l in
      let c_cond_r = cmp cond1_r cond2_r in
      let c_cond =
        if c_cond = 0 then
          if c_cond_l = 0 then c_cond_r
          else c_cond_l
        else c_cond
      in
      if c_cond = 0 then
        let c_then = cmp then1 then2 in
        if c_then = 0 then cmp else1 else2 else c_then
      else c_cond
    | SingleITE _, _ -> 1

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

  let find_top (x: t list) : bool =
    List.find_opt (fun a -> a = SingleTop) x <> None

  let find_top_double (x: t list list) : bool =
    List.find_opt (fun a -> find_top a) x <> None

  let convert_t (e: t list list) : t =
    let rec helper_mul (x: t list) =
      if find_top x then SingleTop
      else
        match x with
        | [] -> SingleConst 0L
        | hd :: [] -> hd
        | hd :: tl -> SingleBExp (SingleMul, helper_mul tl, hd)
    in
    let rec helper_add (x: t list) =
      if find_top x then SingleTop
      else
        match x with
        | [] -> SingleConst 0L
        | hd :: [] -> hd
        | hd :: tl -> SingleBExp (SingleAdd, helper_add tl, hd)
    in
    helper_add (List.map helper_mul e)

  let mul_const (coeff: int64) (e: t list) : t list =
    if find_top e then [ SingleTop ]
    else
      match e with
      | [] -> []
      | (SingleConst c) :: tl -> (SingleConst (Int64.mul coeff c)) :: tl
      | tl -> SingleConst coeff :: tl

  let mul_t (e1: t list) (e2: t list) : t list =
    if find_top e1 || find_top e2 then [ SingleTop ]
    else
      let x = List.sort cmp (e1 @ e2) in
      match x with
      | SingleConst c1 :: SingleConst c2 :: tl ->
        let c12 = Int64.mul c1 c2 in
        if c12 = 0L then []
        else if c12 = 1L then
          if List.is_empty tl then [ SingleConst 1L ] else tl
        else SingleConst c12 :: tl
      | SingleConst c :: tl ->
        if c = 0L then []
        else if c = 1L then 
          if List.is_empty tl then [ SingleConst 1L ] else tl
        else x
      | _ -> x

  let add_t (e1: t list) (e2: t list) : (t list, bool) Either.t =
    (* Printf.printf "=======================\n@@@";
    pp_split_exp 0 [e1; e2];
    Printf.printf "\n=======================\n"; *)
    (* let equal (l: t) (r: t) : bool = if cmp l r = 0 then true else false in *)
    if find_top e1 || find_top e2 then Left [ SingleTop ]
    else
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
    if find_top_double e1 || find_top_double e2 then [[ SingleTop ]]
    else
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
    | SingleBExp (_, SingleTop, _)
    | SingleBExp (_, _, SingleTop)
    | SingleUExp (_, SingleTop) -> [[ SingleTop ]]
    | SingleBExp (SingleMul, SingleBExp (SingleSar, e0, SingleConst shift), SingleConst mul) ->
      if shift >= 0L && shift < 64L && Int64.shift_left 1L (Int64.to_int shift) = mul then
        eval_t (SingleBExp (SingleAnd, e0, SingleConst (Int64.neg mul)))
      else
        [[SingleBExp (SingleMul, SingleBExp (SingleSar, convert_t (eval_t e0), SingleConst shift), SingleConst mul)]]
    | SingleBExp (SingleSal, SingleBExp (SingleSar, e0, SingleConst shift1), SingleConst shift2) ->
      if shift1 >= 0L && shift1 < 64L && shift1 = shift2 then
        eval_t (SingleBExp (SingleAnd, e0, SingleConst (Int64.neg (Int64.shift_left 1L (Int64.to_int shift1)))))
      else
        [[SingleBExp (SingleSal, SingleBExp (SingleSar, convert_t (eval_t e0), SingleConst shift1), SingleConst shift2)]]
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
          | SingleShr -> SingleConst (Int64.shift_right_logical v1 (Int64.to_int v2))
          | SingleXor -> SingleConst (Int64.logxor v1 v2)
          | SingleAnd -> SingleConst (Int64.logand v1 v2)
          | SingleOr -> SingleConst (Int64.logor v1 v2)
          | SingleMod -> SingleConst (Int64.rem v1 v2)
          end
        in [ [x] ]
      | _ -> 
        if cmp eval_l eval_r = 0 && cmp eval_l SingleTop <> 0 then
          begin match bop with
          | SingleXor -> [ [SingleConst 0L] ]
          | SingleAnd | SingleOr -> [ [eval_l] ]
          | _ -> [ [SingleBExp (bop, eval_l, eval_r)] ]
          end
        else
          [ [SingleBExp (bop, eval_l, eval_r)] ]
      end
    | SingleUExp (uop, l) ->
      [ [SingleUExp (uop, convert_t (eval_t l))] ]
    | SingleITE ((cond, cl, cr), l, r) ->
      [ [SingleITE ((cond, cl |> eval_t |> convert_t, cr |> eval_t |> convert_t), l |> eval_t |> convert_t, r |> eval_t |> convert_t)] ]
    | SingleTop | SingleConst _ | SingleVar _ -> [ [e] ]

  let rec reorder_ite (e: t) : t =
    (* NOTE: this is a simple but inefficient implementation. If the exp to be eval is too complex, consider to improve it!!! *)
    match e with
    | SingleTop | SingleConst _ | SingleVar _ -> e
    | SingleBExp (op, e1, e2) ->
      let e1 = reorder_ite e1 in
      let e2 = reorder_ite e2 in
      begin match e1, e2 with
      | SingleITE (cond1, cl1, cr1), _ ->
        SingleITE (cond1, reorder_ite (SingleBExp (op, cl1, e2)), reorder_ite (SingleBExp (op, cr1, e2)))
      | _, SingleITE (cond2, cl2, cr2) ->
        SingleITE (cond2, reorder_ite (SingleBExp (op, e1, cl2)), reorder_ite (SingleBExp (op, e1, cr2)))
      | _ -> e
      end
    | SingleUExp (op, e0) ->
      let e0 = reorder_ite e0 in
      begin match e0 with
      | SingleITE (cond, cl, cr) ->
        SingleITE (cond, reorder_ite (SingleUExp (op, cl)), reorder_ite (SingleUExp (op, cr)))
      | _ -> e
      end
    | SingleITE (cond, cl, cr) ->
      SingleITE (cond, reorder_ite cl, reorder_ite cr)

  let eval (e: t) : t =
    (* Printf.printf "====================\n";
    pp_single_exp 0 e;
    Printf.printf "\n--------------------\n";
    pp_split_exp 0 (eval_t e);
    Printf.printf "\n--------------------\n";
    pp_single_exp 0 (convert_t (eval_t e));
    Printf.printf "\n====================\n"; *)
    (* convert_t (eval_t (e)) *)
    e |> reorder_ite |> eval_t |> convert_t

  let get_align (align_map: (int * int64) list) (e: t) : int64 = 
    let rec helper (e: t) : int64 =
      match e with
      | SingleConst num ->
        List.find (
          fun x -> Int64.rem num x = 0L
        ) [256L; 128L; 64L; 32L; 16L; 8L; 4L; 2L; 1L]
        (* if Int64.rem num 16L = 0L then 16L *)
        (* if Int64.rem num 8L = 0L then 8L
        else if Int64.rem num 4L = 0L then 4L
        else if Int64.rem num 2L = 0L then 2L
        else 1L *)
      | SingleVar v ->
        begin match List.find_map (fun (idx, idx_align) -> if idx = v then Some idx_align else None) align_map with
        | Some v_align -> v_align
        | None -> 1L
        end
      | SingleBExp (SingleAdd, e1, e2) | SingleBExp (SingleSub, e1, e2) ->
        Int64.min (helper e1) (helper e2)
      | SingleBExp (SingleMul, e1, e2) ->
        Int64.mul (helper e1) (helper e2)
      | SingleBExp (SingleAnd, _, SingleConst c) | SingleBExp (SingleAnd, SingleConst c, _) ->
        helper (SingleConst (Int64.neg c))
      | SingleITE (_, l, r) ->
        Int64.min (helper l) (helper r)
      | SingleTop | SingleBExp _ | SingleUExp _ -> 1L
    in
    helper e  

  let eval_align (align_map: (int * int64) list) (e: t) : t =
    let is_and_num_align (c: int64) : bool =
      c = -2L || c = -4L || c = -8L || c = -16L
    in
    let rec helper (e: t) : t =
      match e with
      | SingleBExp (SingleAnd, (SingleBExp (SingleAdd, e1, e2)), SingleConst c) 
      | SingleBExp (SingleAnd, SingleConst c, (SingleBExp (SingleAdd, e1, e2))) ->
        if is_and_num_align c then
          let align = Int64.neg c in
          let is_e1_aligned = Int64.rem (get_align align_map e1) align = 0L in
          let is_e2_aligned = Int64.rem (get_align align_map e2) align = 0L in
          if is_e1_aligned && is_e2_aligned then
            SingleBExp (SingleAdd, helper e1, helper e2)
          else if is_e1_aligned then
            SingleBExp (SingleAdd, helper e1, helper (SingleBExp (SingleAnd, e2, SingleConst c)))
          else if is_e2_aligned then
            SingleBExp (SingleAdd, helper (SingleBExp (SingleAnd, e1, SingleConst c)), helper e2)
          else
            SingleBExp (SingleAnd, (SingleBExp (SingleAdd, helper e1, helper e2)), SingleConst c)
        else SingleBExp (SingleAnd, (SingleBExp (SingleAdd, helper e1, helper e2)), SingleConst c)
      | SingleBExp (SingleAnd, e, SingleConst c)
      | SingleBExp (SingleAnd, SingleConst c, e) ->
        if is_and_num_align c then
          if Int64.rem (get_align align_map e) (Int64.neg c) = 0L then
            helper e
          else SingleBExp (SingleAnd, helper e, SingleConst c)
        else SingleBExp (SingleAnd, helper e, SingleConst c)
      | SingleBExp (bop, e1, e2) -> SingleBExp (bop, helper e1, helper e2)
      | SingleUExp (uop, e1) -> SingleUExp (uop, helper e1)
      | SingleITE ((cond, cl, cr), l, r) -> SingleITE ((cond, helper cl, helper cr), helper l, helper r)
      | SingleTop | SingleConst _ | SingleVar _ -> e
    in
    eval (helper e)

  let rec try_eval_div (e: t) (div: int64) : t option =
    if div = 1L then Some e else
    match e with
    | SingleConst c -> 
      if Int64.rem c div = 0L then 
        Some (SingleConst (Int64.div c div))
      else None
    | SingleBExp (SingleMul, e', SingleConst c)
    | SingleBExp (SingleMul, SingleConst c, e') ->
      if Int64.rem c div = 0L then 
        Some (SingleBExp (SingleMul, e', SingleConst (Int64.div c div)))
      else None
    | SingleBExp (bop, e1, e2) ->
      if bop = SingleAdd || bop = SingleSub then begin
        match try_eval_div e1 div, try_eval_div e2 div with
        | Some e1', Some e2' -> Some (SingleBExp (bop, e1', e2'))
        | _ -> None
      end else None
    | SingleITE ((cond, cond_l, cond_r), l, r) -> begin
        match try_eval_div l div, try_eval_div r div with
        | Some l', Some r' ->
          Some (SingleITE ((cond, cond_l, cond_r), l', r'))
        | _ -> None
      end
    | SingleTop | SingleVar _ | SingleUExp _ -> None

  let is_div (e: t) (div: int64) : bool =
    if div = 1L then true
    else if Int64.rem (get_align [] e) div = 0L then true
    else try_eval_div e div <> None

  let update_local_var (map: local_var_map_t) (e: t) (pc: int) : (local_var_map_t * t) =
    match e with
    | SingleBExp _ | SingleUExp _ | SingleITE _ ->
      let new_idx = -pc in
      (new_idx, e) :: map, SingleVar new_idx
    | SingleTop | SingleConst _ | SingleVar _ -> map, e

  let add_local_var (map: local_var_map_t) (e1: t) (e2: t) : local_var_map_t =
    match e1 with
    | SingleVar v -> 
      begin match List.find_opt (fun (idx, _) -> idx = v) map with
      | Some (_, e) -> 
        if cmp e e2 = 0 then map 
        else 
          single_exp_error (Printf.sprintf "add_local var conflict on var %d exp %s and %s" v (to_string e) (to_string e2)) 
      | None -> (v, e2) :: map
      end
    | SingleTop -> map
    | SingleConst _ | SingleBExp _ | SingleUExp _ | SingleITE _ -> single_exp_error (Printf.sprintf "add_local_var cannot add %s->%s" (to_string e1) (to_string e2))

  let add_local_var_simp (simp_func: t -> t) (map: local_var_map_t) (e1: t) (e2: t) : local_var_map_t =
    match e1 with
    | SingleVar v -> 
      let e2 = simp_func e2 in
      begin match List.find_opt (fun (idx, _) -> idx = v) map with
      | Some (_, e) -> 
        if cmp e e2 = 0 then map 
        else 
          single_exp_error (Printf.sprintf "add_context_map conflict on var %d exp %s and %s" v (to_string e) (to_string e2)) 
      | None -> (v, e2) :: map
      end
    | SingleTop -> map
    | SingleConst _ | SingleBExp _ | SingleUExp _ | SingleITE _ -> single_exp_error (Printf.sprintf "add_context_map cannot add %s->%s" (to_string e1) (to_string e2))

  let add_local_global_var (map: local_var_map_t) (global_var: SingleVarSet.t) : local_var_map_t =
    List.fold_left (
      fun acc var -> 
        add_local_var acc (SingleVar var) (SingleVar var)
    ) map (SingleVarSet.to_list global_var)

  let pp_local_var (lvl: int) (map: local_var_map_t) : unit =
    PP.print_lvl lvl "<Single var map>\n";
    List.iter (
      fun (x, e) -> PP.print_lvl (lvl + 1) "%d -> %s\n" x (to_string e)
    ) map

  let find_local_var_map (map: local_var_map_t) (idx: int) : t option =
    List.find_map (fun (i, e) -> if i = idx then Some e else None) map

  let repl_local_var (map: local_var_map_t) (e: t) : t =
    (* Only applied to repl local var with block vars *)
    (* Recursive repl; if not found, leave the var there *)
    let rec repl_helper (e: t) : t =
      match e with
      | SingleTop | SingleConst _ -> e
      | SingleVar v ->
        (* if only_repl_local && v > 0 then (* input/block var *)
           e (* Here is dirty since it will also lookup global var *)
        else  *)
        begin (* TODO: Double check this change!!! *)
          match find_local_var_map map v with
          | Some (SingleVar v') -> (* Prevent infinite rec call *)
            if v = v' then SingleVar v' else repl_helper (SingleVar v')
          | Some e -> repl_helper e
          | None -> e
        end
      | SingleBExp (bop, e1, e2) ->
        let e1 = repl_helper e1 in
        let e2 = repl_helper e2 in
        eval (SingleBExp (bop, e1, e2))
      | SingleUExp (uop, e) ->
        let e = repl_helper e in
        eval (SingleUExp (uop, e))
      | SingleITE ((cond, cond_l, cond_r), then_exp, else_exp) ->
        let cond_l = repl_helper cond_l in
        let cond_r = repl_helper cond_r in
        let then_exp = repl_helper then_exp in
        let else_exp = repl_helper else_exp in
        eval (SingleITE ((cond, cond_l, cond_r), then_exp, else_exp))
    in
    repl_helper e

  let repl_context_var (map: local_var_map_t) (e: t) : t =
    (* Only applied to repl var from one context with var from the other context *)
    (* Non-recursive repl; if not found, return Top *)
    let rec repl_helper (e: t) : t =
      match e with
      | SingleTop | SingleConst _ -> e
      | SingleVar v ->
        begin match find_local_var_map map v with
        | Some e -> e
        | None -> SingleTop
        end
      | SingleBExp (bop, e1, e2) ->
        eval (SingleBExp (bop, repl_helper e1, repl_helper e2))
      | SingleUExp (uop, e) ->
        eval (SingleUExp (uop, repl_helper e))
      | SingleITE ((cond, cond_l, cond_r), then_exp, else_exp) ->
        eval (SingleITE ((cond, repl_helper cond_l, repl_helper cond_r), repl_helper then_exp, repl_helper else_exp))
    in
    repl_helper e

  let repl_var (map: local_var_map_t) (e: t) : t =
    (* Non-recursive repl; if not found, leave the var there *)
    let rec repl_helper (e: t) : t =
      match e with
      | SingleTop | SingleConst _ -> e
      | SingleVar v ->
        begin match find_local_var_map map v with
        | Some e -> e
        | None -> e
        end
      | SingleBExp (bop, e1, e2) ->
        eval (SingleBExp (bop, repl_helper e1, repl_helper e2))
      | SingleUExp (uop, e) ->
        eval (SingleUExp (uop, repl_helper e))
      | SingleITE ((cond, cond_l, cond_r), then_exp, else_exp) ->
        eval (SingleITE ((cond, repl_helper cond_l, repl_helper cond_r), repl_helper then_exp, repl_helper else_exp))
    in
    repl_helper e

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

  let rec get_vars (e: t) : SingleVarSet.t =
    match e with
    | SingleVar x -> SingleVarSet.singleton x
    | SingleBExp (_, e1, e2) -> SingleVarSet.union (get_vars e1) (get_vars e2)
    | SingleUExp (_, e) -> get_vars e
    | SingleITE ((_, cond_l, cond_r), then_exp, else_exp) ->
      let s1 = get_vars cond_l in
      let s2 = get_vars cond_r in
      let s3 = get_vars then_exp in
      let s4 = get_vars else_exp in
      SingleVarSet.union (SingleVarSet.union s1 s2) (SingleVarSet.union s3 s4)
    | SingleTop | SingleConst _  -> SingleVarSet.empty

  let is_val (global_var: SingleVarSet.t) (e: t) : bool =
    SingleVarSet.is_empty (SingleVarSet.diff (get_vars e) global_var)

  let get_mapped_var_set (var_map: local_var_map_t) : SingleVarSet.t =
    List.map fst var_map |>  SingleVarSet.of_list

  let is_val2 (var_map: local_var_map_t) (e: t) : bool =
    let var_set = get_mapped_var_set var_map in
    is_val var_set e

  let rec repl_var_exp (e: t) (v_idx_exp: IsaBasic.imm_var_id * t) : t =
    let idx, v_exp = v_idx_exp in
    match e with
    | SingleVar v -> if v = idx then v_exp else e
    | SingleBExp (bop, e1, e2) -> SingleBExp (bop, repl_var_exp e1 v_idx_exp, repl_var_exp e2 v_idx_exp)
    | SingleUExp (uop, e) -> SingleUExp (uop, repl_var_exp e v_idx_exp)
    | SingleITE ((cond, cond_l, cond_r), l, r) ->
      SingleITE ((cond, repl_var_exp cond_l v_idx_exp, repl_var_exp cond_r v_idx_exp), 
                 repl_var_exp l v_idx_exp, 
                 repl_var_exp r v_idx_exp)
    | SingleTop | SingleConst _ -> e

  let rec get_imm_type (i: IsaBasic.immediate) : t =
    match i with
    | ImmNum (v, _) -> SingleConst v
    | ImmLabel (v, _) -> SingleVar v
    | ImmBExp ((i1, i2), _) -> SingleBExp (SingleAdd, get_imm_type i1, get_imm_type i2)

  let get_mem_op_type
      (disp: IsaBasic.immediate option) (base: t option)
      (index: t option) (scale: int64) : t =
    let disp_type = 
      match disp with
      | Some d -> get_imm_type d
      | None -> get_imm_type (ImmNum (0L, None))
    in
    let base_type =
      match base with
      | Some b -> b
      | None -> get_imm_type (ImmNum (0L, None))
    in
    let index_type =
      match index with
      | Some i -> i
      | None -> get_imm_type (ImmNum (0L, None))
    in
    eval (
      SingleBExp (SingleAdd,
        SingleBExp (SingleAdd, base_type, disp_type),
        SingleBExp (SingleMul, index_type, SingleConst scale)
      )
    )

  let rec filter_single_var (addr: t) : SingleVarSet.t =
    match addr with
    | SingleVar x -> SingleVarSet.singleton x
    | SingleBExp (SingleAdd, l, r) ->
      let left_ptr = filter_single_var l in
      let right_ptr = filter_single_var r in
      SingleVarSet.union left_ptr right_ptr
    | SingleTop | SingleConst _ | SingleBExp _ | SingleUExp _ | SingleITE _ -> SingleVarSet.empty

  let find_base (e: t) (ptr_set: SingleVarSet.t) : IsaBasic.imm_var_id option =
    let p_set = filter_single_var e in
    match SingleVarSet.to_list (SingleVarSet.inter ptr_set p_set) with
    | [] -> None
    | hd :: [] -> Some hd
    | _ -> None

  let find_base_adv 
      (sub_sol_to_list_func: t -> ((t * t) list) option)
      (e: t) (ptr_set: SingleVarSet.t) : IsaBasic.imm_var_id option =
    let base_opt = find_base e ptr_set in 
    if base_opt <> None then
      base_opt
    else
      match sub_sol_to_list_func e with
      | Some e_list -> 
        let p_set =
          List.fold_left (
            fun (acc: SingleVarSet.t) (e, _) ->
              let p_set = filter_single_var e in
              SingleVarSet.inter acc p_set
          ) ptr_set e_list
        in
        begin match SingleVarSet.to_list p_set with
        | [] -> None
        | hd :: [] -> Some hd
        | _ -> single_exp_error (Printf.sprintf "find_base find more than one base for %s" (to_string e))
        end
      | None -> None

  let get_single_exp (e: t) : t = e

  let to_smt_expr ?(get_var_size: (int -> int option) option = None) (smt_ctx: SmtEmitter.t) (e: t) : SmtEmitter.exp_t = 
    SmtEmitter.expr_of_single_exp ~get_var_size:get_var_size smt_ctx e false

  let match_const_offset (o: t) (base_id: IsaBasic.imm_var_id) : int64 option =
    match o with
    | SingleVar var when var = base_id -> Some 0L
    | SingleBExp (SingleAdd, SingleVar var, SingleConst off) when var = base_id -> Some off
    | SingleBExp (SingleAdd, SingleConst off, SingleVar var) when var = base_id -> Some off
    | SingleTop | SingleConst _ | SingleVar _ | SingleBExp _ | SingleUExp _ | SingleITE _ -> None

  let split_val (e: t) (off_list: (t * t) list) : t list =
    (* We do not check the off list is continuous here since we checked before we call split_val *)
    let get_len_helper (acc: int64 option) (off: t * t) : (int64 option) * int64 =
      match acc with
      | None -> None, 0L
      | Some total_len ->
        let l, r = off in
        match eval (SingleBExp (SingleSub, r, l)) with
        | SingleConst c -> Some (Int64.add c total_len), c
        | _ -> None, 0L
    in
    let get_default_list (default: t) = List.init (List.length off_list) (fun _ -> default) in
    let get_byte_helper (sign_extension: bool) (acc: int64) (size: int64) : int64 * t =
      (* We assume size refers to byte and is smaller than 8 *)
      let bits = (Int64.to_int size) * 8 in
      if bits < 0 || bits >= 64 then single_exp_error (Printf.sprintf "split val got invalid size %Ld" size)
      else
      let left_bits = 64 - bits in
      let part_val = 
        if sign_extension then Int64.shift_right (Int64.shift_left acc left_bits) left_bits
        else Int64.shift_right_logical (Int64.shift_left acc left_bits) left_bits
      in
      let remained_val = Int64.shift_right acc bits in
      remained_val, SingleConst part_val
    in
    match e with
    | SingleConst const_val ->
      begin match List.fold_left_map get_len_helper (Some 0L) off_list with
      | None, _ -> get_default_list SingleTop
      | Some total_len, len_list ->
        if total_len > 8L then (* total_len is in bytes *)
          if const_val = 0L then get_default_list (SingleConst 0L)
          else get_default_list SingleTop
        else
          (* TODO: Decide whether to use signed extension or not later!!! *)
          let _, val_list = List.fold_left_map (get_byte_helper true) const_val len_list in
          val_list
      end
    | _ -> get_default_list SingleTop

end

module SingleExpSet = struct
  include Set.Make (
    struct
      let compare = SingleExp.cmp
      type t = SingleExp.t
    end
  )

  let t_of_sexp (s_exp: Sexp.t) : t = 
    of_list (list_of_sexp SingleExp.t_of_sexp s_exp)

  let sexp_of_t (s: t) : Sexp.t = 
    sexp_of_list SingleExp.sexp_of_t (elements s)

end
