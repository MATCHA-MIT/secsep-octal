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
    | SingleTop
    | SingleConst of int64
    | SingleVar of Isa.imm_var_id
    | SingleBExp of single_bop * t * t
    | SingleUExp of single_uop * t

  type local_var_map_t = (Isa.imm_var_id * t) list

  let get_empty_var_map : local_var_map_t = []

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
      | SingleXor -> "Xor"
      | SingleAnd -> "And"
      | SingleOr -> "Or"
      in
      "S-BinaryExp (" ^ op_str ^ ", " ^ (to_string l) ^ ", " ^ (to_string r) ^ ")"
    | SingleUExp (op, e) ->
      let op_str = match op with
      | SingleNot -> "Not"
      in
      "S-UnaryExp (" ^ op_str ^ ", " ^ (to_string e) ^ ")"

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
    List.find_opt (fun a -> a = SingleTop) x != None

  let find_top_double (x: t list list) : bool =
    List.find_opt (fun a -> find_top a) x != None

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
        else if c12 = 1L then tl
        else SingleConst c12 :: tl
      | SingleConst c :: tl ->
        if c = 0L then []
        else if c = 1L then tl
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

  let update_local_var (map: local_var_map_t) (e: t) (pc: int) : (local_var_map_t * t) =
    let new_idx = -pc in
    (new_idx, e) :: map, SingleVar new_idx

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
    | SingleTop -> map (* It is OK to replace any type with Top!!! For taint, it is OK to replace any taint with Tainted!!! *)
    | _ -> single_exp_error (Printf.sprintf "add_local_var cannot add %s->%s" (to_string e1) (to_string e2))

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
    (* Recursive repl; if not found, then just leave the var there *)
    let rec repl_helper (e: t) : t =
      match e with
      | SingleTop | SingleConst _ -> e
      | SingleVar v ->
        if v > 0 then (* input/block var *)
           e (* Here is dirty since it will also lookup global var *)
        else begin 
          match find_local_var_map map v with
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
    | _ -> SingleVarSet.empty

  let is_val (global_var: SingleVarSet.t) (e: t) : bool =
    SingleVarSet.is_empty (SingleVarSet.diff (get_vars e) global_var)

  let is_val2 (var_map: local_var_map_t) (e: t) : bool =
    let var_set = 
      List.fold_left (
        fun acc (x, _) -> SingleVarSet.add x acc
      ) SingleVarSet.empty var_map 
    in
    is_val var_set e

  let rec repl_var_exp (e: t) (v_idx_exp: Isa.imm_var_id * t) : t =
    let idx, v_exp = v_idx_exp in
    match e with
    | SingleVar v -> if v = idx then v_exp else e
    | SingleBExp (bop, e1, e2) -> SingleBExp (bop, repl_var_exp e1 v_idx_exp, repl_var_exp e2 v_idx_exp)
    | SingleUExp (uop, e) -> SingleUExp (uop, repl_var_exp e v_idx_exp)
    | _ -> e

  let rec get_imm_type (i: Isa.immediate) : t =
    match i with
    | ImmNum v -> SingleConst v
    | ImmLabel v -> SingleVar v
    | ImmBExp (i1, i2) -> SingleBExp (SingleAdd, get_imm_type i1, get_imm_type i2)

  let get_mem_op_type
      (disp: Isa.immediate option) (base: t option)
      (index: t option) (scale: int64) : t =
    let disp_type = 
      match disp with
      | Some d -> get_imm_type d
      | None -> get_imm_type (ImmNum 0L)
    in
    let base_type =
      match base with
      | Some b -> b
      | None -> get_imm_type (ImmNum 0L)
    in
    let index_type =
      match index with
      | Some i -> i
      | None -> get_imm_type (ImmNum 0L)
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
    | _ -> SingleVarSet.empty

  let find_base (e: t) (ptr_set: SingleVarSet.t) : Isa.imm_var_id option =
    let p_set = filter_single_var e in
    match SingleVarSet.to_list (SingleVarSet.inter ptr_set p_set) with
    | [] -> None
    | hd :: [] -> Some hd
    | _ -> single_exp_error (Printf.sprintf "find_base find more than one base for %s" (to_string e))

end