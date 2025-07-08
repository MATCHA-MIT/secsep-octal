open Isa_basic
open Set_sexp
open Sexplib
open Sexplib.Std

module CondTypeBase = struct
  type t = 
    | Eq | Ne
    | Le | Lt (* Signed comparison *)
    | Be | Bt (* Unsigned comparison *)
  [@@deriving sexp, compare]

  let to_string (cond: t) : string =
    match cond with
    | Eq -> "Eq" | Ne -> "Ne"
    | Le -> "Le" | Lt -> "Lt"
    | Be -> "Be" | Bt -> "Bt"

end

module SingleExpBasic = struct
  exception SingleExpError of string
  let single_exp_error msg = raise (SingleExpError ("[Single Exp Error] " ^ msg))

  module SingleVarSet = IntSet

  type single_bop =
    | SingleAdd
    | SingleSub
    | SingleMul
    | SingleSal
    | SingleSar
    | SingleShr
    | SingleXor
    | SingleAnd
    | SingleOr
    | SingleMod
  [@@deriving sexp]

  type single_uop =
    | SingleNot
  [@@deriving sexp]

  type t =
    | SingleTop
    | SingleConst of int64
    | SingleVar of IsaBasic.imm_var_id
    | SingleBExp of single_bop * t * t
    | SingleUExp of single_uop * t
    | SingleITE of (CondTypeBase.t * t * t) * t * t
  [@@deriving sexp]

  type single_var_type_t =
    | VarPtr
    | VarLen
    | VarOther
  [@@deriving sexp]
  
  type var_type_map_t = (IsaBasic.imm_var_id * single_var_type_t) list
  [@@deriving sexp]

  let get_var_type
      (var_type_map: var_type_map_t)
      (var: IsaBasic.imm_var_id) : single_var_type_t =
    match List.find_opt (fun (v, _) -> v = var) var_type_map with
    | Some (_, t) -> t
    | _ -> VarOther

  let set_var_type 
      (var_type_map: var_type_map_t)
      (var: IsaBasic.imm_var_id)
      (var_type: single_var_type_t) :
      var_type_map_t =
    match get_var_type var_type_map var, var_type with
    | VarOther, _ -> (var, var_type) :: var_type_map
    | VarPtr, VarLen
    | VarLen, VarPtr -> single_exp_error "set_var_type conflict"
    | _ -> var_type_map

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
      Int64.compare c1 c2
    | SingleConst _, _ -> -1
    | SingleVar v1, SingleVar v2 -> 
      Int.compare v1 v2
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

end

module SingleCondSet = struct
  module SingleCondTypeBasic = struct
    type t = CondTypeBase.t * SingleExpBasic.t * SingleExpBasic.t
      [@@deriving sexp]

    let compare (cond1: t) (cond2: t) : int =
      let c1, l1, r1 = cond1 in
      let c2, l2, r2 = cond2 in
      let cmp_c = compare c1 c2 in
      if cmp_c = 0 then
        let cmp_l = SingleExpBasic.cmp l1 l2 in
        if cmp_l = 0 then SingleExpBasic.cmp r1 r2
        else cmp_l
      else
        cmp_c
  end

  include Set.Make (SingleCondTypeBasic)

  let t_of_sexp (s_exp: Sexp.t) : t = 
    of_list (list_of_sexp SingleCondTypeBasic.t_of_sexp s_exp)

  let sexp_of_t (s: t) : Sexp.t = 
    sexp_of_list SingleCondTypeBasic.sexp_of_t (elements s)
end

