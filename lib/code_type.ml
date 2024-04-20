(* Type and type derivation *)
open Isa
open Pretty_print

module CodeType = struct
  exception CodeTypeError of string
  let code_type_error msg = raise (CodeTypeError ("[Code Type Error] " ^ msg))

  let stack_base_id : Isa.imm_var_id = 1

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
    (* TODO: Should we add SingleNeg? *)

  type single_exp =
    | SingleConst of int
    | SingleVar of Isa.imm_var_id
    | SingleBExp of single_bop * single_exp * single_exp
    | SingleUExp of single_uop * single_exp

  let rec eval_single_exp (e: single_exp) : single_exp =
    match e with
    | SingleBExp (op, e1, e2) -> 
      let ee1 = eval_single_exp e1 in 
      let ee2 = eval_single_exp e2 in
      let default_single = SingleBExp (op, ee1, ee2) in
      begin match ee1, ee2 with
      | (SingleConst v1), (SingleConst v2) -> 
        begin match op with
        | SingleAdd -> SingleConst (v1 + v2)
        | SingleSub -> SingleConst (v1 - v2)
        | SingleMul -> SingleConst (v1 * v2)
        | SingleSal -> SingleConst (Int.shift_left v1 v2)
        | SingleSar -> SingleConst (Int.shift_right v1 v2)
        | SingleXor -> SingleConst (Int.logxor v1 v2)
        | SingleAnd -> SingleConst (Int.logand v1 v2)
        | SingleOr -> SingleConst (Int.logor v1 v2)
        end
      | _ -> default_single
      end
    | SingleUExp (op, e) -> 
      let ee = eval_single_exp e in
      let default_single = SingleUExp (op, ee) in
      begin match ee with
      | SingleConst v -> begin
          match op with
          | SingleNot -> SingleConst (Int.lognot v)
        end
      | _ -> default_single
      end
    | _ -> e

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

  type type_exp =
    | TypeSingle of single_exp
    | TypeRange of single_exp * bool * single_exp * bool * int (* begin, inc, end, inc, step *)
    | TypeVar of type_var_id
    | TypeTop
    | TypeBot
    | TypeBExp of type_bop * type_exp * type_exp
    | TypeUExp of type_uop * type_exp

  let rec eval_type_exp (e: type_exp) : type_exp =
    match e with
    | TypeBExp (tbop, e1, e2) -> 
      let ee1 = eval_type_exp e1 in
      let ee2 = eval_type_exp e2 in
      let default_type = TypeBExp (tbop, ee1, ee2) in
      begin match ee1, ee2 with
      | TypeSingle s1, TypeSingle s2 -> 
        begin match tbop with
        | TypeAdd -> TypeSingle (eval_single_exp (SingleBExp (SingleAdd, s1, s2)))
        | TypeSub -> TypeSingle (eval_single_exp (SingleBExp (SingleSub, s1, s2)))
        | TypeMul -> TypeSingle (eval_single_exp (SingleBExp (SingleMul, s1, s2)))
        | TypeSal -> TypeSingle (eval_single_exp (SingleBExp (SingleSal, s1, s2)))
        | TypeSar -> TypeSingle (eval_single_exp (SingleBExp (SingleSal, s1, s2)))
        | TypeXor -> TypeSingle (eval_single_exp (SingleBExp (SingleXor, s1, s2)))
        | TypeAnd -> TypeSingle (eval_single_exp (SingleBExp (SingleAnd, s1, s2)))
        | TypeOr -> TypeSingle (eval_single_exp (SingleBExp (SingleOr, s1, s2)))
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
        | TypeAdd -> TypeRange ((eval_single_exp (SingleBExp (SingleAdd, s1, s0))), b1,
                                (eval_single_exp (SingleBExp (SingleAdd, s2, s0))), b2, step)
        | TypeSub -> TypeRange ((eval_single_exp (SingleBExp (SingleSub, s0, s1))), b2,
                                (eval_single_exp (SingleBExp (SingleSub, s0, s1))), b1, step)
        | TypeMul -> 
          begin match s0 with
          | SingleConst i0 -> TypeRange ((eval_single_exp (SingleBExp (SingleMul, s1, s0))), b1,
                                         (eval_single_exp (SingleBExp (SingleMul, s2, s0))), b2, step * i0)
          | _ -> default_type
            (* TypeRange ((eval_single_exp (SingleBExp (SingleMul, s1, s0))), b1,
                            (eval_single_exp (SingleBExp (SingleMul, s2, s0))), b2, step) *)
          end
        (* TODO: TypeXor TypeAnd TypeOr *)
        | _ -> default_type
        end
      | TypeRange (s1, b1, s2, b2, step), TypeSingle s0 -> 
        begin match tbop with
        | TypeAdd -> TypeRange ((eval_single_exp (SingleBExp (SingleAdd, s1, s0))), b1,
                                (eval_single_exp (SingleBExp (SingleAdd, s2, s0))), b2, step)
        | TypeSub -> TypeRange ((eval_single_exp (SingleBExp (SingleSub, s1, s0))), b1,
                                (eval_single_exp (SingleBExp (SingleSub, s2, s0))), b2, step)
        | TypeMul -> 
          begin match s0 with
          | SingleConst i0 -> TypeRange ((eval_single_exp (SingleBExp (SingleMul, s1, s0))), b1,
                                         (eval_single_exp (SingleBExp (SingleMul, s2, s0))), b2, step * i0)
          | _ -> default_type
            (* TypeRange ((eval_single_exp (SingleBExp (SingleMul, s1, s0))), b1,
                            (eval_single_exp (SingleBExp (SingleMul, s2, s0))), b2, step) *)
          end
        | TypeSal ->
          begin match s0 with
          | SingleConst i0 -> TypeRange ((eval_single_exp (SingleBExp (SingleSal, s1, s0))), b1,
                                         (eval_single_exp (SingleBExp (SingleSal, s2, s0))), b2, Int.shift_left step i0)
          | _ -> default_type
          end
        | TypeSar ->
          begin match s0 with
          | SingleConst i0 -> TypeRange ((eval_single_exp (SingleBExp (SingleSar, s1, s0))), b1,
                                          (eval_single_exp (SingleBExp (SingleSar, s2, s0))), b2, Int.shift_right step i0)
          | _ -> default_type
          end
        (* TODO: TypeXor TypeAnd TypeOr *)
        | _ -> default_type
        end
      | _ -> default_type
      end
    | TypeUExp (tuop, e) ->
      let ee = eval_type_exp e in
      let default_type = TypeUExp (tuop, ee) in
      begin match ee with
      | TypeSingle s ->
        begin match tuop with
        | TypeNot -> TypeSingle (eval_single_exp (SingleUExp (SingleNot, s)))
        | _ -> default_type
        end
      | _ -> default_type
      end
    | _ -> e


  module Ints = Set.Make(Int)

  type type_full_exp = type_exp * Ints.t
  (* Record cond with a list [cond * 2 + Taken/NotTaken, ...] where Taken=1 and NotTaken=0 *)

  type reg_type = type_full_exp list
  (* type reg_type = {
    rax: type_full_exp; rcx: type_full_exp; rdx: type_full_exp; rbx: type_full_exp;
    rsp: type_full_exp; rbp: type_full_exp; rsi: type_full_exp; rdi: type_full_exp;
    r8:  type_full_exp; r9:  type_full_exp; r10: type_full_exp; r11: type_full_exp;
    r12: type_full_exp; r13: type_full_exp; r14: type_full_exp; r15: type_full_exp;
  } *)

  type mem_type = (int * type_full_exp) list

  type cond_type =
    | CondNe of (type_full_exp * type_full_exp)
    | CondEq of (type_full_exp * type_full_exp)
    | CondLq of (type_full_exp * type_full_exp)
    | CondLe of (type_full_exp * type_full_exp)

  let not_cond_type (cond: cond_type) : cond_type = 
    match cond with
    | CondNe (l, r) -> CondEq (l, r)
    | CondEq (l, r) -> CondNe (l, r)
    | CondLq (l, r) -> CondLq (r, l)
    | CondLe (l, r) -> CondLe (r, l)
  
  let get_cond_type (cond_list: cond_type list) (cond_idx: int) : cond_type =
    let idx = cond_idx / 2 in
    let cond = List.nth cond_list ((List.length cond_list) - idx) in
    let taken = Int.rem cond_idx 2 in
    if taken = 1 then cond
    else not_cond_type cond

  let add_cond_type (cond_list: cond_type list) (cond: cond_type) (taken: bool) : (cond_type list) * int =
    let cond_suffix = if taken then 1 else 0 in
    (cond::cond_list, ((List.length cond_list) + 1) * 2 + cond_suffix)

  type state_type = {
    reg_type: reg_type;
    mem_type: mem_type;
    cond_type: (type_full_exp * type_full_exp)  (* Cmp and test and etc. will set this; Cond branch will use this *)
  }

  let get_imm_type (i: Isa.immediate) : type_full_exp =
    match i with
    | ImmNum v -> (TypeSingle (SingleConst v), Ints.empty)
    | ImmLabel v -> (TypeSingle (SingleVar v), Ints.empty)

  let get_reg_type (curr_state: state_type) (r: Isa.register) : type_full_exp =
    let reg_type_list = curr_state.reg_type in
    let reg_idx = Isa.get_reg_idx r in
    List.nth reg_type_list reg_idx

  let set_reg_type (curr_state: state_type) (r: Isa.register) (t: type_full_exp) : state_type =
    let reg_list = curr_state.reg_type in
    let reg_idx = Isa.get_reg_idx r in
    let new_reg_list = List.mapi (fun idx reg_type -> if idx = reg_idx then t else reg_type) reg_list in
    {curr_state with reg_type = new_reg_list}

  let get_mem_op_type (curr_state: state_type)
      (disp: Isa.immediate option) (base: Isa.register option)
      (index: Isa.register option) (scale: Isa.scale option) : type_full_exp =
    let disp_type, _ = match disp with
    | Some d -> get_imm_type d
    | None -> get_imm_type (ImmNum 0)
    in
    let base_type, base_cond = match base with
    | Some b -> get_reg_type curr_state b
    | None -> get_imm_type (ImmNum 0)
    in
    let index_type, index_cond = match index with
    | Some i -> get_reg_type curr_state i
    | None -> get_imm_type (ImmNum 0)
    in
    let scale_val = match scale with
    | Some s -> Isa.scale_val s
    | None -> 1
    in
    let new_type = eval_type_exp (
      TypeBExp (TypeAdd,
        TypeBExp (TypeAdd, disp_type, base_type),
        TypeBExp (TypeMul, index_type, TypeSingle (SingleConst scale_val))
      )
    ) in
    let new_cond = Ints.union base_cond index_cond in
    (new_type, new_cond)

  let get_mem_idx (addr_type: type_exp) : int option =
    match addr_type with
    | TypeSingle s -> 
      begin match s with
      | SingleVar v -> if v = stack_base_id then Some 0 else None
      | SingleBExp (op, v1, v2) -> 
        begin match op, v1, v2 with
        | SingleAdd, SingleVar v, SingleConst c
        | SingleAdd, SingleConst c, SingleVar v -> if v = stack_base_id then Some c else None
        | SingleSub, SingleVar v, SingleConst c -> if v = stack_base_id then Some (-c) else None (* NOTE: this maybe not needed*)
        | _ -> None
        end
      | _ -> None
      end
    | _ -> None

  let get_mem_idx_type (curr_state: state_type) (offset: int) : type_full_exp =
    let mem_type = curr_state.mem_type in
    let idx_find = List.find_opt (fun (off, _) -> off = offset) mem_type in
    match idx_find with
    | Some (_, t) -> t
    | None -> code_type_error ("get_mem_idx_type: stack offset " ^ (string_of_int offset) ^ " not in record")

  let set_mem_idx_type (curr_state: state_type) (offset: int) (new_type: type_full_exp) : state_type =
    let mem_type = curr_state.mem_type in
    let idx_find = List.find_opt (fun (off, _) -> off = offset) mem_type in
    match idx_find with
    | Some _ -> 
      {curr_state with mem_type = 
        List.map (fun (off, x) -> if off = offset then (off, new_type) else (off, x)) mem_type}
    | None -> {curr_state with mem_type = (offset, new_type) :: mem_type}

  let get_ld_op_type (curr_state: state_type)
      (disp: Isa.immediate option) (base: Isa.register option)
      (index: Isa.register option) (scale: Isa.scale option) : type_full_exp =
    let addr_type, _ = get_mem_op_type curr_state disp base index scale in
    let mem_idx = get_mem_idx addr_type in
    match mem_idx with
    | Some v -> get_mem_idx_type curr_state v
    | None -> (TypeTop, Ints.empty)

  let set_st_op_type(curr_state: state_type) 
      (disp: Isa.immediate option) (base: Isa.register option)
      (index: Isa.register option) (scale: Isa.scale option)
      (new_type: type_full_exp) : state_type =
    let addr_type, _ = get_mem_op_type curr_state disp base index scale in
    let mem_idx = get_mem_idx addr_type in
    match mem_idx with
    | Some v -> set_mem_idx_type curr_state v new_type (* TODO!!!*)
    | None -> curr_state

  let get_src_op_type (curr_state: state_type) (src: Isa.operand) : type_full_exp =
    match src with
    | ImmOp imm -> get_imm_type imm
    | RegOp r -> get_reg_type curr_state r
    | MemOp (disp, base, index, scale) -> get_mem_op_type curr_state disp base index scale
    | LdOp (disp, base, index, scale) -> get_ld_op_type curr_state disp base index scale
    | StOp _ -> code_type_error ("get_src_op_type: cannot get src op type of a st op")
    | LabelOp _ -> code_type_error ("get_src_op_type: cannot get src op type of a label op")

  let set_dest_op_type (curr_state: state_type) (dest: Isa.operand) (new_type: type_full_exp) : state_type =
    match dest with
    | RegOp r -> set_reg_type curr_state r new_type
    | StOp (disp, base, index, scale) -> set_st_op_type curr_state disp base index scale new_type
    | _ -> code_type_error ("set_dest_op_type: dest is not reg or st op")

  let add_type_cond (tf: type_full_exp) (cond_idx: int) : type_full_exp =
    let t, cond = tf in (t, Ints.add cond_idx cond)
  
  let add_type_cond_set (tf: type_full_exp) (cond_set: Ints.t) : type_full_exp =
    let t, cond = tf in (t, Ints.union cond_set cond)

  let add_reg_type_cond (reg_list: reg_type) (cond_idx: int) : reg_type =
    List.map (fun reg_type -> add_type_cond reg_type cond_idx) reg_list

  let add_mem_type_cond (mem_list: mem_type) (cond_idx: int) : mem_type =
    List.map (fun (off, tf) -> (off, add_type_cond tf cond_idx)) mem_list
    
  let add_state_type_cond (curr_state: state_type) (cond_idx: int) : state_type =
    {
      reg_type = add_reg_type_cond curr_state.reg_type cond_idx;
      mem_type = add_mem_type_cond curr_state.mem_type cond_idx;
      cond_type = curr_state.cond_type
    }

  let type_prop_inst (curr_state: state_type) (cond_list: cond_type list) (inst: Isa.instruction) : state_type * (cond_type list) =
    match inst with
    | Mov (dest, src)
    | MovS (dest, src)
    | MovZ (dest, src)
    | Lea (dest, src) ->
      let src_type = get_src_op_type curr_state src in
      (set_dest_op_type curr_state dest src_type, cond_list)
    | Add (dest, src1, src2) ->
      let src1_type, src1_cond = get_src_op_type curr_state src1 in
      let src2_type, src2_cond = get_src_op_type curr_state src2 in
      let dest_type = (eval_type_exp (TypeBExp (TypeAdd, src1_type, src2_type)), Ints.union src1_cond src2_cond) in
      (set_dest_op_type curr_state dest dest_type, cond_list)
    | Sub (dest, src1, src2) ->
      let src1_type, src1_cond = get_src_op_type curr_state src1 in
      let src2_type, src2_cond = get_src_op_type curr_state src2 in
      let dest_type = (eval_type_exp (TypeBExp (TypeSub, src1_type, src2_type)), Ints.union src1_cond src2_cond) in
      (set_dest_op_type curr_state dest dest_type, cond_list)
    | Sal (dest, src1, src2) ->
      let src1_type, src1_cond = get_src_op_type curr_state src1 in
      let src2_type, src2_cond = get_src_op_type curr_state src2 in
      let dest_type = (eval_type_exp (TypeBExp (TypeSal, src1_type, src2_type)), Ints.union src1_cond src2_cond) in
      (set_dest_op_type curr_state dest dest_type, cond_list)
    | Shr (dest, src1, src2) ->
      let src1_type, src1_cond = get_src_op_type curr_state src1 in
      let src2_type, src2_cond = get_src_op_type curr_state src2 in
      let dest_type = (eval_type_exp (TypeBExp (TypeShr, src1_type, src2_type)), Ints.union src1_cond src2_cond) in
      (set_dest_op_type curr_state dest dest_type, cond_list)
    | Sar (dest, src1, src2) ->
      let src1_type, src1_cond = get_src_op_type curr_state src1 in
      let src2_type, src2_cond = get_src_op_type curr_state src2 in
      let dest_type = (eval_type_exp (TypeBExp (TypeSar, src1_type, src2_type)), Ints.union src1_cond src2_cond) in
      (set_dest_op_type curr_state dest dest_type, cond_list)
    | Xor (dest, src1, src2) ->
      (* TODO: Add special case for xor %rax %rax *)
      begin match src1, src2 with
      | RegOp r1, RegOp r2 ->
        if r1 = r2 then (set_dest_op_type curr_state dest (TypeSingle (SingleConst 0), Ints.empty), cond_list)
        else begin
          let src1_type, src1_cond = get_src_op_type curr_state src1 in
          let src2_type, src2_cond = get_src_op_type curr_state src2 in
          let dest_type = (eval_type_exp (TypeBExp (TypeXor, src1_type, src2_type)), Ints.union src1_cond src2_cond) in
          (set_dest_op_type curr_state dest dest_type, cond_list)
        end
      | _ ->
        let src1_type, src1_cond = get_src_op_type curr_state src1 in
        let src2_type, src2_cond = get_src_op_type curr_state src2 in
        let dest_type = (eval_type_exp (TypeBExp (TypeXor, src1_type, src2_type)), Ints.union src1_cond src2_cond) in
        (set_dest_op_type curr_state dest dest_type, cond_list)
      end
    | Not (dest, src) ->
      let src_type, src_cond = get_src_op_type curr_state src in
      let dest_type = (eval_type_exp (TypeUExp (TypeNot, src_type)), src_cond) in
      (set_dest_op_type curr_state dest dest_type, cond_list)
    | And (dest, src1, src2) ->
      let src1_type, src1_cond = get_src_op_type curr_state src1 in
      let src2_type, src2_cond = get_src_op_type curr_state src2 in
      let dest_type = (eval_type_exp (TypeBExp (TypeAnd, src1_type, src2_type)), Ints.union src1_cond src2_cond) in
      (set_dest_op_type curr_state dest dest_type, cond_list)
    | Or (dest, src1, src2) ->
      let src1_type, src1_cond = get_src_op_type curr_state src1 in
      let src2_type, src2_cond = get_src_op_type curr_state src2 in
      let dest_type = (eval_type_exp (TypeBExp (TypeOr, src1_type, src2_type)), Ints.union src1_cond src2_cond) in
      (set_dest_op_type curr_state dest dest_type, cond_list)
    | Cmp (src1, src2) ->
      let src1_full_type = get_src_op_type curr_state src1 in
      let src2_full_type = get_src_op_type curr_state src2 in
      ({curr_state with cond_type = (src1_full_type, src2_full_type)}, cond_list)
      (* TODO: Handle the case where some other instructions set flags and some other instructions use flags!!! *)
    | Jcond (_, branch_cond) ->
      let cond_left, cond_right = curr_state.cond_type in
      let new_cond = 
        begin match branch_cond with
        | JNe -> CondNe (cond_left, cond_right)
        | JE -> CondEq (cond_left, cond_right)
        | JL -> CondLq (cond_left, cond_right)
        | JLe -> CondLe (cond_left, cond_right)
        | JG -> CondLq (cond_right, cond_left)
        | JGe -> CondLe (cond_right, cond_left)
        end in
      let new_cond_list, new_cond_idx = add_cond_type cond_list new_cond false in
      (add_state_type_cond curr_state new_cond_idx, new_cond_list)
    | _ -> begin
        print_endline ("[Warning] type_prop_inst: instruction not handled: " ^ (Isa.mnemonic_of_instruction inst));
        (curr_state, cond_list) (* TODO *)
    end



  type block_type = {
    label: Isa.label;
    block_code_type: state_type;
  }

  type t = block_type list

  let init_reg_type (start_type_var_idx: type_var_id) : int * reg_type =
    (start_type_var_idx + Isa.total_reg_num,
    List.init Isa.total_reg_num (fun idx -> (TypeVar (start_type_var_idx + idx), Ints.empty))) (* TODO: for rsp, use a passed in "init offset" *)

  let init_mem_type (start_type_var_idx: int) (mem_off_list: int list) : int * mem_type =
    List.fold_left_map (fun acc a -> (acc + 1, (a, (TypeVar acc, Ints.empty)))) start_type_var_idx mem_off_list

  let init_code_type_var (start_type_var_idx: type_var_id) (prog: Isa.program) (mem_off_list: int list) : int * t =
    let helper (acc: type_var_id) (block: Isa.basic_block) : type_var_id * block_type = begin
      let acc1, reg_t = init_reg_type acc in
      let acc2, mem_t = init_mem_type acc1 mem_off_list in
      (acc2, {
        label = block.label;
        block_code_type = {
          reg_type = reg_t;
          mem_type = mem_t;
          cond_type = ((TypeSingle (SingleConst 0), Ints.empty), (TypeSingle (SingleConst 0), Ints.empty));
        }
      }) 
    end in
    List.fold_left_map helper start_type_var_idx prog
    
  let get_label_type (code_type: t) (label: Isa.label) : state_type =
    (List.find (fun x -> label = x.label) code_type).block_code_type

  let rec string_of_single_exp (se: single_exp) =
    match se with
    | SingleConst v -> "Const " ^ (string_of_int v)
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

  let rec string_of_type_exp (t: type_exp) =
    match t with
    | TypeSingle ts -> string_of_single_exp ts
    | TypeRange (bg, bgi, ed, edi, step) -> 
      let bg_str = if bgi then "[" else "(" in
      let ed_str = if edi then "]" else ")" in
      bg_str ^ (string_of_single_exp bg) ^ ", " ^ (string_of_single_exp ed) ^ ed_str ^ " step = " ^ (string_of_int step)
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
  
  let string_of_one_cond_status (cond: int) =
    let cond_idx = cond / 2 in
    let taken = if Int.rem cond 2 = 1 then "Taken" else "NotTaken" in
    string_of_int cond_idx ^ " " ^ taken

  let string_of_cond_status (conds: Ints.t) =
    "[" ^ (Ints.fold (fun x acc -> (if acc = "" then "" else acc ^ ", ") ^ (string_of_one_cond_status x)) conds "") ^ "]"

  let pp_type_full_exp (lvl: int) (tf: type_full_exp) =
    let t, cond = tf in
    PP.print_lvl lvl "(Type = %s, Cond = %s)"
      (string_of_type_exp t)
      (string_of_cond_status cond)

  let pp_cond (lvl: int) (cond: cond_type) =
    let op, str1, str2 = match cond with
    | CondNe (l, r) -> ("Ne", string_of_type_exp (fst l), string_of_type_exp (fst r))
    | CondEq (l, r) -> ("Eq", string_of_type_exp (fst l), string_of_type_exp (fst r))
    | CondLq (l, r) -> ("Lq", string_of_type_exp (fst l), string_of_type_exp (fst r))
    | CondLe (l, r) -> ("Le", string_of_type_exp (fst l), string_of_type_exp (fst r))
    in
    PP.print_lvl lvl "Cond %s between\n" op;
    PP.print_lvl (lvl + 1) "%s\n" str1;
    PP.print_lvl (lvl + 1) "%s\n" str2

  let pp_cond_list (lvl: int) (cond_list: cond_type list) =
    List.iteri (fun i x -> 
      PP.print_lvl lvl "<Cond %d>\n" (i + 1);
      pp_cond (lvl + 1) x
    ) (List.rev cond_list)

  let pp_state_type (lvl: int) (s: state_type) =
    PP.print_lvl lvl "Reg:\n";
    List.iteri (fun i x ->
      PP.print_lvl (lvl + 1) "<Reg %d> " i;
      pp_type_full_exp (lvl + 2) x;
      Printf.printf "\n"
    ) s.reg_type;
    PP.print_lvl lvl "Mem:\n";
    List.iter (fun (off, x) ->
      PP.print_lvl (lvl + 1) "<offset %d> " off;
      pp_type_full_exp (lvl + 2) x;
      Printf.printf "\n"
    ) s.mem_type;
    PP.print_lvl lvl "Cond:\n";
    pp_type_full_exp (lvl + 1) (fst s.cond_type);
    Printf.printf "\n";
    pp_type_full_exp (lvl + 1) (snd s.cond_type)

  let pp_block_types (lvl: int) (block_types: t) =
    List.iter (fun x ->
      PP.print_lvl lvl "<Block \"%s\">\n" x.label;
      pp_state_type (lvl + 1) x.block_code_type;
      Printf.printf "\n"
    ) block_types

  (* TODO: Initialize register and stack type based on program *)
  (* TODO: Maybe we can simply remove Lea!!! *)
  (* TODO: RSP type should always be represented as TypeSingle (stack_id + offset) to track stack spill. So we need to derive rsp first. *)
end

