open Isa

module InitStack = struct
  exception InitStackError of string

  let init_stack_error msg = raise (InitStackError ("[Init Stack Error] " ^ msg))

  module Ints = Set.Make(Int)

  let add_stack_slots (stack_slots: Ints.t) (offset: int) (operand: Isa.operand) : Ints.t =
    match operand with
    | MemOp (None, Some Isa.RSP, None, None) -> Ints.add offset stack_slots
    | MemOp (Some (ImmNum x), Some Isa.RSP, None, None) -> Ints.add (offset + x) stack_slots
    | _ -> stack_slots

  let find_offset_helper (offset_list: (int option) list) (stack_slots: Ints.t) (p: Isa.program) : (int option) list * Ints.t =
    let helper (acc: (int option) list * Ints.t * int) (block: Isa.basic_block) : (int option) list * Ints.t * int =
      let helper2 (acc2: (int option) list * Ints.t * int)  (inst: Isa.instruction) : (int option) list * Ints.t * int =
        let old_offset_list, stack_slots, offset = acc2 in
        match inst with
        (* Insts that affect rsp offset *)
        | Push _ -> (old_offset_list, stack_slots, offset - 8)
        | Pop _ -> (old_offset_list, stack_slots, offset + 8)
        | Add (RegOp dst, RegOp r, ImmOp (ImmNum i))
        | Add (RegOp dst, ImmOp (ImmNum i), RegOp r) -> (* NOTE: Maybe remove the second case *)
          if dst = Isa.RSP && r = Isa.RSP then (old_offset_list, stack_slots, offset + i) else (old_offset_list, stack_slots, offset)
        | Sub (RegOp dst, RegOp r, ImmOp (ImmNum i)) ->
          if dst = Isa.RSP && r = Isa.RSP then (old_offset_list, stack_slots, offset - i) else (old_offset_list, stack_slots, offset)
        (* Insts that connect to other blocks *)
        | Jcond (target_label, _)
        | Jmp target_label ->
          let find_target_idx = List.find_index (fun (x: Isa.basic_block) -> target_label = x.label) p.bbs in
          begin match find_target_idx with
            | Some target_idx ->
              let target_offset = List.nth old_offset_list target_idx in
              begin match target_offset with
              | Some o -> if o = offset then (old_offset_list, stack_slots, offset) else init_stack_error ("offset conflict at label " ^ target_label)
              | None -> (List.mapi (fun i x -> if i = target_idx then Some offset else x) old_offset_list, stack_slots, offset)
              end
            | None -> (old_offset_list, stack_slots, offset)
          end
        (* Insts that may access stack spills *)
        (* TODO: We may optimize this with x86 features *)
        | Mov (op0, op1) | MovS (op0, op1) | MovZ (op0, op1) 
        | Not (op0, op1) | Cmp (op0, op1) ->
          let slots1 = add_stack_slots stack_slots offset op0 in
          let slots2 = add_stack_slots slots1 offset op1 in
          (old_offset_list, slots2, offset)
        | Add (op0, op1, op2) | Sub (op0, op1, op2) | Sal (op0, op1, op2)
        | Sar (op0, op1, op2) | Shr (op0, op1, op2) | Xor (op0, op1, op2)
        | And (op0, op1, op2) | Or (op0, op1, op2) ->
          let slots1 = add_stack_slots stack_slots offset op0 in
          let slots2 = add_stack_slots slots1 offset op1 in
          let slots3 = add_stack_slots slots2 offset op2 in
          (old_offset_list, slots3, offset)
        | _ -> (old_offset_list, stack_slots, offset)
      in
      let old_offset_list, old_stack_slots, idx = acc in
      let curr_offset = List.nth old_offset_list idx in
      match curr_offset with
      | Some o -> 
        let new_offset_list, new_stack_slots, _ = List.fold_left helper2 (old_offset_list, old_stack_slots, o) block.insts in
        (new_offset_list, new_stack_slots, idx + 1)
      | None -> (offset_list, old_stack_slots, idx + 1)
    in
    let new_offset_list, new_stack_slots, _ = List.fold_left helper (offset_list, stack_slots, 0) p.bbs in (new_offset_list, new_stack_slots)

  let offset_solved (offset_list: (int option) list) : bool =
    let helper (acc: bool) (offset: int option) =
      match offset with
      | Some _ -> acc
      | None -> false
    in
    List.fold_left helper true offset_list

  let find_offset (offset_list: (int option) list) (stack_slots: Ints.t) (p: Isa.program) (num_iter: int) : (int option) list * Ints.t =
    if num_iter == 0 then (offset_list, stack_slots)
    else if offset_solved offset_list then (offset_list, stack_slots)
    else find_offset_helper offset_list stack_slots p

  let update_offset (p: Isa.program) : Isa.program * (int list) =
    let init_offset_list = List.mapi (fun i _ -> if i = 0 then Some 0 else None) p.bbs in
    let offset_list, stack_slots = find_offset init_offset_list Ints.empty p 1 in
    let helper (new_offset: int option) (block: Isa.basic_block) : Isa.basic_block =
      match new_offset with
      | Some o -> { block with rsp_offset = o }
      | None -> init_stack_error "offset not initialized"
    in
    ({ p with bbs = List.map2 helper offset_list p.bbs }, Ints.to_list stack_slots)

end
