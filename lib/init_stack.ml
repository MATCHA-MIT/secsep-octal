open Isa

module InitStack = struct
  exception InitStackError of string

  let init_stack_error msg = raise (InitStackError ("[Init Stack Error] " ^ msg))

  type offset_var_id = int

  let find_offset_helper (offset_list: (int option) list) (p: Isa.program) : (int option) list =
    let helper (acc: (int option) list * int) (block: Isa.basic_block) : (int option) list * int =
      let helper2 (acc2: (int option) list * int)  (inst: Isa.instruction) : (int option) list * int =
        let old_offset_list, offset = acc2 in
        match inst with
        | Push _ -> (old_offset_list, offset - 8)
        | Pop _ -> (old_offset_list, offset + 8)
        | Add (RegOp dst, RegOp r, ImmOp (ImmNum i))
        | Add (RegOp dst, ImmOp (ImmNum i), RegOp r) -> (* NOTE: Maybe remove the second case *)
          if dst = Isa.RSP && r = Isa.RSP then (old_offset_list, offset + i) else (old_offset_list, offset)
        | Sub (RegOp dst, RegOp r, ImmOp (ImmNum i)) ->
          if dst = Isa.RSP && r = Isa.RSP then (old_offset_list, offset - i) else (old_offset_list, offset)
        | Jcond (target_label, _)
        | Jmp target_label ->
          let find_target_idx = List.find_index (fun (x: Isa.basic_block) -> target_label = x.label) p.bbs in
          begin match find_target_idx with
            | Some target_idx ->
              let target_offset = List.nth old_offset_list target_idx in
              begin match target_offset with
              | Some o -> if o = offset then (old_offset_list, offset) else init_stack_error ("offset conflict at label " ^ target_label)
              | None -> (List.mapi (fun i x -> if i = target_idx then Some offset else x) old_offset_list, offset)
              end
            | None -> (old_offset_list, offset)
          end
        | _ -> (old_offset_list, offset)
      in
      let old_offset_list, idx = acc in
      let curr_offset = List.nth old_offset_list idx in
      match curr_offset with
      | Some o -> 
        let new_offset_list, _ = List.fold_left helper2 (old_offset_list, o) block.insts in
        (new_offset_list, idx + 1)
      | None -> (offset_list, idx + 1)
    in
    let new_offset_list, _ = List.fold_left helper (offset_list, 0) p.bbs in new_offset_list

  let offset_solved (offset_list: (int option) list) : bool =
    let helper (acc: bool) (offset: int option) =
      match offset with
      | Some _ -> acc
      | None -> false
    in
    List.fold_left helper true offset_list

  let find_offset (offset_list: (int option) list) (p: Isa.program) (num_iter: int) : (int option) list =
    if num_iter == 0 then offset_list
    else if offset_solved offset_list then offset_list
    else find_offset_helper offset_list p

  let update_offset (p: Isa.program) : Isa.program =
    let init_offset_list = List.mapi (fun i _ -> if i = 0 then Some 0 else None) p.bbs in
    let offset_list = find_offset init_offset_list p 1 in
    let helper (new_offset: int option) (block: Isa.basic_block) : Isa.basic_block =
      match new_offset with
      | Some o -> { block with rsp_offset = o }
      | None -> init_stack_error "offset not initialized"
    in
    { p with bbs = List.map2 helper offset_list p.bbs }

end
