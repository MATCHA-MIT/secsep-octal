open Type.Mem_type
 
 (* 
 0| RAX | EAX | AX | AH | AL -> 0
 1| RCX | ECX | CX | CH | CL -> 1
 2| RDX | EDX | DX | DH | DL -> 2
 3| RBX | EBX | BX | BH | BL -> 3
 4| RSP | ESP | SP | SPL -> 4
 5| RBP | EBP | BP | BPL -> 5
 6| RSI | ESI | SI | SIL -> 6
 7| RDI | EDI | DI | DIL -> 7
 8| R8 | R8D | R8W | R8B -> 8
 9| R9 | R9D | R9W | R9B -> 9
10| R10 | R10D | R10W | R10B -> 10
11| R11 | R11D | R11W | R11B -> 11
12| R12 | R12D | R12W | R12B -> 12
13| R13 | R13D | R13W | R13B -> 13
14| R14 | R14D | R14W | R14B -> 14
15| R15 | R15D | R15W | R15B -> 15 
*)

let rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15 = 
  (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)

(* Note the type in the init_mem here are for the first block, which will be added to start_imm_var later *)

let sha512_block_data_order_init_mem : MemRangeType.t = {
  ptr_list = MemKeySet.of_list [ rsp; rdi; rsi ];
  mem_type = [
    (rdi, [ ((SingleConst 0L, SingleConst 64L), TypeSingle (SingleVar 16)) ]);
    (rsi, [ ((SingleConst 0L, SingleBExp (SingleMul, SingleVar rdx, SingleConst 128L)), TypeSingle (SingleVar 17))])
  ]
}

let sha512_final_impl_init_mem : MemRangeType.t = {
  ptr_list = MemKeySet.of_list [ rsp; rdi; rdx ];
  mem_type = [
    (rdi, [ ((SingleConst 0L, SingleVar rsi), TypeSingle (SingleVar 16)) ]);
    (rdx, [
      ((SingleConst 0L, SingleConst 64L), TypeSingle (SingleVar 16));
      ((SingleConst 64L, SingleConst 72L), TypeSingle (SingleVar 17));
      ((SingleConst 72L, SingleConst 80L), TypeSingle (SingleVar 18));
      ((SingleConst 80L, SingleConst 208L), TypeSingle (SingleVar 19));
      ((SingleConst 208L, SingleConst 212L), TypeSingle (SingleVar 20));
      ((SingleConst 212L, SingleConst 216L), TypeSingle (SingleVar 21))
    ])
  ]
}

