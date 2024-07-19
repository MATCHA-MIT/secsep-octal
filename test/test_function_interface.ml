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

(* There's no need to enter absolute offset here, for the init function will add the base *)

let sha512_block_data_order_init_mem : MemRangeType.t = {
  ptr_list = MemKeySet.of_list [ rsp; rdi; rsi ];
  mem_type = [
    (rsp, []);
    (rdi, [ ((SingleConst 0L, SingleConst 64L), TypeSingle (SingleVar 16)) ]);
    (rsi, [ ((SingleConst 0L, SingleBExp (SingleMul, SingleVar rdx, SingleConst 128L)), TypeSingle (SingleVar 17))])
  ]
}

let sha512_final_impl_init_mem : MemRangeType.t = {
  ptr_list = MemKeySet.of_list [ rsp; rdi; rdx ];
  mem_type = [
    (rsp, []);
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

let table_select_init_mem : MemRangeType.t = {
  ptr_list = MemKeySet.of_list [ rsp; rdi ];
  mem_type = [
    (rsp, []);
    (rdi, [
      ((SingleConst 0L, SingleConst 8L), TypeSingle (SingleVar 16));
      ((SingleConst 8L, SingleConst 16L), TypeSingle (SingleVar 17));
      ((SingleConst 16L, SingleConst 24L), TypeSingle (SingleVar 18));
      ((SingleConst 24L, SingleConst 32L), TypeSingle (SingleVar 19));
      ((SingleConst 32L, SingleConst 40L), TypeSingle (SingleVar 20));
      ((SingleConst 40L, SingleConst 48L), TypeSingle (SingleVar 21));
      ((SingleConst 48L, SingleConst 56L), TypeSingle (SingleVar 22));
      ((SingleConst 56L, SingleConst 64L), TypeSingle (SingleVar 23));
      ((SingleConst 64L, SingleConst 72L), TypeSingle (SingleVar 24));
      ((SingleConst 72L, SingleConst 80L), TypeSingle (SingleVar 25));
      ((SingleConst 80L, SingleConst 88L), TypeSingle (SingleVar 26));
      ((SingleConst 88L, SingleConst 96L), TypeSingle (SingleVar 27));
      ((SingleConst 96L, SingleConst 104L), TypeSingle (SingleVar 28));
      ((SingleConst 104L, SingleConst 112L), TypeSingle (SingleVar 29));
      ((SingleConst 112L, SingleConst 120L), TypeSingle (SingleVar 30));
    ])
  ]
}
