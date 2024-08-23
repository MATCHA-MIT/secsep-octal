open Type.Single_type_infer

let rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15 = 
  (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)

let salsa20_block_init_mem : SingleTypeInfer.ArchType.MemType.t = [
  (rsp, []);
  (rdi, [ ((SingleConst 0L, SingleConst 64L), [], SingleTop) ]);
  (rsi, [ ((SingleConst 0L, SingleConst 32L), [], SingleTop) ]);
]

