open Type.Isa
open Type.Single_type_infer

let rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15 = 
  (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)

exception MemInterfaceError of string
let mem_interface_error msg = raise (MemInterfaceError ("[Mem Interface Error] " ^ msg))

let get_mem_interface 
    (interface_list: (Isa.label * 'a) list)
    (func_name: Isa.label) : 'a =
  let find_interface = 
    List.find_map (fun (name, interface) -> if name = func_name then Some interface else None) interface_list
  in
  match find_interface with
  | Some interface -> interface
  | None -> mem_interface_error (Printf.sprintf "Cannot get mem interface of func %s" func_name)

let salsa20_global : SingleTypeInfer.ArchType.MemType.t = [
  -2, [ ((SingleConst 0L, SingleConst 32L), [(SingleConst 0L, SingleConst 32L)], SingleTop) ];
  -3, [ ((SingleConst 0L, SingleConst 8L), [(SingleConst 0L, SingleConst 8L)], SingleTop) ];
  -4, [ ((SingleConst 0L, SingleConst 64L), [(SingleConst 0L, SingleConst 64L)], SingleTop) ];
]
(* Note that salsa20_global is not real global var, so I only put it in _start's mem interface *)
let standalone_salsa20 : (Isa.label * SingleTypeInfer.ArchType.MemType.t) list = [
  "salsa20_words", [
    rsp, [ ((SingleConst 0L, SingleConst 0L), [], SingleTop) ];
    rdi, [ ((SingleConst 0L, SingleConst 64L), [], SingleTop) ];
    rsi, [ ((SingleConst 0L, SingleConst 64L), [], SingleTop) ];
  ];
  "salsa20_block", [
    rsp, [ ((SingleConst 0L, SingleConst 0L), [], SingleTop) ];
    rdi, [ ((SingleConst 0L, SingleConst 64L), [], SingleTop) ];
    rsi, [ ((SingleConst 0L, SingleConst 32L), [], SingleTop) ];
  ];
  "salsa20", [
    rsp, [ ((SingleConst 0L, SingleConst 0L), [], SingleTop) ];
    rdi, [ ((SingleConst 0L, SingleVar rsi), [], SingleTop) ];
    rdx, [ ((SingleConst 0L, SingleConst 32L), [], SingleTop) ]
  ];
  "_start", salsa20_global @ [
    rsp, [ ((SingleConst 0L, SingleConst 0L), [], SingleTop) ]
  ]
]

let sha512_final_impl : (Isa.label * SingleTypeInfer.ArchType.MemType.t) list = [
  "sha512_block_data_order", [
    rsp, [ ((SingleConst 0L, SingleConst 0L), [], SingleTop) ];
    rdi, [ ((SingleConst 0L, SingleConst 64L), [], SingleTop) ];
    rsi, [ ((SingleConst 0L, SingleBExp (SingleMul, SingleConst 128L, SingleVar rdx)), [], SingleTop) ];
    -2,  [ ((SingleConst 0L, SingleConst 640L), [], SingleTop) ];
  ];
  (* "sha512_final_impl", [
    rsp, [ ((SingleConst 0L, SingleConst 0L), [], SingleTop) ];
    rdi, [ ((SingleConst 0L, SingleVar rsi), [], SingleTop) ];
    rdx, [ ((SingleConst 0L, SingleConst 216L), [], SingleTop) ];
    -2,  [ ((SingleConst 0L, SingleConst 640L), [], SingleTop) ];
  ] *)
]

let salsa20_block_init_mem : SingleTypeInfer.ArchType.MemType.t = [
  (rsp, [ ((SingleConst 0L, SingleConst 0L), [], SingleTop) ]);
  (rdi, [ ((SingleConst 0L, SingleConst 64L), [], SingleTop) ]);
  (rsi, [ ((SingleConst 0L, SingleConst 32L), [], SingleTop) ]);
]

