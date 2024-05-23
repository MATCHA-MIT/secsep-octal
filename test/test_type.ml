
(* open Type.Isa *)
open Type.Cond_type
open Type.Code_type
open Type.Gen_type
open Type.Subtype
open Type.Parser
open Type.Init_mem

(* let _ : Isa.program = 
  [ { label = "foo";
      insts =
      [ Isa.Xor (Isa.RegOp Isa.RAX, Isa.RegOp Isa.RAX, Isa.RegOp Isa.RAX);
      ];
    };
    {
      label = ".Loop";
      insts = 
      [ Isa.Add (Isa.RegOp Isa.RAX, Isa.RegOp Isa.RAX, Isa.ImmOp (Isa.ImmNum 1));
        Isa.Cmp (Isa.RegOp Isa.RAX, Isa.ImmOp (Isa.ImmNum 5));
        Isa.Jcond (".Loop", Isa.JNe);
        Isa.Ret
      ]
    }
];; *)


let read_file (filename: string) : string =
  let channel = open_in filename in
  try
    let content = really_input_string channel (in_channel_length channel) in
    close_in channel;
    content
  with e ->
    close_in_noerr channel;
    raise e

let p = Parser.parse_program (read_file "../asm/demo.s") 

(* TODO: print subtype relation *)
(* TODO: run this test case *)

let tv_rel, code_type, cond_list = GenType.gen_init_type_subtype_rel p [@@deriving show]

let _ = CondType.pp_cond_list 0 cond_list

let _ = CodeType.pp_block_types 0 code_type

(* let _ = SubType.pp_tv_rels 0 tv_rel *)

let subtype_sol = SubType.solve_vars (SubType.remove_all_var_dummy_sub tv_rel) cond_list 2
let pure_sol = SubType.get_pure_sol subtype_sol

let _= SubType.pp_tv_rels 0 subtype_sol

let addr_exp_list = InitMem.init_addr_exp p code_type

let _ = InitMem.pp_addr_exp 0 addr_exp_list

let addr_repl_list = InitMem.repl_addr_exp addr_exp_list pure_sol

let _ = InitMem.pp_addr_exp 0 addr_repl_list

let addr_range_list = InitMem.get_addr_range addr_repl_list

let _ = InitMem.pp_addr_range 0 addr_range_list


