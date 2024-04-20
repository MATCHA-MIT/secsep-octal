
open Type.Isa
open Type.Code_type
open Type.Gen_type
open Type.Subtype
open Type.Parser

let _ : Isa.program = 
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
];;


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

let _ = CodeType.pp_cond_list 0 cond_list

let _ = CodeType.pp_block_types 0 code_type

let _ = SubType.pp_tv_rels 0 tv_rel

