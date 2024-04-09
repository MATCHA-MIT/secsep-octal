
open Type.Isa
open Type.Code_type
open Type.Gen_type

let p : Isa.program = 
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
]

let _ = GenType.gen_init_type_subtype_rel p [@@deriving show]

let () = print_endline "HH"

