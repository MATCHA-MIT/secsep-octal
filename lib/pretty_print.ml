module PP = struct

  let print_lvl (lvl: int) =
    for _ = 1 to lvl do
      print_string "  "
    done;
    (* Printf.printf "%s" (String.make lvl '\t'); *)
    Printf.printf

  let bprint_lvl (lvl: int) (buf: Buffer.t) =
    Printf.bprintf buf "%s" (String.make lvl '\t');
    Printf.bprintf buf

end