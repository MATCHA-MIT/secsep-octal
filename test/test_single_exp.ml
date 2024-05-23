open Type.Single_exp
open Type.Pretty_print

let x: SingleExp.t = SingleVar 1
let y: SingleExp.t = SingleVar 2
let z: SingleExp.t = SingleVar 3

let test_cases : SingleExp.t list = [
  SingleBExp (
    SingleAdd,
    SingleBExp (
      SingleAdd,
      SingleConst 1,
      SingleVar 1
    ),
    SingleBExp (
      SingleMul,
      SingleBExp (
        SingleAdd,
        SingleConst 5,
        SingleVar 1
      ),
      SingleBExp (
        SingleAdd,
        SingleVar 1,
        SingleConst 2
      )
    )
  );

  SingleBExp (
    SingleAdd,
    SingleBExp (SingleSal, SingleVar 1, SingleConst 2),
    SingleBExp (
      SingleAdd,
      SingleBExp (SingleMul, SingleBExp (SingleAdd, SingleVar 2, SingleConst 3), SingleBExp (SingleAdd, SingleConst 0, SingleVar 3)),
      SingleVar 1
    )
  );

  SingleBExp (
    SingleAdd,
    SingleBExp (
      SingleAdd,
      SingleBExp (SingleAdd, x, y),
      SingleBExp (SingleMul, z, SingleConst 2)
    ),
    SingleBExp (
      SingleAdd,
      SingleBExp (SingleAdd, SingleConst 3, z),
      SingleBExp (SingleAdd, x, x)
    )
  );

  SingleBExp (
    SingleXor,
    SingleBExp (SingleAdd, SingleConst 1, SingleConst 2),
    SingleBExp (SingleMul, SingleConst 5, SingleConst 5)
  );

  SingleBExp (
    SingleAnd,
    SingleBExp (SingleAdd, SingleConst 1, SingleConst 2),
    SingleBExp (SingleMul, SingleConst 5, SingleConst 5)
  )
]

let _ = List.map (
  fun x -> 
    let split_eval_x = SingleExp.eval_t x in
    let eval_x = SingleExp.convert_t split_eval_x in
    PP.print_lvl 0 "=======================\n";
    SingleExp.pp_split_exp 0 split_eval_x;
    PP.print_lvl 0 "-----------------------\n";
    SingleExp.pp_single_exp 0 eval_x;
    Printf.printf "\n"
) test_cases

