open Sexplib.Std


module RegTypeBasic = struct

  type ('valid, 'a) reg_content = ('valid * 'a) list
  [@@deriving sexp]

  let map (func: 'a -> 'b) (reg: ('valid, 'a) reg_content) : ('valid, 'b) reg_content =
    List.map (fun (valid, e) -> valid, func e) reg

  let mapi (func: int -> 'a -> 'b) (reg: ('valid, 'a) reg_content) : ('valid, 'b) reg_content =
    List.mapi (fun idx (valid, e) -> valid, func idx e) reg

  let map2entry (func: 'a -> 'b -> 'c) (reg1: ('valid, 'a) reg_content) (reg2: ('valid, 'b) reg_content) : 'c list =
    List.map2 (fun (_, e1) (_, e2) -> func e1 e2) reg1 reg2

  let filter_map (func: 'a -> 'b option) (reg: ('valid, 'a) reg_content) : ('valid, 'b) reg_content =
    List.filter_map (
      fun (valid, e) ->
        func e |>
        Option.map (fun x -> valid, x)
    ) reg

  let fold_left
      (func: 'acc -> 'a -> 'acc)
      (acc: 'acc) (reg1: ('valid, 'a) reg_content) : 'acc =
    List.fold_left (
      fun acc (_, e) -> func acc e
    ) acc reg1

  let fold_left_map
      (func: 'acc -> 'a -> ('acc * 'b))
      (acc: 'acc)
      (reg: ('valid, 'a) reg_content) : 'acc * (('valid, 'b) reg_content) =
    List.fold_left_map (
      fun acc (valid, e) ->
        let acc, e = func acc e in
        acc, (valid, e)
    ) acc reg

  let fold_left2
      (func: 'acc -> 'a -> 'b -> 'acc)
      (acc: 'acc) (reg1: ('valid, 'a) reg_content) (reg2: ('valid, 'b) reg_content) : 'acc =
    List.fold_left2 (
      fun acc (_, e1) (_, e2) -> func acc e1 e2
    ) acc reg1 reg2

  let find_index
      (func: 'a -> bool) (reg: ('valid, 'a) reg_content) : int option =
    List.find_index (
      fun (_, e) -> func e
    ) reg

end
