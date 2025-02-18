open Type.Isa_basic
open Sexplib.Std

module IsaFlagConfig = struct
  exception IsaFlagConfigError of string

  let isa_flag_config_error msg = raise (IsaFlagConfigError ("[Isa Flag Config Error] " ^ msg))

  type 'a flag_map_t = (IsaBasic.flag * 'a) list
  [@@deriving sexp]

  type flag_list_t = IsaBasic.flag list
  [@@deriving sexp]

  type t = flag_list_t * (bool flag_map_t) 
  (* src flag list, (dst flag, conditional update) list *)
  [@@deriving sexp]

  let set_flag_list 
      (orig_list: 'a flag_map_t) 
      (update_flag_list: 'a flag_map_t) : 'a flag_map_t =
    (* A helper function for easy config, feel free to use or not use it. *)
    (* <TODO> please check this, might containt bug *)
    List.fold_left (
      fun (acc: (IsaBasic.flag * t) list) (update_entry: IsaBasic.flag * 'a) ->
        let update_flag, _ = update_entry in
        let find, acc =
          List.fold_left_map (
            fun (acc: bool) (entry: IsaBasic.flag * 'a) ->
              if acc then acc, entry
              else if update_flag = fst entry then
                true, update_entry
              else false, entry
          ) false acc
        in
        if find then acc
        else isa_flag_config_error "set_flag_list cannot find flag in orig_list"
    ) orig_list update_flag_list
  
  let get_bop_config (bop: IsaBasic.bop) : t =
    match bop with
    | Add | Sub -> 
      [], (* flags used to calculate dest value *) 
      [ CF, false; PF, false; AF, false; ZF, false; SF, false; OF, false ]
      (* flags being updated and whether the new flag value depends on the old value *)
    | Adc | Sbb ->
      [ CF ],
      [ CF, false; PF, false; AF, false; ZF, false; SF, false; OF, false ]
    | Mul | Imul ->
      [],
      [ CF, false; OF, false ]
    | Sal | Sar | Shl | Shr ->
      [], (* the dest value does not depends on any flags *)
      [ CF, false; PF, false; ZF, false; SF, false; OF, true ]
      (* but the new OF value may depends on the old OF value, 
         so if we calculate taint of OF, we need to include it as one of the taint src *)
    | Rol | Ror ->
      [],
      [ CF, false; OF, true ]
    | Xor | And | Or -> 
      [],
      [ CF, false; PF, false; ZF, false; SF, false; OF, false ]
    | CmovEq -> [ ZF ], []
    | Bt -> [], [ CF, false ]
    | Punpck | Packxs -> [], []
    | Pshuf -> [], []
    | Padd | Psub | Pxor | Pandn | Pand | Por -> [], []
    | Psll | Psrl -> [], []
    | Xorp -> [], []

  let get_uop_config (uop: IsaBasic.uop) : t =
    match uop with
    | Neg -> [], [ CF, false; PF, false; AF, false; ZF, false; SF, false; OF, false ]
    | Inc -> [], [ PF, false; AF, false; ZF, false; SF, false; OF, false ]
    | Dec -> [], [ PF, false; AF, false; ZF, false; SF, false; OF, false ]
    | _ -> [], []
  
  let get_top_config (top: IsaBasic.top) : t =
    match top with
    | Shld
    | Shrd -> [], [ CF, false; PF, false; ZF, false; SF, false; OF, false ]
  
  let get_xchg_config : t = [], []
  
  let get_cmp_config : t = [], [ CF, false; PF, false; AF, false; ZF, false; SF, false; OF, false ]
  
  let get_test_config : t = [], [ CF, false; PF, false; AF, false; ZF, false; SF, false; OF, false ]
  
  let get_push_config : t = [], []
  
  let get_pop_config : t = [], []

  let get_repmovs_config : t = [], []
  
  let get_replods_config : t = [], []
  
  let get_repstos_config : t = [], []

  let get_call_config : t = [], []
  
end
