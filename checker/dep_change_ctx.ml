open Basic_type

module DepChangeCtx = struct
  exception DepChangeCtxerror of string
  let dep_change_ctx_error msg = raise (DepChangeCtxerror ("[Dep Change Ctx Error] " ^ msg))

  type t = DepType.ctx_t
  [@@deriving sexp]
  
end

