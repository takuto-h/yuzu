open Printf

type t =
  | Expr of Expr.t
  | LetFun of (Names.val_name * Expr.t)
  | LetVal of (Names.val_name * Expr.t)
  | Open of Names.mod_path
  | Variant of (Names.typector_name * ((Names.ctor_name * (Type.t) option)) list)

let rec show = begin fun top ->
  begin match top with
    | Expr(expr) ->
      begin let str_expr = (Expr.show expr) in
      ((sprintf "Expr(%s)") str_expr)
      end
    | LetFun(name, expr) ->
      begin let str_name = (Names.show_val_name name) in
      begin let str_expr = (Expr.show expr) in
      (((sprintf "LetFun(%s,%s)") str_name) str_expr)
      end
      end
    | LetVal(name, expr) ->
      begin let str_name = (Names.show_val_name name) in
      begin let str_expr = (Expr.show expr) in
      (((sprintf "LetVal(%s,%s)") str_name) str_expr)
      end
      end
    | Open(path) ->
      begin let str_path = (Names.show_mod_path path) in
      ((sprintf "Open(%s)") str_path)
      end
    | Variant(name, ctor_decls) ->
      ((sprintf "Variant(%s)") name)
  end
end

