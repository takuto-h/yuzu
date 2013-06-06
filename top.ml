open Printf

type t =
  | Expr of Expr.t
  | LetVal of (Pattern.t * Expr.t)
  | LetFun of (Names.val_name * Expr.t)
  | Open of Names.mod_path
  | Abbrev of (Names.typector_name * Type.t)
  | Variant of (Names.typector_name * ((Names.ctor_name * (Type.t) option)) list)
  | Record of (Names.typector_name * ((Names.val_name * Type.t)) list)

let rec show = begin fun top ->
  begin match top with
    | (Expr(expr)) ->
      begin let str_expr = (Expr.show expr) in
      ((sprintf "Expr(%s)") str_expr)
      end
    | (LetVal(pat, expr)) ->
      begin let str_pat = (Pattern.show pat) in
      begin let str_expr = (Expr.show expr) in
      (((sprintf "LetVal(%s,%s)") str_pat) str_expr)
      end
      end
    | (LetFun(name, expr)) ->
      begin let str_name = (Names.show_val_name name) in
      begin let str_expr = (Expr.show expr) in
      (((sprintf "LetFun(%s,%s)") str_name) str_expr)
      end
      end
    | (Open(path)) ->
      begin let str_path = (Names.show_mod_path path) in
      ((sprintf "Open(%s)") str_path)
      end
    | (Abbrev(name, t)) ->
      ((sprintf "Abbrev(%s)") name)
    | (Variant(name, ctor_decls)) ->
      ((sprintf "Variant(%s)") name)
    | (Record(name, field_decls)) ->
      ((sprintf "Record(%s)") name)
  end
end

