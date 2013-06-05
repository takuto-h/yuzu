
open Printf

type t =
  | Expr of Expr.t
  | LetFun of Names.val_name * Expr.t
  | LetVal of Names.val_name * Expr.t
  | Open of Names.mod_path
  | Variant of Names.typector_name * (Names.ctor_name * Type.t) list

let show = function
  | Expr(expr) ->
    let str_expr = Expr.show expr in
    sprintf "Expr(%s)" str_expr
  | LetFun(name,expr) ->
    let str_name = Names.show_val_name name in
    let str_expr = Expr.show expr in
    sprintf "LetFun(%s,%s)" str_name str_expr
  | LetVal(name,expr) ->
    let str_name = Names.show_val_name name in
    let str_expr = Expr.show expr in
    sprintf "LetVal(%s,%s)" str_name str_expr
  | Open(path) ->
    let str_path = Names.show_mod_path path in
    sprintf "Open(%s)" str_path
  | Variant(name,ctors) ->
    sprintf "Variant(%s)" name
