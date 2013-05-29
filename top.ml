
open Printf

type t =
  | Expr of Expr.t
  | LetFun of Ident.t * Expr.t
  | LetVal of Ident.t * Expr.t

let show = function
  | Expr(expr) ->
    let str_expr = Expr.show expr in
    sprintf "Expr(%s)" str_expr
  | LetFun(ident,expr) ->
    let str_ident = Ident.show ident in
    let str_expr = Expr.show expr in
    sprintf "LetFun(%s,%s)" str_ident str_expr
  | LetVal(ident,expr) ->
    let str_ident = Ident.show ident in
    let str_expr = Expr.show expr in
    sprintf "LetVal(%s,%s)" str_ident str_expr
