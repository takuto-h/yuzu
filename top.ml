
open Printf

type t =
  | Expr of Expr.t
  | LetFun of Name.t * Expr.t
  | LetVal of Name.t * Expr.t

let show = function
  | Expr(expr) ->
    let str_expr = Expr.show expr in
    sprintf "Expr(%s)" str_expr
  | LetFun(name,expr) ->
    let str_name = Name.show name in
    let str_expr = Expr.show expr in
    sprintf "LetFun(%s,%s)" str_name str_expr
  | LetVal(name,expr) ->
    let str_name = Name.show name in
    let str_expr = Expr.show expr in
    sprintf "LetVal(%s,%s)" str_name str_expr
