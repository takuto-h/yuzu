
open Printf

type t =
  | Expr of Expr.t
  | LetFun of ValName.t * Expr.t
  | LetVal of ValName.t * Expr.t
  | Open of ModPath.t

let show = function
  | Expr(expr) ->
    let str_expr = Expr.show expr in
    sprintf "Expr(%s)" str_expr
  | LetFun(name,expr) ->
    let str_name = ValName.show name in
    let str_expr = Expr.show expr in
    sprintf "LetFun(%s,%s)" str_name str_expr
  | LetVal(name,expr) ->
    let str_name = ValName.show name in
    let str_expr = Expr.show expr in
    sprintf "LetVal(%s,%s)" str_name str_expr
  | Open(path) ->
    let str_path = ModPath.show path in
    sprintf "Open(%s)" str_path
