
open Printf

type t =
  | Expr of Expr.t
  | LetFun of Ident.t * Ident.t * Expr.t

let show = function
  | Expr(expr) ->
    let str_expr = Expr.show expr in
    sprintf "Expr(%s)" str_expr
  | LetFun(fun_ident,param_ident,body_expr) ->
    let str_fun = Ident.show fun_ident in
    let str_param = Ident.show param_ident in
    let str_body = Expr.show body_expr in
    sprintf "LetFun(%s,%s,%s)" str_fun str_param str_body
