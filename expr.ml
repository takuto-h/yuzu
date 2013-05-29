
open Printf

type t =
  | Con of Literal.t
  | Var of Ident.t
  | Abs of Ident.t * t
  | App of t * t

let rec show = function
  | Con(lit) ->
    sprintf "Con(%s)" (Literal.show lit)
  | Var(ident) ->
    sprintf "Var(%s)" (Ident.show ident)
  | Abs(param_ident,body_expr) ->
    sprintf "Abs(%s,%s)" (Ident.show param_ident) (show body_expr)
  | App(fun_expr,arg_expr) ->
    sprintf "App(%s,%s)" (show fun_expr) (show arg_expr)
