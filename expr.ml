
open Printf

type t =
  | Con of Literal.t
  | Var of Path.t
  | Abs of Name.t * t
  | App of t * t
  | If of t * t * t
  | Tuple of t list

let rec show = function
  | Con(lit) ->
    sprintf "Con(%s)" (Literal.show lit)
  | Var(path) ->
    sprintf "Var(%s)" (Path.show path)
  | Abs(param_name,body_expr) ->
    sprintf "Abs(%s,%s)" (Name.show param_name) (show body_expr)
  | App(fun_expr,arg_expr) ->
    sprintf "App(%s,%s)" (show fun_expr) (show arg_expr)
  | If(cond_expr,then_expr,else_expr) ->
    sprintf "If(%s,%s,%s)" (show cond_expr) (show then_expr) (show else_expr)
  | Tuple(x::xs) ->
    sprintf "Tuple([%s])"
      (List.fold_left (fun acc elem -> sprintf "%s,%s" acc (show elem)) (show x) xs)
  | Tuple([]) ->
    assert false
