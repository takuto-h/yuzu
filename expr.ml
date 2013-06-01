
open Printf

type t =
  | Con of Literal.t
  | Var of ValPath.t
  | Abs of ValName.t * t
  | App of t * t
  | If of t * t * t
  | Tuple of t list
  | Match of t * (Pattern.t * t) list

let rec show = function
  | Con(lit) ->
    sprintf "Con(%s)" (Literal.show lit)
  | Var(path) ->
    sprintf "Var(%s)" (ValPath.show path)
  | Abs(param_name,body_expr) ->
    sprintf "Abs(%s,%s)" (ValName.show param_name) (show body_expr)
  | App(fun_expr,arg_expr) ->
    sprintf "App(%s,%s)" (show fun_expr) (show arg_expr)
  | If(cond_expr,then_expr,else_expr) ->
    sprintf "If(%s,%s,%s)" (show cond_expr) (show then_expr) (show else_expr)
  | Tuple(x::xs) ->
    sprintf "Tuple([%s])"
      (List.fold_left (fun acc elem -> sprintf "%s;%s" acc (show elem)) (show x) xs)
  | Tuple([]) ->
    assert false
  | Match(target_expr,[]) ->
    sprintf "Match(%s,[])" (show target_expr)
  | Match(target_expr,case::cases) ->
    sprintf "Match(%s,[%s])" (show target_expr)
      (List.fold_left begin fun acc elem ->
        sprintf "%s;%s" acc (show_case elem)
      end (show_case case) cases)

and show_case (pat,expr) =
  sprintf "Case(%s,%s)" (Pattern.show pat) (show expr)
