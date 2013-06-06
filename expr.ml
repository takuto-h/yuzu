
open Printf

type t =
  | Con of Literal.t
  | Var of Names.val_path
  | Ctor of Names.ctor
  | Abs of Names.val_name * t
  | App of t * t
  | If of t * t * t
  | Tuple of t list
  | Match of t * (Pattern.t * t) list
  | LetVal of Names.val_name * t * t

let rec show = function
  | Con(lit) ->
    sprintf "Con(%s)" (Literal.show lit)
  | Var(path) ->
    sprintf "Var(%s)" (Names.show_val_path path)
  | Ctor(ctor) ->
    sprintf "Ctor(%s)" (Names.show_ctor ctor)
  | Abs(param_name,body_expr) ->
    sprintf "Abs(%s,%s)" (Names.show_val_name param_name) (show body_expr)
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
  | LetVal(name,val_expr,cont_expr) ->
    let str_name = Names.show_val_name name in
    let str_val = show val_expr in
    let str_cont = show cont_expr in
    sprintf "LetVal(%s,%s,%s)" str_name str_val str_cont

and show_case (pat,expr) =
  sprintf "Case(%s,%s)" (Pattern.show pat) (show expr)
