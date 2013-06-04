
open Printf

type t =
  | Con of Literal.t
  | Var of Names.val_name

let show = function
  | Con(lit) ->
    sprintf "Con(%s)" (Literal.show lit)
  | Var(name) ->
    sprintf "Var(%s)" (Names.show_val_name name)
