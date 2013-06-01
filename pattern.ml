
open Printf

type t =
  | Con of Literal.t
  | Var of ValName.t

let show = function
  | Con(lit) ->
    sprintf "Con(%s)" (Literal.show lit)
  | Var(name) ->
    sprintf "Var(%s)" (ValName.show name)
