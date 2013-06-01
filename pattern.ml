
open Printf

type t =
  | Var of ValName.t

let show = function
  | Var(name) ->
    sprintf "Var(%s)" (ValName.show name)
