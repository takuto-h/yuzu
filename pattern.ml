
open Printf

type t =
  | Con of Literal.t
  | Var of Names.val_name
  | Variant of Names.cstr * t

let rec show = function
  | Con(lit) ->
    sprintf "Con(%s)" (Literal.show lit)
  | Var(name) ->
    sprintf "Var(%s)" (Names.show_val_name name)
  | Variant(cstr,pat) ->
    sprintf "Variant(%s,%s)" (Names.show_cstr cstr) (show pat)
