
type t =
  | Con of Literal.t
  | Closure of (env ref) * Ident.t * Expr.t

and env = (Ident.t * t) list

let show value =
  begin match value with
    | Con(lit) ->
      Literal.show lit
    | Closure(_,_,_) ->
      "<closure>"
  end
