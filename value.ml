
type t =
  | Con of Literal.t
  | Closure of (env ref) * Ident.t * Expr.t
  | Subr of (t -> t)

and env = (Ident.t * t) list

let show value =
  begin match value with
    | Con(lit) ->
      Literal.show lit
    | Closure(_,_,_) ->
      "<closure>"
    | Subr(_) ->
      "<subr>"
  end
