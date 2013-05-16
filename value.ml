
open Printf

type t =
  | Con of Literal.t
  | Closure of (env ref) * Ident.t * Expr.t
  | Subr of (t -> t)
  | Tuple of t list

and env = (Ident.t * t) list

let rec show value =
  begin match value with
    | Con(lit) ->
      Literal.show lit
    | Closure(_,_,_) ->
      "<closure>"
    | Subr(_) ->
      "<subr>"
    | Tuple(x::xs) ->
      sprintf "(%s)"
        (List.fold_left
           (fun acc elem -> sprintf "%s, %s" acc (show elem)) (show x) xs)
    | Tuple([]) ->
      assert false
  end
