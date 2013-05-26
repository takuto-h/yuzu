
type t = {pos:Pos.t; raw:raw}
and raw =
  | Con of Literal.t
  | Var of Ident.t
  | Abs of Ident.t * t
  | App of t * t
  | LetVal of Ident.t * t * t
  | LetFun of Ident.t * t * t
  | If of t * t * t
  | Tuple of t list
  | LetTuple of Ident.t list * t * t

let at pos raw = {pos=pos; raw=raw}
  