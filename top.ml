
type t = {pos:Pos.t; raw:raw}
and raw =
  | LetVal of Ident.t * Expr.t
  | LetFun of Ident.t * Expr.t
  | Expr of Expr.t

let at pos raw = {pos=pos; raw=raw}
