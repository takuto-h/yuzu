
type t = {pos:Pos.t; raw:raw}
and raw =
  | LetVal of Ident.t * Expr.t
  | LetFun of Ident.t * Expr.t
  | Expr of Expr.t
  | Type of Ident.t * Type.t * (Pos.t * Ident.t * Type.t list) list

let at pos raw = {pos=pos; raw=raw}
