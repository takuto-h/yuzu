
type t =
  | LetVal of Ident.t * Expr.t
  | LetFun of Ident.t * Expr.t
  | Expr of Expr.t
