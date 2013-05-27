
type t =
  | Expr of Expr.t

let show = function
  | Expr(expr) -> Expr.show expr
