open Printf

type t =
  | Expr of Expr.t
  | LetVal of (Pattern.t * Expr.t)
  | LetFun of ((Names.val_name * Expr.t)) list
  | Open of Names.mod_path
  | Abbrev of (Names.typector_name * TypeExpr.t)
  | Variant of (Names.typector_name * ((Names.ctor_name * (TypeExpr.t) option)) list)
  | Record of (Names.typector_name * ((bool * Names.val_name * TypeExpr.t)) list)
  | Exception of (Names.ctor_name * (TypeExpr.t) option)

