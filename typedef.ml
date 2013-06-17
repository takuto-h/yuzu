type t =
  | Abbrev of (Names.typector_name * TypeExpr.t)
  | Variant of (Names.typector_name * ((Names.ctor_name * (TypeExpr.t) option)) list)
  | Record of (Names.typector_name * ((bool * Names.val_name * TypeExpr.t)) list)

