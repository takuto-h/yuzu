type t = 
  | Abbrev of (Names.typector_name * (TypeExpr.t) list * TypeExpr.t * TypeExpr.t)
  | Repr of (Names.typector_name * (TypeExpr.t) list * TypeInfo.t)


