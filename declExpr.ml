type t = 
  | Val of (Names.val_name * TypeExpr.t)
  | AbstrType of (Names.typector_name * int)
  | ConcrType of TypeDef.t
  | Exception of (Names.ctor_name * (TypeExpr.t) option * TypeExpr.t)


