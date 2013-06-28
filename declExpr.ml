type t = 
  | AbstrType of (Names.typector_name * int)
  | Val of (Names.val_name * TypeExpr.t)


