type t = 
  | Val of (Names.val_name * TypeExpr.t)
  | AbstrType of (Names.typector_name * int)
  | ConcrType of (TypeDef.t) list
  | Exception of ExnDecl.t


