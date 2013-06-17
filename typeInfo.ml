type t = 
  | Abbrev of TypeExpr.t
  | Variant of ((Names.ctor_name * (TypeExpr.t) option)) list
  | Record of ((bool * Names.val_name * TypeExpr.t)) list


