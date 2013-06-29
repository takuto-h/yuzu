type t = 
  | Variant of ((Names.ctor_name * (TypeExpr.t) option * TypeExpr.t)) list
  | Record of ((bool * Names.val_name * TypeExpr.t)) list


