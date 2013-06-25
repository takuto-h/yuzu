open Printf

type t = 
  | Con of Literal.t
  | Var of Names.val_name
  | Variant of (Names.ctor * t)
  | Tuple of (t) list
  | Record of ((Names.val_path * (t) option)) list
  | Or of (t * t)
  | As of (t * Names.val_name)


