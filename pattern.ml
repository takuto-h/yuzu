open Printf

type t =
  | Con of Literal.t
  | Var of Names.val_name
  | Variant of (Names.ctor * (t) list)
  | Tuple of (t) list
  | Record of ((Names.val_path * (t) option)) list
  | Or of (t * t)

