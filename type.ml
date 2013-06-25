type t = 
  | Con of Names.typector
  | Var of (int * ((t) option) ref)
  | Gen of int
  | App of (Names.typector * (t) list)
  | Tuple of (t) list


