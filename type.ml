type t = 
  | Con of Names.typector
  | App of (Names.typector * (t) list)
  | Tuple of (t) list


