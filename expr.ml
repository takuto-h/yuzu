open Printf

type t =
  | Con of Literal.t
  | Var of Names.val_path
  | Ctor of Names.ctor
  | Abs of (Pattern.t * t)
  | App of (t * t)
  | If of (t * t * t)
  | Tuple of (t) list
  | Record of ((Names.val_path * t)) list
  | Match of (t * ((Pattern.t * (t) option * t)) list)
  | LetVal of (Pattern.t * t * t)
  | LetFun of (((Names.val_name * t)) list * t)
  | Or of (t * t)
  | And of (t * t)
  | Seq of (t * t)
  | Field of (t * Names.val_path)
  | Assign of (t * t)
  | Try of (t * ((Pattern.t * (t) option * t)) list)

