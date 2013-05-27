
type t =
  | Int of int

let show = function
  | Int(n) -> string_of_int n
