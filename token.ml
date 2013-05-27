
type t =
  | EOF
  | Int of int

let show = function
  | EOF -> "EOF"
  | Int(_) -> "integer"
