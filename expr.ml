
type t =
  | Con of Literal.t

let show = function
  | Con(lit) -> Literal.show lit
