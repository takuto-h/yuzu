
open Printf

type t =
  | Int of int

let show = function
  | Int(n) -> sprintf "%d" n
