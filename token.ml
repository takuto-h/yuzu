
open Printf

type t =
  | EOF
  | Semi
  | Int of int

let show = function
  | EOF -> "EOF"
  | Semi -> "';'"
  | Int(_) -> "integer"
