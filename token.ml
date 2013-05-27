
open Printf

type t =
  | EOF
  | Just of char
  | Int of int

let show = function
  | EOF -> "EOF"
  | Just(c) -> sprintf "'%c'" c
  | Int(_) -> "integer"
