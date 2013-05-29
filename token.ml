
open Printf

type t =
  | EOF
  | Just of char
  | Int of int
  | Ident of string
  | Def

let show = function
  | EOF -> "EOF"
  | Just(c) -> sprintf "'%c'" c
  | Int(_) -> "integer"
  | Ident(_) -> "identifier"
  | Def -> "'def'"
