
open Printf

type t =
  | EOF
  | Def
  | Var
  | If
  | Else
  | Newline
  | Undent
  | Int of int
  | Ident of string
  | AddOp of string
  | Just of char

let show = function
  | EOF -> "EOF"
  | Def -> "'def'"
  | Var -> "'var'"
  | If -> "'if'"
  | Else -> "'else'"
  | Newline -> "newline"
  | Undent -> "undent"
  | Int(_) -> "integer"
  | Ident(_) -> "identifier"
  | AddOp(s) -> sprintf "'%s'" s
  | Just(c) -> sprintf "'%c'" c
