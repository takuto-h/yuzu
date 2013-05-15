
open Printf

type t =
  | EOF
  | Def
  | Var
  | True
  | False
  | If
  | Else
  | RArrow
  | EQ
  | Int of int
  | Ident of string
  | Just of char

let show token =
  begin match token with
    | EOF -> "EOF"
    | Def -> "'def'"
    | Var -> "'var'"
    | True -> "'true'"
    | False -> "'false'"
    | If -> "'if'"
    | Else -> "'else'"
    | RArrow -> "'->'"
    | EQ -> "'=='"
    | Int(_) -> "integer"
    | Ident(_) -> "identifier"
    | Just(c) -> sprintf "'%c'" c
  end
