
open Printf

type t =
  | Int of int
  | String of string
  | Char of string

let show = function
  | Int(n) -> sprintf "%d" n
  | String(str) -> sprintf "\"%s\"" str
  | Char(str) -> sprintf "'%s'" str
