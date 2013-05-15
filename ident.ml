
open Printf

type t = { name : string }

let intern str = { name = str }

let show ident =
  if Lexer.is_special_ident ident.name then
    sprintf "$|%s|" ident.name
  else
    ident.name
