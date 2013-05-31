
open Printf

type t = {
  name : string;
}

let intern str = {
  name = str;
}

let show {name} =
  if Lexer.is_special_ident name then
    sprintf "$(%s)" name
  else
    name
