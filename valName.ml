
open Printf

type t = {
  value : string;
}

let make str = {
  value = str;
}

let show {value=str} =
  if Lexer.is_special_ident str then
    sprintf "$(%s)" str
  else
    str
