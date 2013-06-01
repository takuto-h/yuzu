
open Printf

type t = {
  str : string;
}

let make str = {
  str = str;
}

let show {str} =
  if Lexer.is_special_ident str then
    sprintf "$(%s)" str
  else
    str
