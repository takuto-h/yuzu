
open Printf

type t = {
  line_num : int;
  offset : int;
}

let make lnum off = {
  line_num = lnum;
  offset = off;
}

let show {line_num=lnum;offset=off} =
  sprintf "%d:%d" lnum off
