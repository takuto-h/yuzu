
open Printf

type t = {
  fname : string;
  lnum : int;
  cnum : int;
  bol : int;
}

let make fname lnum cnum bol = {
  fname = fname;
  lnum = lnum;
  cnum = cnum;
  bol = bol;
}

let dummy = make "<dummy>" 1 0 0

let show {fname;lnum;cnum;bol} =
  let offset = cnum - bol in
  sprintf "%s:%d:%d" fname lnum offset
