
open Printf

type t = {
  mutable src_lnum : int;
  mutable src_cnum : int;
  mutable src_bol : int;
  src_strm : CharStream.t;
}

type pos = {
  pos_lnum : int;
  pos_cnum : int;
  pos_bol : int;
  pos_strm : CharStream.t;
}
    
let of_string str = {
  src_lnum = 1;
  src_cnum = 0;
  src_bol = 0;
  src_strm = CharStream.of_string str;
}

let peek src =
  CharStream.peek src.src_strm

let junk src =
  begin match peek src with
    | None ->
      ()
    | Some('\n') -> begin
      src.src_lnum <- src.src_lnum + 1;
      src.src_cnum <- src.src_cnum + 1;
      src.src_bol <- src.src_cnum;
      CharStream.junk src.src_strm
    end
    | Some(_) -> begin
      src.src_cnum <- src.src_cnum + 1;
      CharStream.junk src.src_strm
    end
  end

let pos src = {
  pos_lnum = src.src_lnum;
  pos_cnum = src.src_cnum;
  pos_bol = src.src_bol;
  pos_strm = src.src_strm;
}

let show_line {pos_bol=bol; pos_cnum=cnum; pos_strm=strm} =
  let offset = cnum - bol in
  let str_line = CharStream.show_line strm bol in
  let str_anchor = String.make (offset + 1) ' ' in begin
  String.set str_anchor offset '^';
  sprintf "%s%s\n" str_line str_anchor
  end

let show_pos {pos_lnum=lnum; pos_cnum=cnum; pos_bol=bol} =
  let offset = cnum - bol in
  sprintf "%s:%s" (string_of_int lnum) (string_of_int offset)
