
open Printf

type t = {
  src_fname : string;
  mutable src_lnum : int;
  mutable src_cnum : int;
  mutable src_bol : int;
  src_strm : char Stream.t;
}

type pos = {
  pos_fname : string;
  pos_lnum : int;
  pos_cnum : int;
  pos_bol : int;
}

let create fname chan = {
  src_fname = fname;
  src_lnum = 1;
  src_cnum = 0;
  src_bol = 0;
  src_strm = Stream.of_channel chan;
}

let pos src = {
  pos_fname = src.src_fname;
  pos_lnum = src.src_lnum;
  pos_cnum = src.src_cnum;
  pos_bol = src.src_bol;
}

let peek src =
  Stream.peek src.src_strm

let junk src =
  begin match peek src with
    | None ->
      ()
    | Some('\n') -> begin
      src.src_lnum <- src.src_lnum + 1;
      src.src_cnum <- src.src_cnum + 1;
      src.src_bol <- src.src_cnum;
      Stream.junk src.src_strm
    end
    | Some(_) -> begin
      src.src_cnum <- src.src_cnum + 1;
      Stream.junk src.src_strm
    end
  end
