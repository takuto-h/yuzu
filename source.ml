
open Printf

type t = {
  fname : string;
  mutable lnum : int;
  mutable cnum : int;
  mutable bol : int;
  strm : char Stream.t;
}

let create fname chan = {
  fname = fname;
  lnum = 1;
  cnum = 0;
  bol = 0;
  strm = Stream.of_channel chan;
}

let pos src =
  Pos.make src.fname src.lnum src.cnum src.bol
    
let peek src =
  Stream.peek src.strm

let junk src =
  begin match peek src with
    | None ->
      ()
    | Some('\n') -> begin
      src.lnum <- src.lnum + 1;
      src.cnum <- src.cnum + 1;
      src.bol <- src.cnum;
      Stream.junk src.strm
    end
    | Some(_) -> begin
      src.cnum <- src.cnum + 1;
      Stream.junk src.strm
    end
  end
