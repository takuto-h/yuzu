
open Printf

type t = {
  fname : string;
  lnum : int;
  cnum : int;
  bol : int;
}

let make fname lnum cnum bol = {
  fname=fname;
  lnum=lnum;
  cnum=cnum;
  bol=bol;
}

let dummy = make "<dummy>" 1 0 0
  
let show {fname;lnum;cnum;bol} =
  let offset = cnum - bol in
  sprintf "%s:%d:%d" fname lnum offset

let show_source {fname;lnum;cnum;bol} =
  let offset = cnum - bol in
  let in_channel = open_in fname in
  begin try begin
    seek_in in_channel bol;
    let str_line = input_line in_channel in
    let str_anchor = String.make (offset + 1) ' ' in begin
    String.set str_anchor offset '^';
    close_in in_channel;
    sprintf "%s\n%s\n" str_line str_anchor
    end
  end
  with
    | exn -> begin
      close_in_noerr in_channel;
      raise exn
    end
  end
