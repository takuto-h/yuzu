type t = {
  fname : string;
  mutable lnum : int;
  mutable cnum : int;
  mutable bol : int;
  strm : (char) Stream.t;
}

let rec create = begin fun fname ->
  begin fun strm ->
    {
      fname = fname;
      lnum = 1;
      cnum = 0;
      bol = 0;
      strm = strm;
    }
  end
end

let rec pos = begin fun {fname;lnum;cnum;bol;} ->
  ((((Pos.make fname) lnum) cnum) bol)
end

let rec peek = begin fun {strm;} ->
  (Stream.peek strm)
end

let rec junk = begin fun src ->
  begin match (peek src) with
    | (None _) ->
      ()
    | (Some ('\n')) ->
      begin
      (src.lnum <- ((( + ) src.lnum) 1));
      begin
      (src.cnum <- ((( + ) src.cnum) 1));
      begin
      (src.bol <- src.cnum);
      (Stream.junk src.strm)
      end
      end
      end
    | (Some (_)) ->
      begin
      (src.cnum <- ((( + ) src.cnum) 1));
      (Stream.junk src.strm)
      end
  end
end

