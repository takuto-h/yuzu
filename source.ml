type t = {
  fname : string;
  mutable lnum : int;
  mutable cnum : int;
  mutable bol : int;
  strm : (char) Stream.t;
  source : Pos.source;
}

let rec create = begin fun fname ->
  begin fun strm ->
    begin fun source ->
      {
        fname = fname;
        lnum = 1;
        cnum = 0;
        bol = 0;
        strm = strm;
        source = source;
      }
    end
  end
end

let rec pos = begin fun {fname;lnum;cnum;bol;source;} ->
  (((((Pos.make fname) lnum) cnum) bol) source)
end

let rec peek = begin fun {strm;} ->
  (Stream.peek strm)
end

let rec junk = begin fun src ->
  begin match (peek src) with
    | None ->
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

