open Printf

type t = {
  fname : string;
  lnum : int;
  cnum : int;
  bol : int;
}

let rec make = begin fun fname ->
  begin fun lnum ->
    begin fun cnum ->
      begin fun bol ->
        {
          fname = fname;
          lnum = lnum;
          cnum = cnum;
          bol = bol;
        }
      end
    end
  end
end

let dummy = ((((make "<dummy>") 1) 0) 0)

let rec show = begin fun {fname;lnum;cnum;bol;} ->
  begin let offset = ((( - ) cnum) bol) in
  ((((sprintf "%s:%d:%d") fname) lnum) offset)
  end
end

