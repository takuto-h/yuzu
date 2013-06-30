open YzPervasives

open Printf

type source = 
  | File
  | String of string


type t = {
  fname : string;
  lnum : int;
  cnum : int;
  bol : int;
  source : source;
}

let rec make = begin fun fname ->
  begin fun lnum ->
    begin fun cnum ->
      begin fun bol ->
        begin fun source ->
          {
            fname = fname;
            lnum = lnum;
            cnum = cnum;
            bol = bol;
            source = source;
          }
        end
      end
    end
  end
end

let dummy = (((((make "<dummy>") 1) 0) 0) (String "<dummy>"))

