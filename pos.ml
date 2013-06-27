open YzPervasives

open Printf

type t = {
  is_file : bool;
  fname : string;
  lnum : int;
  cnum : int;
  bol : int;
}

let rec make = begin fun is_file ->
  begin fun fname ->
    begin fun lnum ->
      begin fun cnum ->
        begin fun bol ->
          {
            is_file = is_file;
            fname = fname;
            lnum = lnum;
            cnum = cnum;
            bol = bol;
          }
        end
      end
    end
  end
end

let dummy = (((((make false) "<dummy>") 1) 0) 0)

let rec show = begin fun {fname;lnum;cnum;bol;} ->
  begin let offset = ((( - ) cnum) bol) in
  ((((sprintf "%s:%d:%d") fname) lnum) offset)
  end
end

let rec show_source = begin fun {is_file;fname;lnum;cnum;bol;} ->
  begin if (not is_file) then
    ""
  else
    begin let offset = ((( - ) cnum) bol) in
    ((with_open_in fname) begin fun chan_in ->
      begin try
        begin
        ((seek_in chan_in) bol);
        begin let str_line = (input_line chan_in) in
        begin let str_anchor = ((String.make ((( + ) offset) 1)) ' ') in
        begin
        (((String.set str_anchor) offset) '^');
        (((sprintf "%s\n%s\n") str_line) str_anchor)
        end
        end
        end
        end
      with

        | (End_of_file _) ->
          ""
      end
    end)
    end
  end
end

