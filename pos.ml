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

let dummy = (((((make "<dummy>") 1) 0) 0) (String ("<dummy>")))

let rec show = begin fun {fname;lnum;cnum;bol;} ->
  begin let offset = ((( - ) cnum) bol) in
  ((((sprintf "%s:%d:%d") fname) lnum) offset)
  end
end

let rec show_source = begin fun {fname;lnum;cnum;bol;source;} ->
  begin let offset = ((( - ) cnum) bol) in
  begin let str_anchor = ((String.make ((( + ) offset) 1)) ' ') in
  begin
  (((String.set str_anchor) offset) '^');
  begin match source with
    | (File _) ->
      ((with_open_in fname) begin fun chan_in ->
        begin try
          begin
          ((seek_in chan_in) bol);
          begin let str_line = (input_line chan_in) in
          (((sprintf "%s\n%s\n") str_line) str_anchor)
          end
          end
        with

          | (End_of_file _) ->
            ""
        end
      end)
    | (String (str)) ->
      begin let str = (((String.sub str) bol) ((( - ) (String.length str)) bol)) in
      begin let str_line = begin try
        (((String.sub str) 0) ((String.index str) '\n'))
      with

        | (Not_found _) ->
          str
      end in
      (((sprintf "%s\n%s\n") str_line) str_anchor)
      end
      end
  end
  end
  end
  end
end

let rec show_message = begin fun pos ->
  begin fun message ->
    ((((sprintf "%s: %s%s") (show pos)) message) (show_source pos))
  end
end

let rec show_error = begin fun pos ->
  begin fun message ->
    ((show_message pos) ((sprintf "error: %s") message))
  end
end

