open Printf

let initial_buffer_size = 256

let rec compile = begin fun fnames ->
  begin let rec translate = begin fun fname_in ->
    begin let fname_out = begin if ((Filename.check_suffix fname_in) ".yz") then
      ((sprintf "%s.ml") ((Filename.chop_suffix fname_in) ".yz"))
    else
      ((sprintf "%s.ml") fname_in)
    end in
    ((Trans.translate_file fname_in) fname_out)
    end
  end in
  ((List.for_all translate) fnames)
  end
end

let rec interactive = begin fun (() _) ->
  begin let rec read = begin fun buf ->
    begin
    (printf "> ");
    begin let line = (read_line ()) in
    begin if ((( = ) line) "") then
      (Buffer.contents buf)
    else
      begin
      ((Buffer.add_string buf) ((sprintf "%s\n") line));
      (read buf)
      end
    end
    end
    end
  end in
  begin try
    begin let str = (read (Buffer.create initial_buffer_size)) in
    begin
    begin match ((Trans.translate_string "<interactive>") str) with
      | (None _) ->
        ()
      | (Some (result)) ->
        begin
        ((printf "%s") result);
        (flush stdout)
        end
    end;
    (interactive ())
    end
    end
  with

    | (End_of_file _) ->
      ()
  end
  end
end

