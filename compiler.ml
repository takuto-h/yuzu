open Printf

let initial_buffer_size = 256

let basic_offset = 2

let rec with_open_in = begin fun fname_in ->
  begin fun func ->
    begin let chan_in = (open_in fname_in) in
    begin try
      begin let x = (func chan_in) in
      begin
      (close_in chan_in);
      x
      end
      end
    with

      | exn ->
        begin
        (close_in_noerr chan_in);
        (raise exn)
        end
    end
    end
  end
end

let rec with_open_out = begin fun fname_out ->
  begin fun func ->
    begin let chan_out = (open_out fname_out) in
    begin try
      begin let x = (func chan_out) in
      begin
      (close_out chan_out);
      x
      end
      end
    with

      | exn ->
        begin
        (close_out_noerr chan_out);
        (raise exn)
        end
    end
    end
  end
end

let rec compile_file = begin fun fname_in ->
  begin fun fname_out ->
    ((with_open_in fname_in) begin fun chan_in ->
      ((with_open_out fname_out) begin fun chan_out ->
        begin let strm = (Stream.of_channel chan_in) in
        begin let src = ((Source.create fname_in) strm) in
        begin let lexer = (Lexer.create src) in
        begin let parser = (Parser.create lexer) in
        begin let trans = (Trans.create basic_offset) in
        begin try
          begin let rec loop = begin fun (() _) ->
            begin match (Parser.parse parser) with
              | (None _) ->
                true
              | (Some (top)) ->
                begin let result = ((Trans.translate_top trans) top) in
                begin
                (((fprintf chan_out) "%s\n") result);
                (loop ())
                end
                end
            end
          end in
          (loop ())
          end
        with

          | (Failure (message)) ->
            begin
            ((eprintf "%s") message);
            begin
            (flush stderr);
            false
            end
            end
        end
        end
        end
        end
        end
        end
      end)
    end)
  end
end

let rec compile_string = begin fun fname ->
  begin fun str ->
    begin let decls = (Buffer.create initial_buffer_size) in
    begin let output = (Buffer.create initial_buffer_size) in
    begin let strm = (Stream.of_string str) in
    begin let src = ((Source.create fname) strm) in
    begin let lexer = (Lexer.create src) in
    begin let parser = (Parser.create lexer) in
    begin let trans = (Trans.create basic_offset) in
    begin let inf = (Inf.create ()) in
    begin try
      begin let rec loop = begin fun inf ->
        begin match (Parser.parse parser) with
          | (None _) ->
            (Some ((Buffer.contents decls), (Buffer.contents output)))
          | (Some (top)) ->
            begin let (inf, decl) = ((Inf.infer_top inf) top) in
            begin
            ((Buffer.add_string decls) (Decl.show decl));
            begin let result = ((Trans.translate_top trans) top) in
            begin
            ((Buffer.add_string output) result);
            (loop inf)
            end
            end
            end
            end
        end
      end in
      (loop inf)
      end
    with

      | (Failure (message)) ->
        begin
        ((eprintf "%s") message);
        begin
        (flush stderr);
        None
        end
        end
    end
    end
    end
    end
    end
    end
    end
    end
    end
  end
end

let rec compile = begin fun fnames ->
  begin let rec comp = begin fun fname_in ->
    begin let fname_out = begin if ((Filename.check_suffix fname_in) ".yz") then
      ((sprintf "%s.ml") ((Filename.chop_suffix fname_in) ".yz"))
    else
      ((sprintf "%s.ml") fname_in)
    end in
    ((compile_file fname_in) fname_out)
    end
  end in
  ((List.for_all comp) fnames)
  end
end

let rec read = begin fun buf ->
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
end

let rec interactive = begin fun (() _) ->
  begin try
    begin let str = (read (Buffer.create initial_buffer_size)) in
    begin
    begin match ((compile_string "<interactive>") str) with
      | (None _) ->
        ()
      | (Some (decls, output)) ->
        begin
        ((printf "decls:\n%s") decls);
        ((printf "output:\n%s") output)
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

