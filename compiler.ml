open Printf

type t = {
  inf : Inf.t;
  dummy : unit;
}

let initial_buffer_size = 256

let basic_offset = 2

let rec create = begin fun (() _) ->
  {
    inf = (Inf.create ());
    dummy = ();
  }
end

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

let rec compile_file = begin fun compiler ->
  begin fun fname_in ->
    begin let fname_out = begin try
      ((sprintf "%s.ml") (Filename.chop_extension fname_in))
    with

      | (Invalid_argument (_)) ->
        ((sprintf "%s.ml") fname_in)
    end in
    ((with_open_in fname_in) begin fun chan_in ->
      ((with_open_out fname_out) begin fun chan_out ->
        begin let strm = (Stream.of_channel chan_in) in
        begin let src = ((Source.create fname_in) strm) in
        begin let lexer = (Lexer.create src) in
        begin let parser = (Parser.create lexer) in
        begin let trans = (Trans.create basic_offset) in
        begin try
          begin let rec loop = begin fun compiler ->
            begin match (Parser.parse parser) with
              | (None _) ->
                (Some (compiler))
              | (Some (top)) ->
                begin let result = ((Trans.translate_top trans) top) in
                begin
                (((fprintf chan_out) "%s\n") result);
                (loop compiler)
                end
                end
            end
          end in
          (loop compiler)
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
      end)
    end)
    end
  end
end

let rec compile_string = begin fun compiler ->
  begin fun fname_in ->
    begin fun str ->
      begin let decls = (Buffer.create initial_buffer_size) in
      begin let output = (Buffer.create initial_buffer_size) in
      begin let strm = (Stream.of_string str) in
      begin let src = ((Source.create fname_in) strm) in
      begin let lexer = (Lexer.create src) in
      begin let parser = (Parser.create lexer) in
      begin let trans = (Trans.create basic_offset) in
      begin try
        begin let rec loop = begin fun compiler ->
          begin match (Parser.parse parser) with
            | (None _) ->
              (Some (compiler, (Buffer.contents decls), (Buffer.contents output)))
            | (Some (top)) ->
              begin let (inf, decl) = ((Inf.infer_top compiler.inf) top) in
              begin
              ((Buffer.add_string decls) (Decl.show decl));
              begin let result = ((Trans.translate_top trans) top) in
              begin
              ((Buffer.add_string output) result);
              (loop {
                compiler with
                inf = inf;
              })
              end
              end
              end
              end
          end
        end in
        (loop compiler)
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

let rec compile = begin fun compiler ->
  begin fun fnames ->
    begin match fnames with
      | ([] _) ->
        (Some (compiler))
      | (( :: ) (fname_in, fnames)) ->
        begin match ((compile_file compiler) fname_in) with
          | (None _) ->
            None
          | (Some (compiler)) ->
            ((compile compiler) fnames)
        end
    end
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

let rec interactive = begin fun compiler ->
  begin try
    begin let str = (read (Buffer.create initial_buffer_size)) in
    begin match (((compile_string compiler) "<interactive>") str) with
      | (None _) ->
        (interactive compiler)
      | (Some (compiler, decls, output)) ->
        begin
        ((printf "decls:\n%s") decls);
        begin
        ((printf "output:\n%s") output);
        (interactive compiler)
        end
        end
    end
    end
  with

    | (End_of_file _) ->
      compiler
  end
end

