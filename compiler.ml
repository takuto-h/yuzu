open YzPervasives

open Printf

type t = {
  inf : Inf.t;
  dummy : unit;
}

let initial_buffer_size = 256

let basic_offset = 2

let rec create = begin fun () ->
  {
    inf = (Inf.create ());
    dummy = ();
  }
end

let rec compile_file = begin fun compiler ->
  begin fun fname_in ->
    begin let chopped = begin try
      (Filename.chop_extension fname_in)
    with
      | (Invalid_argument (_)) ->
        fname_in
    end in
    begin let fname_out = ((sprintf "%s.ml") chopped) in
    begin let mod_name = (String.capitalize chopped) in
    ((with_open_in fname_in) begin fun chan_in ->
      ((with_open_out fname_out) begin fun chan_out ->
        begin let strm = (Stream.of_channel chan_in) in
        begin let src = (((Source.create fname_in) strm) Pos.File) in
        begin let lexer = (Lexer.create src) in
        begin let parser = (Parser.create lexer) in
        begin let trans = (Trans.create basic_offset) in
        begin try
          begin let rec loop = begin fun compiler ->
            begin match (Parser.parse parser) with
              | None ->
                (Some ({
                  compiler with
                  inf = ((Inf.leave_module compiler.inf) mod_name);
                }))
              | (Some (top)) ->
                begin let inf = compiler.inf in
                begin let result = ((Trans.translate_top trans) top) in
                begin
                (((fprintf chan_out) "%s\n") result);
                (loop {
                  compiler with
                  inf = inf;
                })
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
      end)
    end)
    end
    end
    end
  end
end

let rec compile_string = begin fun compiler ->
  begin fun fname_in ->
    begin fun str ->
      begin let buf_decls = (Buffer.create initial_buffer_size) in
      begin let buf_output = (Buffer.create initial_buffer_size) in
      begin let strm = (Stream.of_string str) in
      begin let src = (((Source.create fname_in) strm) (Pos.String (str))) in
      begin let lexer = (Lexer.create src) in
      begin let parser = (Parser.create lexer) in
      begin let trans = (Trans.create basic_offset) in
      begin try
        begin let rec loop = begin fun compiler ->
          begin match (Parser.parse parser) with
            | None ->
              (Some (compiler, (Buffer.contents buf_decls), (Buffer.contents buf_output)))
            | (Some (top)) ->
              begin let (inf, decls) = ((Inf.infer_top compiler.inf) top) in
              begin
              ((List.iter begin fun decl ->
                ((Buffer.add_string buf_decls) (Decl.show decl))
              end) decls);
              begin let result = ((Trans.translate_top trans) top) in
              begin
              ((Buffer.add_string buf_output) result);
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

let rec compile_files = begin fun compiler ->
  begin fun fnames ->
    begin match fnames with
      | ( [] ) ->
        (Some (compiler))
      | (( :: ) (fname_in, fnames)) ->
        begin match ((compile_file compiler) fname_in) with
          | None ->
            None
          | (Some (compiler)) ->
            ((compile_files compiler) fnames)
        end
    end
  end
end

let rec read = begin fun buf ->
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

let rec interactive = begin fun compiler ->
  begin
  (printf "---\n");
  begin try
    begin let str = (read (Buffer.create initial_buffer_size)) in
    begin match (((compile_string compiler) "<interactive>") str) with
      | None ->
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
    | End_of_file ->
      compiler
  end
  end
end

let rec load_iface_file = begin fun compiler ->
  begin fun fname_in ->
    begin let chopped = begin try
      (Filename.chop_extension fname_in)
    with
      | (Invalid_argument (_)) ->
        fname_in
    end in
    begin let mod_name = (String.capitalize chopped) in
    ((with_open_in fname_in) begin fun chan_in ->
      begin let strm = (Stream.of_channel chan_in) in
      begin let src = (((Source.create fname_in) strm) Pos.File) in
      begin let lexer = (Lexer.create src) in
      begin let parser = (Parser.create lexer) in
      begin try
        begin let rec loop = begin fun compiler ->
          begin match (Parser.parse_decl parser) with
            | None ->
              (Some ({
                compiler with
                inf = ((Inf.leave_module compiler.inf) mod_name);
              }))
            | (Some (decl)) ->
              begin let inf = ((Inf.load_decl compiler.inf) decl) in
              (loop {
                compiler with
                inf = inf;
              })
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
    end)
    end
    end
  end
end

