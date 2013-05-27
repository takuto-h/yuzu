
open Printf

type t = {
  dummy : unit;
}

let create () = {
  dummy = ();
}

let translate trans = function
  | Expr.Con(Literal.Int(n)) ->
    string_of_int n

exception Break
      
let translate_file fname_in fname_out =
  let chan_in = open_in fname_in in
  try
    let chan_out = open_out fname_out in
    begin try
      let src = Source.create fname_in chan_in in
      let lexer = Lexer.create src in
      let parser = Parser.create lexer in
      let trans = create () in
      while true do
        match Parser.parse parser with
          | None ->
            raise Break
          | Some(expr) ->
            let result = translate trans expr in
            fprintf chan_out "%s\n" result
      done
    with
      | Failure(message) ->
        close_out chan_out;
        prerr_string message;
        flush stderr;
        raise Break
      | Break ->
        close_out chan_out;
        raise Break
      | exn ->
        close_out_noerr chan_out;
        raise exn
    end
  with
    | Break ->
      close_in chan_in;
    | exn ->
      close_in_noerr chan_in;
      raise exn
