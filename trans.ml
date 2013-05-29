
open Printf

type t = {
  dummy : unit;
}

let create () = {
  dummy = ();
}

let translate_ident {Ident.name} =
  name

let rec translate_expr trans = function
  | Expr.Con(Literal.Int(n)) ->
    sprintf "%d" n
  | Expr.Var(ident) ->
    translate_ident ident
  | Expr.Abs(param_ident,body_expr) ->
    let str_param = translate_ident param_ident in
    let str_body = translate_expr trans body_expr in
    sprintf "(fun %s -> %s)" str_param str_body
  | Expr.App(fun_expr,arg_expr) ->
    let str_fun = translate_expr trans fun_expr in
    let str_arg = translate_expr trans arg_expr in
    sprintf "(%s %s)" str_fun str_arg

let translate_top trans = function
  | Top.Expr(expr) ->
    sprintf "let _ = %s\n" (translate_expr trans expr)
      
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
          | Some(top) ->
            let result = translate_top trans top in
            fprintf chan_out "%s" result
      done
    with
      | Failure(message) ->
        close_out chan_out;
        eprintf "%s" message;
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
