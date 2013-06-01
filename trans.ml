
open Printf

type t = {
  basic_offset : int;
  indent_level : int;
}

let create basic_offset = {
  basic_offset = basic_offset;
  indent_level = 0;
}

let ocaml_basic_offset = 2
  
let indent {basic_offset;indent_level} str =
  let offset = basic_offset * indent_level in
  sprintf "%s%s" (String.make offset ' ') str
  
let translate_value_name {ValName.value=str} =
  if Lexer.is_special_ident str then
    sprintf "( %s )" str
  else
    str

let translate_module_path = function
  | {ModPath.value=[]} ->
    ""
  | {ModPath.value=name::names} ->
    List.fold_left begin fun acc elem ->
      sprintf "%s.%s" acc elem
    end name names

let translate_value_path = function
  | ({ModPath.value=[]}, val_name) ->
    translate_value_name val_name
  | (mod_path, val_name) ->
    sprintf "%s.%s" (translate_module_path mod_path) (translate_value_name val_name)

let rec translate_expr trans = function
  | Expr.Con(Literal.Int(n)) ->
    sprintf "%d" n
  | Expr.Con(Literal.String(str)) ->
    sprintf "\"%s\"" str
  | Expr.Con(Literal.Char(str)) ->
    sprintf "'%s'" str
  | Expr.Var(path) ->
    translate_value_path path
  | Expr.Abs(param_name,body_expr) ->
    let str_param = translate_value_name param_name in
    let trans_body = {trans with indent_level=trans.indent_level+1} in
    let str_body = translate_expr trans_body body_expr in
    sprintf
      "begin fun %s ->\n%s\n%s" str_param (indent trans_body str_body) (indent trans "end")
  | Expr.App(fun_expr,arg_expr) ->
    let str_fun = translate_expr trans fun_expr in
    let str_arg = translate_expr trans arg_expr in
    sprintf "(%s %s)" str_fun str_arg
  | Expr.If(cond_expr,then_expr,else_expr) ->
    let str_cond = translate_expr trans cond_expr in
    let trans_then_else = {trans with indent_level=trans.indent_level+1} in
    let str_then = translate_expr trans_then_else then_expr in
    let str_else = translate_expr trans_then_else else_expr in
    sprintf
      "begin if %s then\n%s\n%s\n%s\n%s"
      str_cond
      (indent trans_then_else str_then)
      (indent trans "else")
      (indent trans_then_else str_else)
      (indent trans "end")
  | Expr.Tuple(x::xs) ->
    let str_x = translate_expr trans x in
    let str_x_xs = List.fold_left begin fun acc elem ->
      sprintf "%s, %s" acc (translate_expr trans elem)
    end str_x xs in
    sprintf "(%s)" str_x_xs
  | Expr.Tuple([]) ->
    assert false

let translate_top trans = function
  | Top.Expr(expr) ->
    let str_expr = translate_expr trans expr in
    sprintf "let () = %s\n" str_expr
  | Top.LetFun(name,expr) ->
    let str_name = translate_value_name name in
    let str_expr = translate_expr trans expr in
    sprintf "let rec %s = %s\n" str_name str_expr
  | Top.LetVal(name,expr) ->
    let str_name = translate_value_name name in
    let str_expr = translate_expr trans expr in
    sprintf "let %s = %s\n" str_name str_expr

exception Break
      
let translate_file fname_in fname_out =
  let chan_in = open_in fname_in in
  try
    let chan_out = open_out fname_out in
    begin try
      let src = Source.create fname_in chan_in in
      let lexer = Lexer.create src in
      let parser = Parser.create lexer in
      let trans = create ocaml_basic_offset in
      while true do
        match Parser.parse parser with
          | None ->
            raise Break
          | Some(top) ->
            let result = translate_top trans top in
            fprintf chan_out "%s\n" result;
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

let test () =
  translate_file "test.yz" "test.out"
