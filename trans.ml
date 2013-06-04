
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
  
let translate_value_name = function
  | Names.Id(str) ->
    str
  | Names.Op(str) ->
    sprintf "( %s )" str

let translate_module_path = function
  | [] ->
    ""
  | name::names ->
    List.fold_left begin fun acc elem ->
      sprintf "%s.%s" acc elem
    end name names

let translate_value_path = function
  | ([], val_name) ->
    translate_value_name val_name
  | (mod_path, val_name) ->
    sprintf "%s.%s" (translate_module_path mod_path) (translate_value_name val_name)

let translate_constr = function
  | ([], cstr_name) ->
    cstr_name
  | (mod_path, cstr_name) ->
    sprintf "%s.%s" (translate_module_path mod_path) cstr_name

let translate_literal = function
  | Literal.Int(n) ->
    sprintf "%d" n
  | Literal.String(str) ->
    sprintf "\"%s\"" str
  | Literal.Char(str) ->
    sprintf "'%s'" str

let rec translate_pattern = function
  | Pattern.Con(lit) ->
    translate_literal lit
  | Pattern.Var(name) ->
    translate_value_name name
  | Pattern.Variant(cstr,pat) ->
    let str_cstr = translate_constr cstr in
    let str_pat = translate_pattern pat in
    sprintf "(%s %s)" str_cstr str_pat

let rec translate_expr trans = function
  | Expr.Con(lit) ->
    translate_literal lit
  | Expr.Var(path) ->
    translate_value_path path
  | Expr.Cstr(cstr) ->
    translate_constr cstr
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
  | Expr.Match(target_expr,cases) ->
    let str_target = translate_expr trans target_expr in
    let trans_case = {trans with indent_level=trans.indent_level+1} in
    let str_cases = List.fold_left begin fun acc elem ->
      sprintf "%s%s" acc (translate_case trans_case elem)
    end "" cases in
    sprintf "begin match %s with%s\n%s" str_target str_cases (indent trans "end")

and translate_case trans (pat,body_expr) =
  let str_pat = sprintf "| %s ->" (translate_pattern pat) in
  let trans_body = {trans with indent_level=trans.indent_level+1} in
  let str_body = translate_expr trans_body body_expr in
  sprintf "\n%s\n%s" (indent trans str_pat) (indent trans_body str_body)
      
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
  | Top.Open(path) ->
    let str_path = translate_module_path path in
    sprintf "open %s\n" str_path

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
