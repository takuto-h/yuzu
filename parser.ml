
open Printf

type t = {
  lexer : Lexer.t;
  mutable token : Token.t;
  mutable pos : Pos.t;
}

let create lexer = {
  lexer = lexer;
  token = Token.EOF;
  pos = Pos.dummy;
}

let expected parser str_token =
  sprintf
    "%s: error: unexpected %s, expected %s\n"
    (Pos.show parser.pos) (Token.show parser.token) str_token
  
let lookahead parser =
  match Lexer.next parser.lexer with
    | (None, pos) ->
      (* printf "%s: EOF\n" (Pos.show pos); *)
      parser.token <- Token.EOF;
      parser.pos <- pos
    | (Some(token), pos) ->
      (* printf "%s: %s\n" (Pos.show pos) (Token.show token); *)
      parser.token <- token;
      parser.pos <- pos

let skip parser token =
  if parser.token = token then
    lookahead parser
  else
    ()

let rec make_abs params body_expr =
  let mk_abs param expr = Expr.Abs(param, expr) in
  List.fold_right mk_abs params body_expr

let rec make_app fun_expr arg_exprs =
  let mk_app e1 e2 = Expr.App(e1, e2) in
  List.fold_left mk_app fun_expr arg_exprs
    
let rec parse_expr parser =
  parse_prim_expr parser

and parse_prim_expr parser =
  let expr_ref = ref (parse_atomic_expr parser) in
  while parser.token = Token.Just('(') do
    lookahead parser;
    let args = parse_expr_list parser [] in
    expr_ref := make_app !expr_ref args
  done;
  !expr_ref

and parse_expr_list parser lst =
  let expr = parse_expr parser in
  match parser.token with
    | Token.Just(')') -> begin
      lookahead parser;
      List.rev (expr::lst)
    end
    | Token.Just(',') -> begin
      lookahead parser;
      parse_expr_list parser (expr::lst)
    end
    | _ ->
      failwith (expected parser "')' or ','")
    
and parse_atomic_expr parser =
  match parser.token with
    | Token.Int(n) -> begin
      lookahead parser;
      Expr.Con(Literal.Int(n))
    end
    | Token.Ident(str) -> begin
      lookahead parser;
      Expr.Var(Ident.intern(str))
    end
    | Token.Just('^') -> begin
      lookahead parser;
      let params = parse_params parser in
      let body_expr = parse_block parser in
      make_abs params body_expr
    end
    | _ ->
      failwith (expected parser "expression")

and parse_params parser =
  if parser.token <> Token.Just('(') then
    failwith (expected parser "'('")
  else begin
    lookahead parser;
    parse_ident_list parser []
  end

and parse_ident_list parser lst =
  match parser.token with
    | Token.Ident(str) ->
      lookahead parser;
      let ident = Ident.intern str in
      begin match parser.token with
        | Token.Just(')') -> begin
          lookahead parser;
          List.rev (ident::lst)
        end
        | Token.Just(',') -> begin
          lookahead parser;
          parse_ident_list parser (ident::lst)
        end
        | _ ->
          failwith (expected parser "')' or ','")
      end
    | _ ->
      failwith (expected parser "identifier")

and parse_block parser =
  match parser.token with
    | Token.Just(':') ->
      Lexer.indent parser.lexer;
      parse_indented_block parser
    | Token.Just('{') ->
      parse_braced_block parser
    | _ ->
      failwith (expected parser "':' or '{'")

and parse_indented_block parser =
  lookahead parser;
  let expr = parse_block_elem parser in
  skip parser (Token.Just(';'));
  if parser.token <> Token.Undent then
    failwith (expected parser "undent")
  else begin
    lookahead parser;
    expr
  end

and parse_braced_block parser =
  lookahead parser;
  let expr = parse_block_elem parser in
  skip parser (Token.Just(';'));
  if parser.token <> Token.Just('}') then
    failwith (expected parser "'}'")
  else begin
    lookahead parser;
    expr
  end

and parse_block_elem parser =
  parse_expr parser

let parse_top_let_fun parser =
  match parser.token with
    | Token.Ident(str) ->
      lookahead parser;
      let fun_ident = Ident.intern str in
      let params = parse_params parser in
      let body_expr = parse_block parser in
      Top.LetFun(fun_ident, make_abs params body_expr)
    | _ ->
      failwith (expected parser "identifier")
  
let parse_top parser =
  match parser.token with
    | Token.Def ->
      lookahead parser;
      parse_top_let_fun parser
    | _ ->
      Top.Expr(parse_expr parser)

let parse_stmt parser =
  let expr = parse_top parser in
  match parser.token with
    | Token.EOF | Token.Newline | Token.Just(';') ->
      expr
    | _ ->
      failwith (expected parser "newline or ';'")
        
let parse parser =
  lookahead parser;
  skip parser Token.Newline;
  match parser.token with
    | Token.EOF ->
      None
    | _ ->
      Some(parse_stmt parser)
