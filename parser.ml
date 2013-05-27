
open Printf

type t = {
  lexer : Lexer.t;
  mutable token : Token.t;
}

let create lexer = {
  lexer = lexer;
  token = Token.EOF;
}

let expected parser str_token =
  let pos = Lexer.pos parser.lexer in
  sprintf
    "%s: error: unexpected %s, expected %s\n"
    (Pos.show pos) (Token.show parser.token) str_token
  
let lookahead parser =
  match Lexer.next parser.lexer with
    | None ->
      parser.token <- Token.EOF
    | Some(token) ->
      parser.token <- token

let parse_atomic_expr parser =
  match parser.token with
    | Token.Int(n) -> begin
      lookahead parser;
      Expr.Con(Literal.Int(n))
    end
    | _ ->
      failwith (expected parser "expression")

let parse_expr parser =
  parse_atomic_expr parser

let parse_top parser =
  Top.Expr(parse_expr parser)

let parse_stmt parser =
  let expr = parse_top parser in
  match parser.token with
    | Token.EOF
    | Token.Just(';') ->
      expr
    | _ ->
      failwith (expected parser "';'")
        
let parse parser =
  lookahead parser;
  match parser.token with
    | Token.EOF ->
      None
    | _ ->
      Some(parse_stmt parser)
