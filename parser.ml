
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

let lookahead parser =
  begin match Lexer.next parser.lexer with
    | None ->
      parser.token <- Token.EOF
    | Some(token, pos) -> begin
      parser.token <- token;
      parser.pos <- pos
    end
  end

let expected str_token parser =
  sprintf
    "%s: unexpected %s, expected %s\n%s"
    (Pos.show parser.pos) (Token.show parser.token) str_token (Pos.show_source parser.pos)

let parse_param parser =
  if parser.token <> Token.LParen then
    failwith (expected "'('" parser)
  else begin
    lookahead parser;
    begin match parser.token with
      | Token.Ident(str) ->
        let param_ident = Ident.intern str in begin
          lookahead parser;
          if parser.token <> Token.RParen then
            failwith (expected "')'" parser)
          else begin
            lookahead parser;
            param_ident
          end
        end
      | _ ->
        failwith (expected "identifier" parser)
    end
  end

let rec parse_expr parser =
  let expr_ref = ref (parse_atom parser) in begin
  while parser.token = Token.LParen do
    lookahead parser;
    let arg = parse_expr parser in
    if parser.token <> Token.RParen then
      failwith (expected "')'" parser)
    else begin
      lookahead parser;
      expr_ref := Expr.App(!expr_ref,arg)
    end
  done;
  !expr_ref
  end

and parse_atom parser =
  begin match parser.token with
    | Token.Int(n) -> begin
      lookahead parser;
      Expr.Con(Literal.Int(n))
    end
    | Token.True -> begin
      lookahead parser;
      Expr.Con(Literal.Bool(true))
    end
    | Token.False -> begin
      lookahead parser;
      Expr.Con(Literal.Bool(false))
    end
    | Token.Ident(str) -> begin
      lookahead parser;
      Expr.Var(Ident.intern str)
    end
    | Token.LParen -> begin
      lookahead parser;
      if parser.token = Token.RParen then begin
        lookahead parser;
        Expr.Con(Literal.Unit)
      end
      else begin
        let expr = parse_expr parser in
        if parser.token <> Token.RParen then
          failwith (expected "')'" parser)
        else begin
          lookahead parser;
          expr
        end
      end
    end
    | Token.Hat -> begin
      lookahead parser;
      let param_ident = parse_param parser in
      let body_expr = parse_block parser in
      Expr.Abs(param_ident,body_expr)
    end
    | Token.If -> begin
      lookahead parser;
      parse_if parser
    end
    | _ ->
      failwith (expected "expression" parser)
  end

and parse_block parser =
  if parser.token <> Token.LBrace then
    failwith (expected "'{'" parser)
  else begin
    lookahead parser;
    let body_expr = parse_expr parser in
    if parser.token <> Token.RBrace then
      failwith (expected "'}'" parser)
    else begin
      lookahead parser;
      body_expr
    end
  end

and parse_if parser =
  if parser.token <> Token.LParen then
    failwith (expected "'('" parser)
  else begin
    lookahead parser;
    let cond_expr = parse_expr parser in
    if parser.token <> Token.RParen then
      failwith (expected "')'" parser)
    else begin
      lookahead parser;
      let then_expr = parse_block parser in
      if parser.token <> Token.Else then
        failwith (expected "'else'" parser)
      else begin
        lookahead parser;
        let else_expr = parse_block parser in
        Expr.App(
          Expr.App(Expr.App(Expr.Var(Ident.intern "if"),cond_expr),then_expr),else_expr
        )
      end
    end
  end

let parse_top_let_val parser =
  begin match parser.token with
    | Token.Ident(str) ->
      let ident = Ident.intern str in begin
        lookahead parser;
        if parser.token <> Token.EQ then
          failwith (expected "'='" parser)
        else begin
          lookahead parser;
          Top.LetVal(ident, parse_expr parser)
        end
      end
    | _ ->
      failwith (expected "identifier" parser)
  end

let parse_top_let_fun parser =
  begin match parser.token with
    | Token.Ident(str) ->
      let ident = Ident.intern str in begin
      lookahead parser;
      let param_ident = parse_param parser in
      let body_expr = parse_block parser in
      Top.LetFun(ident, Expr.Abs(param_ident, body_expr))
      end
    | _ ->
      failwith (expected "identifier" parser)
  end

let parse_top parser =
  begin match parser.token with
    | Token.Var -> begin
      lookahead parser;
      parse_top_let_val parser
    end
    | Token.Def -> begin
      lookahead parser;
      parse_top_let_fun parser
    end
    | _ ->
      Top.Expr(parse_expr parser)
  end
    
let parse_stmt parser =
  let top = parse_top parser in
  begin match parser.token with
    | Token.Semi ->
      top
    | Token.EOF ->
      top
    | _ ->
      failwith (expected "';'" parser)
  end

let parse parser = begin
  lookahead parser;
  if parser.token = Token.EOF then
    None
  else
    Some(parse_stmt parser)
end
