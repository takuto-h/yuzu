
type t = {lexer:Lexer.t; mutable token:Token.t}

let make_parser lexer =
  {lexer=lexer; token=Token.EOF}

let lookahead parser =
  begin match Lexer.next parser.lexer with
    | Some(token) ->
      parser.token <- token
    | None ->
      parser.token <- Token.EOF
  end

let parse_param parser =
  if parser.token <> Token.LParen then
    failwith "expected LParen"
  else begin
    lookahead parser;
    begin match parser.token with
      | Token.Ident(str) ->
        let param_ident = Ident.intern str in begin
          lookahead parser;
          if parser.token <> Token.RParen then
            failwith "expected RParen"
          else begin
            lookahead parser;
            param_ident
          end
        end
      | _ ->
        failwith "expected Ident"
    end
  end

let rec parse_expr parser =
  let expr_ref = ref (parse_atom parser) in begin
  while parser.token = Token.LParen do
    lookahead parser;
    let arg = parse_expr parser in
    if parser.token <> Token.RParen then
      failwith "expected RParen"
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
      let expr = parse_expr parser in
      if parser.token <> Token.RParen then
        failwith "expected RParen"
      else begin
        lookahead parser;
        expr
      end
    end
    | Token.Hat -> begin
      lookahead parser;
      let param_ident = parse_param parser in
      let body_expr = parse_block parser in
      Expr.Abs(param_ident,body_expr)
    end
    | _ ->
      failwith "expected atom"
  end

and parse_block parser =
  if parser.token <> Token.LBrace then
    failwith "expected LBrace"
  else begin
    lookahead parser;
    let body_expr = parse_expr parser in
    if parser.token <> Token.RBrace then
      failwith "expected RBrace"
    else begin
      lookahead parser;
      body_expr
    end
  end

let parse_top_let_val parser =
  begin match parser.token with
    | Token.Ident(str) ->
      let ident = Ident.intern str in begin
        lookahead parser;
        if parser.token <> Token.EQ then
          failwith "expected EQ"
        else begin
          lookahead parser;
          Top.LetVal(ident, parse_expr parser)
        end
      end
    | _ ->
      failwith "expected Ident"
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
      failwith "expected Ident"
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
      failwith "expected Semi"
  end

let parse parser = begin
  lookahead parser;
  if parser.token = Token.EOF then
    raise End_of_file
  else
    parse_stmt parser
end

let of_string str =
  let lexer = Lexer.of_string str in
  make_parser lexer
