
open Printf
open Type.Open

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
    | None -> begin
      parser.token <- Token.EOF
    end
    | Some(token, pos) -> begin
      parser.token <- token;
      parser.pos <- pos
    end
  end

let expected parser str_token =
  sprintf
    "%s: error: unexpected %s, expected %s\n%s"
    (Pos.show parser.pos) (Token.show parser.token) str_token (Pos.show_source parser.pos)

let skip parser token =
  if parser.token = token then
    lookahead parser
  else
    ()

let require_semi parser =
  begin match parser.token with
    | Token.Just(';') -> begin
      lookahead parser;
      skip parser Token.Newline
    end
    | Token.Newline -> begin
      lookahead parser;
      skip parser (Token.Just(';'))
    end
    | _ ->
      failwith (expected parser "';'")
  end
      
let rec make_abs pos params expr =
  begin match params with
    | [] ->
      assert false
    | (x::[]) ->
      Expr.at pos (Expr.Abs(x, expr))
    | (x::xs) ->
      Expr.at pos (Expr.Abs(x, make_abs pos xs expr))
  end

let rec make_app pos fun_expr arg_exprs =
  begin match arg_exprs with
    | [] ->
      assert false
    | (x::[]) ->
      Expr.at pos (Expr.App(fun_expr,x))
    | (x::xs) ->
      make_app pos (Expr.at pos (Expr.App(fun_expr,x))) xs
  end

let make_ctor_decl pos ret_type ident arg_types =
  let ctor_type = List.fold_right begin fun acc elem ->
    (elem @-> acc) pos
  end arg_types ret_type in
  (ident, Scheme.mono ctor_type)

let rec parse_ident_list parser lst =
  begin match parser.token with
    | Token.Ident(str) -> begin
      let ident = Ident.intern str in
      lookahead parser;
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
          failwith (expected parser "',' or ')'")
      end
    end
    | _ ->
      failwith (expected parser "identifier")
  end

let parse_params parser =
  if parser.token <> Token.Just('(') then
    failwith (expected parser "'('")
  else begin
    lookahead parser;
    parse_ident_list parser []
  end

let rec parse_expr parser =
  parse_eq_expr parser

and parse_eq_expr parser =
  let lhs = parse_add_expr parser in
  begin match parser.token with
    | Token.EQ ->
      let pos = parser.pos in begin
      lookahead parser;
      let rhs = parse_add_expr parser in
      let op = Expr.at pos (Expr.Var(Ident.intern("=="))) in
      Expr.at pos (Expr.App(Expr.at pos (Expr.App(op,lhs)),rhs))
      end
    | _ ->
      lhs
  end

and parse_add_expr parser =
  let lhs_ref = ref (parse_mul_expr parser) in begin
  while parser.token = Token.Just('+') || parser.token = Token.Just('-') do
    begin match parser.token with
      | Token.Just(c) ->
        let pos = parser.pos in begin
        lookahead parser;
        let rhs = parse_mul_expr parser in
        let op = Expr.at pos (Expr.Var(Ident.intern(sprintf "%c" c))) in
        lhs_ref := Expr.at pos (Expr.App(Expr.at pos (Expr.App(op,!lhs_ref)),rhs))
        end
      | _ ->
        assert false
    end
  done;
  !lhs_ref
  end

and parse_mul_expr parser =
  let lhs_ref = ref (parse_prim_expr parser) in begin
  while parser.token = Token.Just('*') do
    begin match parser.token with
      | Token.Just(c) ->
        let pos = parser.pos in begin
        lookahead parser;
        let rhs = parse_prim_expr parser in
        let op = Expr.at pos (Expr.Var(Ident.intern(sprintf "%c" c))) in
        lhs_ref := Expr.at pos (Expr.App(Expr.at pos (Expr.App(op,!lhs_ref)),rhs))
        end
      | _ ->
        assert false
    end
  done;
  !lhs_ref
  end

and parse_prim_expr parser =
  let expr_ref = ref (parse_atom parser) in begin
  while parser.token = Token.Just('(') do
    let pos = parser.pos in
    lookahead parser;
    let args = parse_expr_list parser [] in
    expr_ref := make_app pos !expr_ref args
  done;
  !expr_ref
  end
  
and parse_atom parser =
  let pos = parser.pos in
  begin match parser.token with
    | Token.Int(n) -> begin
      lookahead parser;
      Expr.at pos (Expr.Con(Literal.Int(n)))
    end
    | Token.True -> begin
      lookahead parser;
      Expr.at pos (Expr.Con(Literal.Bool(true)))
    end
    | Token.False -> begin
      lookahead parser;
      Expr.at pos (Expr.Con(Literal.Bool(false)))
    end
    | Token.Ident(str) -> begin
      lookahead parser;
      Expr.at pos (Expr.Var(Ident.intern str))
    end
    | Token.Just('(') -> begin
      lookahead parser;
      parse_parens parser pos
    end
    | Token.Just('^') -> begin
      lookahead parser;
      let params = parse_params parser in
      let body_expr = parse_block parser in
      make_abs pos params body_expr
    end
    | Token.If -> begin
      lookahead parser;
      parse_if parser pos
    end
    | _ ->
      failwith (expected parser "expression")
  end

and parse_parens parser pos =
  if parser.token = Token.Just(')') then begin
    lookahead parser;
    Expr.at pos (Expr.Con(Literal.Unit))
  end
  else
    let expr = parse_expr parser in
    begin match parser.token with
      | Token.Just(')') -> begin
        lookahead parser;
        expr
      end
      | Token.Just(',') -> begin
        lookahead parser;
        parse_tuple parser pos [expr]
      end
      | _ ->
        failwith (expected parser "',' or ')'")
    end

and parse_expr_list parser lst =
  let expr = parse_expr parser in
  begin match parser.token with
    | Token.Just(')') -> begin
      lookahead parser;
      List.rev (expr::lst)
    end
    | Token.Just(',') -> begin
      lookahead parser;
      parse_expr_list parser (expr::lst)
    end
    | _ ->
      failwith (expected parser "',' or ')'")
  end

and parse_tuple parser pos lst =
  let expr_list = parse_expr_list parser lst in
  Expr.at pos (Expr.Tuple(expr_list))

and parse_indented_block parser = begin
  lookahead parser;
  let body_expr = parse_block_elem parser in
  skip parser (Token.Just(';'));
  if parser.token <> Token.Undent then
    failwith (expected parser "undent")
  else begin
    lookahead parser;
    body_expr
  end
end

and parse_braced_block parser = begin
  lookahead parser;
  let body_expr = parse_block_elem parser in
  skip parser (Token.Just(';'));
  if parser.token <> Token.Just('}') then
    failwith (expected parser "'}'")
  else begin
    lookahead parser;
    body_expr
  end
end

and parse_let_val parser pos =
  begin match parser.token with
    | Token.Ident(str) ->
      let ident = Ident.intern str in begin
        lookahead parser;
        if parser.token <> Token.Just('=') then
          failwith (expected parser "'='")
        else begin
          lookahead parser;
          let val_expr = parse_expr parser in begin
          require_semi parser;
          let body_expr = parse_block_elem parser in
          Expr.at pos (Expr.LetVal(ident, val_expr, body_expr))
          end
        end
      end
    | Token.Just('(') ->
      lookahead parser;
      parse_let_tuple parser pos;
    | _ ->
      failwith (expected parser "identifier")
  end

and parse_let_tuple parser pos =
  let ident_list = parse_ident_list parser [] in
  if parser.token <> Token.Just('=') then
    failwith (expected parser "'='")
  else begin
    lookahead parser;
    let val_expr = parse_expr parser in begin
    require_semi parser;
    let body_expr = parse_block_elem parser in
    Expr.at pos (Expr.LetTuple(ident_list, val_expr, body_expr))
    end
  end

and parse_let_fun parser pos =
  begin match parser.token with
    | Token.Ident(str) ->
      let pos_abs = parser.pos in
      let ident = Ident.intern str in begin
      lookahead parser;
      let params = parse_params parser in
      let val_expr = parse_block parser in
      require_semi parser;
      let body_expr = parse_block_elem parser in
      let fun_expr = make_abs pos_abs params val_expr in
      Expr.at pos (Expr.LetFun(ident, fun_expr, body_expr))
      end
    | _ ->
      failwith (expected parser "identifier")
  end

and parse_block_elem parser =
  begin match parser.token with
    | Token.Var ->
      let pos = parser.pos in begin
      lookahead parser;
      parse_let_val parser pos
      end
    | Token.Def ->
      let pos = parser.pos in begin
      lookahead parser;
      parse_let_fun parser pos
      end
    | _ ->
      parse_expr parser
  end

and parse_block parser =
  begin match parser.token with
    | Token.Just(':') ->
      Lexer.indent parser.lexer;
      parse_indented_block parser
    | Token.Just('{') ->
      parse_braced_block parser
    | _ ->
      failwith (expected parser "':' or '{'")
  end

and parse_if parser pos =
  if parser.token <> Token.Just('(') then
    failwith (expected parser "'('")
  else begin
    lookahead parser;
    let cond_expr = parse_expr parser in
    if parser.token <> Token.Just(')') then
      failwith (expected parser "')'")
    else begin
      lookahead parser;
      let then_expr = parse_block parser in
      (skip parser Token.Newline);
      (if parser.token <> Token.Else then
          failwith (expected parser "'else'")
       else begin
         lookahead parser;
         let else_expr = parse_block parser in
         Expr.at pos (Expr.If(cond_expr,then_expr,else_expr))
       end)
    end
  end

let parse_top_let_val parser =
  begin match parser.token with
    | Token.Ident(str) ->
      let ident = Ident.intern str in begin
        lookahead parser;
        if parser.token <> Token.Just('=') then
          failwith (expected parser "'='")
        else begin
          lookahead parser;
          Top.LetVal(ident, parse_expr parser)
        end
      end
    | _ ->
      failwith (expected parser "identifier")
  end

let parse_top_let_fun parser =
  begin match parser.token with
    | Token.Ident(str) ->
      let pos_abs = parser.pos in
      let ident = Ident.intern str in begin
      lookahead parser;
      let params = parse_params parser in
      let body_expr = parse_block parser in
      Top.LetFun(ident, make_abs pos_abs params body_expr)
      end
    | _ ->
      failwith (expected parser "identifier")
  end

let rec parse_simple_type parser =
  let pos = parser.pos in
  begin match parser.token with
    | Token.Ident(str) -> begin
      lookahead parser;
      Type.Con(pos,Ident.intern(str))
    end
    | Token.Just('(') -> begin
      lookahead parser;
      if parser.token <> Token.Just(')') then
        let t = parse_complex_type parser in
        if parser.token <> Token.Just(')') then
          failwith (expected parser "')'")
        else begin
          lookahead parser;
          t
        end
      else begin
        lookahead parser;
        Type.Con(pos,Ident.intern("()"))
      end
    end
    | _ ->
      failwith (expected parser "type")
  end
  
and parse_complex_type parser =
  let lhs = parse_simple_type parser in
  if parser.token <> Token.RArrow then
    lhs
  else
    let pos = parser.pos in begin
    lookahead parser;
    let rhs = parse_complex_type parser in
    (lhs @-> rhs) pos
    end
  
let rec parse_type_list parser lst =
  let t = parse_complex_type parser in
  begin match parser.token with
    | Token.Just(')') ->
      lookahead parser;
      List.rev (t::lst)
    | Token.Just(',') ->
      lookahead parser;
      parse_type_list parser (t::lst)
    | _ ->
      failwith (expected parser "',' or ')'")
  end
    
let parse_ctor_arg_types parser =
  if parser.token <> Token.Just('(') then
    failwith (expected parser "'('")
  else begin
    lookahead parser;
    parse_type_list parser [];
  end

let parse_ctor_decl parser ret_type =
  let pos = parser.pos in
  if parser.token <> Token.Def then
    failwith (expected parser "'def'")
  else begin
    lookahead parser;
    begin match parser.token with
      | Token.Ident(str) ->
        let ident = Ident.intern str in begin
        lookahead parser;
        let arg_types = parse_ctor_arg_types parser in
        make_ctor_decl pos ret_type ident arg_types
        end
      | _ ->
        failwith (expected parser "identifier")
    end
  end
    
let rec parse_braced_type_def parser ret_type lst =
  lookahead parser;
  if parser.token = Token.Just('}') then begin
    lookahead parser;
    List.rev lst
  end
  else
    let ctor_decl = parse_ctor_decl parser ret_type in
    begin match parser.token with
      | Token.Just('}') -> begin
        lookahead parser;
        List.rev (ctor_decl::lst)
      end
      | Token.Just(';') -> begin
        parse_braced_type_def parser ret_type (ctor_decl::lst)
      end
      | _ ->
        failwith (expected parser "';' or '}'")
    end

let parse_top_type_def parser pos =
  begin match parser.token with
    | Token.Ident(str) ->
      let ret_type = Type.Con(pos, Ident.intern str) in
      lookahead parser;
      begin match parser.token with
        | Token.Just('{') ->
          Top.Type(parse_braced_type_def parser ret_type [])
        | _ ->          
          failwith (expected parser "':' or '{'")
      end
    | _ ->
      failwith (expected parser "identifier")
  end

let parse_top parser =
  begin match parser.token with
    | Token.Var -> begin
      let pos = parser.pos in
      lookahead parser;
      Top.at pos (parse_top_let_val parser)
    end
    | Token.Def -> begin
      let pos = parser.pos in
      lookahead parser;
      Top.at pos (parse_top_let_fun parser)
    end
    | Token.Type -> begin
      let pos = parser.pos in
      lookahead parser;
      Top.at pos (parse_top_type_def parser pos)
    end
    | _ ->
      Top.at parser.pos (Top.Expr(parse_expr parser))
  end
    
let parse_stmt parser =
  let top = parse_top parser in
  begin match parser.token with
    | Token.Just(';') | Token.Newline | Token.EOF ->
      top
    | _ ->
      failwith (expected parser "';'")
  end

let parse parser = begin
  lookahead parser;
  (skip parser Token.Newline);
  (if parser.token = Token.EOF then
      None
   else
      Some(parse_stmt parser))
end

let parse_scheme parser =
  Scheme.mono (parse_complex_type parser)
  
let parse_val_decl parser =
  begin match parser.token with
    | Token.Ident(str) -> begin
      lookahead parser;
      if parser.token <> Token.Just(':') then
        failwith (expected parser "':'")
      else begin
        lookahead parser;
        let scm = parse_scheme parser in
        Decl.Val(Ident.intern(str),scm)
      end
    end
    | _ ->
      failwith (expected parser "identifier")
  end
  
let parse_decl_expr parser =
  begin match parser.token with
    | Token.Def -> begin
      lookahead parser;
      parse_val_decl parser
    end
    | _ ->
      failwith (expected parser "'def'")
  end
  
let parse_decl_stmt parser =
  let decl = parse_decl_expr parser in
  begin match parser.token with
    | Token.Just(';') | Token.Newline | Token.EOF ->
      decl
    | _ ->
      failwith (expected parser "';'")
  end
  
let parse_decl parser = begin
  lookahead parser;
  (skip parser Token.Newline);
  (if parser.token = Token.EOF then
      None
   else
      Some(parse_decl_stmt parser))
end
