
open Printf

type t = {
  lexer : Lexer.t;
  mutable token : Token.t;
  mutable pos : Pos.t;
}

let initial_buffer_size = 16

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

let parse_non_assoc parser get_op parse_lower =
  let lhs = parse_lower parser in
  match get_op parser.token with
    | None ->
      lhs
    | Some(str) ->
      lookahead parser;
      let op = Expr.Var(Ident.intern(str)) in
      let rhs = parse_lower parser in
      Expr.App(Expr.App(op,lhs),rhs)

let rec parse_right_assoc parser get_op parse_lower =
  let lhs = parse_lower parser in
  match get_op parser.token with
    | None ->
      lhs
    | Some(str) -> begin
      lookahead parser;
      let op = Expr.Var(Ident.intern(str)) in
      let rhs = parse_right_assoc parser get_op parse_lower in
      Expr.App(Expr.App(op,lhs),rhs)
    end

let parse_left_assoc parser get_op parse_lower =
  let lhs = parse_lower parser in
  let rec loop lhs =
    match get_op parser.token with
      | None ->
        lhs
      | Some(str) -> begin
        lookahead parser;
        let op = Expr.Var(Ident.intern(str)) in
        let rhs = parse_lower parser in
        loop (Expr.App(Expr.App(op,lhs),rhs))
      end
  in loop lhs

let parse_to_list parser is_terminal parse_elem =
  let rec loop list =
    let elem = parse_elem parser in
    if is_terminal parser.token then begin
      lookahead parser;
      List.rev (elem::list)
    end
    else begin
      lookahead parser;
      loop (elem::list)
    end
  in loop []

let rec parse_expr parser =
  parse_or_expr parser

and parse_or_expr parser =
  let get_op = function
    | Token.OrOp(str) ->
      Some(str)
    | _ ->
      None
  in parse_right_assoc parser get_op parse_and_expr

and parse_and_expr parser =
  let get_op = function
    | Token.AndOp(str) ->
      Some(str)
    | _ ->
      None
  in parse_right_assoc parser get_op parse_cmp_expr

and parse_cmp_expr parser =
  let get_op = function
    | Token.CmpOp(str) ->
      Some(str)
    | _ ->
      None
  in parse_non_assoc parser get_op parse_cons_expr

and parse_cons_expr parser =
  let lhs = parse_add_expr parser in
  match parser.token with
    | Token.ConsOp(str) -> begin
      lookahead parser;
      let op = Expr.Var(Ident.intern(str)) in
      let rhs = parse_cons_expr parser in
      Expr.App(op,Expr.Tuple([lhs;rhs]))
    end
    | _ ->
      lhs

and parse_add_expr parser =
  let get_op = function
    | Token.AddOp(str) ->
      Some(str)
    | _ ->
      None
  in parse_left_assoc parser get_op parse_mul_expr

and parse_mul_expr parser =
  let get_op = function
    | Token.MulOp(str) ->
      Some(str)
    | _ ->
      None
  in parse_left_assoc parser get_op parse_pow_expr

and parse_pow_expr parser =
  let get_op = function
    | Token.PowOp(str) ->
      Some(str)
    | _ ->
      None
  in parse_right_assoc parser get_op parse_unary_expr

and parse_unary_expr parser =
  parse_prim_expr parser

and parse_prim_expr parser =
  let fun_expr = parse_atomic_expr parser in
  let rec loop fun_expr =
    match parser.token with
      | Token.Reserved("(") -> begin
        lookahead parser;
        let arg_exprs = parse_expr_list parser in
        loop (make_app fun_expr arg_exprs)
      end
      | Token.Reserved("^") -> begin
        lookahead parser;
        let arg_expr = parse_abs parser in
        loop (Expr.App(fun_expr, arg_expr))
      end
      | _ ->
        fun_expr
  in loop fun_expr
    
and parse_atomic_expr parser =
  match parser.token with
    | Token.Int(n) -> begin
      lookahead parser;
      Expr.Con(Literal.Int(n))
    end
    | Token.Ident(str) -> begin
      lookahead parser;
      let buf = Buffer.create initial_buffer_size in
      Buffer.add_string buf str;
      parse_var parser buf
    end
    | Token.String(str) -> begin
      lookahead parser;
      Expr.Con(Literal.String(str))
    end
    | Token.Char(str) -> begin
      lookahead parser;
      Expr.Con(Literal.Char(str))
    end
    | Token.Reserved("^") -> begin
      lookahead parser;
      parse_abs parser
    end
    | Token.Reserved("if") -> begin
      lookahead parser;
      parse_if_expr parser
    end
    | Token.Reserved("[") -> begin
      lookahead parser;
      parse_list parser
    end
    | _ ->
      failwith (expected parser "expression")

and parse_var parser buf =
  match parser.token with
    | Token.Reserved(".") -> begin
      Buffer.add_char buf '.';
      lookahead parser;
      begin match parser.token with
        | Token.Ident(str) -> begin
          Buffer.add_string buf str;
          lookahead parser;
          parse_var parser buf
        end
        | _ ->
          failwith (expected parser "identifier")
      end
    end
    | _ ->
      Expr.Var(Ident.intern (Buffer.contents buf))

and parse_abs parser =
  let params = parse_params parser in
  let body_expr = parse_block parser in
  make_abs params body_expr

and parse_params parser =
  if parser.token <> Token.Reserved("(") then
    failwith (expected parser "'('")
  else begin
    lookahead parser;
    parse_ident_list parser
  end

and parse_ident_list parser =
  let is_terminal = function
    | Token.Reserved(")") ->
      true
    | Token.Reserved(",") ->
      false
    | _ -> 
      failwith (expected parser "')' or ','")
  in parse_to_list parser is_terminal parse_ident

and parse_ident parser =
  match parser.token with
    | Token.Ident(str) -> begin
      lookahead parser;
      Ident.intern str
    end
    | _ ->
      failwith (expected parser "identifier")

and parse_block parser =
  match parser.token with
    | Token.Reserved(":") ->
      Lexer.indent parser.lexer;
      parse_indented_block parser
    | Token.Reserved("{") ->
      parse_braced_block parser
    | _ ->
      failwith (expected parser "':' or '{'")

and parse_indented_block parser =
  lookahead parser;
  let expr = parse_block_elem parser in
  skip parser (Token.Reserved(";"));
  if parser.token <> Token.Undent then
    failwith (expected parser "undent")
  else begin
    lookahead parser;
    expr
  end

and parse_braced_block parser =
  lookahead parser;
  let expr = parse_block_elem parser in
  skip parser (Token.Reserved(";"));
  if parser.token <> Token.Reserved("}") then
    failwith (expected parser "'}'")
  else begin
    lookahead parser;
    expr
  end

and parse_block_elem parser =
  parse_expr parser

and parse_if_expr parser =
  if parser.token <> Token.Reserved("(") then
    failwith (expected parser "'('")
  else begin
    lookahead parser;
    let cond_expr = parse_expr parser in
    if parser.token <> Token.Reserved(")") then
      failwith (expected parser "')'")
    else begin
      lookahead parser;
      let then_expr = parse_block parser in
      skip parser Token.Newline;
      if parser.token <> Token.Reserved("else") then
        failwith (expected parser "'else'")
      else begin
        lookahead parser;
        let else_expr = parse_block parser in
        Expr.If(cond_expr,then_expr,else_expr)
      end
    end
  end

and parse_list parser =
  if parser.token <> Token.Reserved("]") then
    failwith (expected parser "']'")
  else begin
    lookahead parser;
    Expr.Var(Ident.intern("[]"))
  end

and parse_expr_list parser =
  let is_terminal = function
    | Token.Reserved(")") ->
      true
    | Token.Reserved(",") ->
      false
    | _ -> 
      failwith (expected parser "')' or ','")
  in parse_to_list parser is_terminal parse_expr
    
let parse_top_let_fun parser =
  let fun_ident = parse_ident parser in
  let params = parse_params parser in
  let body_expr = parse_block parser in
  Top.LetFun(fun_ident, make_abs params body_expr)

let parse_top_let_val parser =
  let ident = parse_ident parser in
  if parser.token <> Token.CmpOp("=") then
    failwith (expected parser "'='")
  else begin
    lookahead parser;
    Top.LetVal(ident, parse_expr parser)
  end

let parse_top parser =
  match parser.token with
    | Token.Reserved("def") -> begin
      lookahead parser;
      parse_top_let_fun parser
    end
    | Token.Reserved("var") -> begin
      lookahead parser;
      parse_top_let_val parser
    end
    | _ ->
      Top.Expr(parse_expr parser)

let parse_stmt parser =
  let expr = parse_top parser in
  match parser.token with
    | Token.EOF | Token.Newline | Token.Reserved(";") ->
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
