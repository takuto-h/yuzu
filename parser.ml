
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

let parse_non_assoc parser get_op parse_lower =
  let lhs = parse_lower parser in
  match get_op parser.token with
    | None ->
      lhs
    | Some(str) ->
      lookahead parser;
      let op = Expr.Var([], (Names.Op(str))) in
      let rhs = parse_lower parser in
      Expr.App(Expr.App(op,lhs),rhs)

let rec parse_right_assoc parser get_op parse_lower =
  let lhs = parse_lower parser in
  match get_op parser.token with
    | None ->
      lhs
    | Some(str) -> begin
      lookahead parser;
      let op = Expr.Var([], (Names.Op(str))) in
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
        let op = Expr.Var([], (Names.Op(str))) in
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
      let op = Expr.Var([], (Names.Op(str))) in
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
  match parser.token with
    | Token.AddOp("-") -> begin
      lookahead parser;
      let expr = parse_unary_expr parser in
      Expr.App(Expr.Var([],(Names.Op("~-"))),expr)
    end
    | Token.AddOp("+") -> begin
      lookahead parser;
      let expr = parse_unary_expr parser in
      Expr.App(Expr.Var([],(Names.Op("~+"))),expr)
    end
    | _ ->
      parse_prim_expr parser

and parse_prim_expr parser =
  let fun_expr = parse_atomic_expr parser in
  let rec loop fun_expr =
    match parser.token with
      | Token.Reserved("(") -> begin
        lookahead parser;
        let arg_exprs = parse_args parser in
        loop (make_app fun_expr arg_exprs)
      end
      | Token.Reserved("^") -> begin
        let arg_expr = parse_abs parser in
        loop (Expr.App(fun_expr, arg_expr))
      end
      | _ ->
        fun_expr
  in loop fun_expr
    
and parse_atomic_expr parser =
  match parser.token with
    | Token.Int(_) | Token.String(_) | Token.Char(_) ->
      let lit = parse_literal parser in
      Expr.Con(lit)
    | Token.VarId(_) | Token.ConId(_) | Token.Reserved("$") ->
      parse_var_or_constr parser []
    | Token.Reserved("^") ->
      parse_abs parser
    | Token.Reserved("if") ->
      parse_if_expr parser
    | Token.Reserved("[") ->
      parse_list parser
    | Token.Reserved("(") ->
      parse_parens parser
    | Token.Reserved("match") ->
      parse_match_expr parser
    | _ ->
      failwith (expected parser "expression")

and parse_var_or_constr parser mod_names =
  match parser.token with
    | Token.VarId(_) | Token.Reserved("$") ->
      let val_name = parse_value_name parser in
      Expr.Var(mod_names, val_name)
    | Token.ConId(_) ->
      let conid = parse_conid parser in
      if parser.token <> Token.Reserved(".") then
        Expr.Cstr(mod_names, conid)
      else begin
        lookahead parser;
        parse_var_or_constr parser (conid::mod_names)
      end
    | _ ->
      failwith (expected parser "identifier")

and parse_constr parser mod_names =
  match parser.token with
    | Token.ConId(_) ->
      let conid = parse_conid parser in
      if parser.token <> Token.Reserved(".") then
        (mod_names, conid)
      else begin
        lookahead parser;
        parse_constr parser (conid::mod_names)
      end
    | _ ->
      failwith (expected parser "capitalized identifier")

and parse_module_path parser mod_names =
  let conid = parse_conid parser in
  match parser.token with
    | Token.Reserved(".") -> begin
      lookahead parser;
      parse_module_path parser (conid::mod_names)
    end
    | _ ->
      List.rev (conid::mod_names)

and parse_abs parser =
  lookahead parser;
  let params = parse_params parser in
  let body_expr = parse_block parser in
  make_abs params body_expr

and parse_params parser =
  if parser.token <> Token.Reserved("(") then
    failwith (expected parser "'('")
  else
    parse_value_name_list parser

and parse_value_name_list parser =
  lookahead parser;
  let is_terminal = function
    | Token.Reserved(")") ->
      true
    | Token.Reserved(",") ->
      false
    | _ -> 
      failwith (expected parser "')' or ','")
  in parse_to_list parser is_terminal parse_value_name

and parse_value_name parser =
  match parser.token with
    | Token.VarId(_) ->
      Names.Id(parse_varid parser)
    | Token.Reserved("$") ->
      lookahead parser;
      if parser.token <> Token.Reserved("(") then
        failwith (expected parser "'('")
      else begin
        lookahead parser;
        Names.Op(parse_op parser)
      end
    | _ ->
      failwith (expected parser "identifier")

and parse_varid parser =
  match parser.token with
    | Token.VarId(str) -> begin
      lookahead parser;
      str
    end
    | _ ->
      failwith (expected parser "lowercase identifier")

and parse_conid parser =
  match parser.token with
    | Token.ConId(str) -> begin
      lookahead parser;
      str
    end
    | _ ->
      failwith (expected parser "capitalized identifier")

and parse_op parser =
  match Token.get_op parser.token with
    | Some(str) -> begin
      lookahead parser;
      if parser.token <> Token.Reserved(")") then
        failwith (expected parser "')'")
      else begin
        lookahead parser;
        str
      end
    end
    | None ->
      failwith (expected parser "operator")

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
  lookahead parser;
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
  lookahead parser;
  if parser.token = Token.Reserved("]") then begin
    lookahead parser;
    Expr.Var([], (Names.Id("[]")))
  end
  else
    let is_terminal = function
      | Token.Reserved("]") ->
        true
      | Token.Reserved(";") ->
        false
      | _ ->
        failwith (expected parser "']' or ';'")
    in
    let list = parse_to_list parser is_terminal parse_expr in
    List.fold_right begin fun elem acc ->
      let ctor = Expr.Var([], (Names.Op("::"))) in
      Expr.App(ctor, Expr.Tuple([elem;acc]))
    end list (Expr.Var([], (Names.Id("[]"))))

and parse_parens parser =
  lookahead parser;
  if parser.token = Token.Reserved(")") then begin
    lookahead parser;
    Expr.Var([], (Names.Id("()")))
  end
  else
    let list = parse_args parser in
    Expr.Tuple(list)

and parse_args parser =
  let is_terminal = function
    | Token.Reserved(")") ->
      true
    | Token.Reserved(",") ->
      false
    | _ -> 
      failwith (expected parser "')' or ','")
  in parse_to_list parser is_terminal parse_expr

and parse_match_expr parser =
  lookahead parser;
  if parser.token <> Token.Reserved("(") then
    failwith (expected parser "'('")
  else begin
    lookahead parser;
    let target_expr = parse_expr parser in
    if parser.token <> Token.Reserved(")") then
      failwith (expected parser "')'")
    else begin
      lookahead parser;
      let cases = parse_cases parser [] in
      Expr.Match(target_expr, cases)
    end
  end

and parse_cases parser cases =
  skip parser Token.Newline;
  match parser.token with
    | Token.Reserved("case") -> begin
      lookahead parser;
      let pat = parse_pattern parser in
      let expr = parse_block parser in
      parse_cases parser ((pat,expr)::cases)
    end
    | _ ->
      List.rev cases

and parse_pattern parser =
  match parser.token with
    | Token.Int(_) | Token.String(_) | Token.Char(_) ->
      let lit = parse_literal parser in
      Pattern.Con(lit)
    | Token.VarId(_) ->
      let name = parse_value_name parser in
      Pattern.Var(name)
    | Token.ConId(_) ->
      parse_variant_pattern parser
    | _ ->
      failwith (expected parser "pattern")

and parse_variant_pattern parser =
  let cstr = parse_constr parser [] in
  if parser.token <> Token.Reserved("(") then
    Pattern.Variant(cstr, Pattern.Var(Names.Id("_")))
  else begin
    lookahead parser;
    let pat = parse_pattern parser in
    if parser.token <> Token.Reserved(")") then
      failwith (expected parser "')'")
    else begin
      lookahead parser;
      Pattern.Variant(cstr, pat)
    end
  end

and parse_literal parser =
  match parser.token with
    | Token.Int(n) -> begin
      lookahead parser;
      Literal.Int(n)
    end
    | Token.String(str) -> begin
      lookahead parser;
      Literal.String(str)
    end
    | Token.Char(str) -> begin
      lookahead parser;
      Literal.Char(str)
    end
    | _ ->
      failwith (expected parser "literal")

let parse_top_open parser =
  lookahead parser;
  let mod_path = parse_module_path parser [] in
  Top.Open(mod_path)

let parse_top_let_fun parser =
  lookahead parser;
  let fun_name = parse_value_name parser in
  let params = parse_params parser in
  let body_expr = parse_block parser in
  Top.LetFun(fun_name, make_abs params body_expr)

let parse_top_let_val parser =
  lookahead parser;
  let val_name = parse_value_name parser in
  if parser.token <> Token.CmpOp("=") then
    failwith (expected parser "'='")
  else begin
    lookahead parser;
    Top.LetVal(val_name, parse_expr parser)
  end
    
let parse_top parser =
  match parser.token with
    | Token.Reserved("open") ->
      parse_top_open parser
    | Token.Reserved("def") ->
      parse_top_let_fun parser
    | Token.Reserved("var") ->
      parse_top_let_val parser
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
