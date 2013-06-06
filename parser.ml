
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

let rec parse_top parser =
  match parser.token with
    | Token.Reserved("def") -> begin
      parse_top_let_fun parser
    end
    | Token.Reserved("var") -> begin
      parse_top_let_val parser
    end
    | Token.Reserved("open") -> begin
      parse_top_open parser
    end
    | Token.Reserved("type") -> begin
      parse_top_typedef parser
    end
    | _ ->
      Top.Expr(parse_expr parser)

and parse_top_let_fun parser =
  lookahead parser;
  let fun_name = parse_val_name parser in
  let params = parse_params parser in
  let body_expr = parse_block parser in
  Top.LetFun(fun_name, make_abs params body_expr)

and parse_top_let_val parser =
  lookahead parser;
  let val_pat = parse_pattern parser in
  if parser.token <> Token.CmpOp("=") then
    failwith (expected parser "'='")
  else begin
    lookahead parser;
    Top.LetVal(val_pat, parse_expr parser)
  end

and parse_top_open parser =
  lookahead parser;
  let mod_path = parse_mod_path parser [] in
  Top.Open(mod_path)

and parse_top_typedef parser =
  lookahead parser;
  let typector_name = parse_lowid parser in
  match parser.token with
    | Token.CmpOp("=") -> begin
      lookahead parser;
      let t = parse_type parser in
      Top.Abbrev(typector_name,t)
    end
    | Token.Reserved(":") | Token.Reserved("{") ->
      parse_type_repr parser typector_name
    | _ ->
      failwith (expected parser "'=' or ':' or '{'")

and parse_type_repr parser typector_name =
  match parser.token with
    | Token.Reserved(":") ->
      Lexer.indent parser.lexer;
      lookahead parser;
      begin match parser.token with
        | Token.Reserved("def") ->
          Top.Variant(typector_name, parse_indented_ctor_decls parser [])
        | _ ->
          failwith (expected parser "def")
      end
    | Token.Reserved("{") ->
      lookahead parser;
      begin match parser.token with
        | Token.Reserved("def") ->
          Top.Variant(typector_name, parse_braced_ctor_decls parser [])
        | _ ->
          failwith (expected parser "def")
      end
    | _ ->
      failwith (expected parser "':' or '{'")

and parse_indented_ctor_decls parser ctor_decls =
  if parser.token <> Token.Reserved("def") then
    List.rev ctor_decls
  else
    let ctor_decl = parse_ctor_decl parser in
    match parser.token with
      | Token.Reserved(";") ->
        lookahead parser;
        skip parser Token.Newline;
        parse_indented_ctor_decls parser (ctor_decl::ctor_decls)
      | Token.Newline ->
        lookahead parser;
        parse_indented_ctor_decls parser (ctor_decl::ctor_decls)
      | Token.Undent -> begin
        lookahead parser;
        List.rev (ctor_decl::ctor_decls)
      end
      | _ ->
        failwith (expected parser "';' or newline or undent")

and parse_braced_ctor_decls parser ctor_decls =
  if parser.token <> Token.Reserved("def") then
    List.rev ctor_decls
  else
    let ctor_decl = parse_ctor_decl parser in
    match parser.token with
      | Token.Reserved(";") ->
        lookahead parser;
        parse_braced_ctor_decls parser (ctor_decl::ctor_decls)
      | Token.Reserved("}") -> begin
        lookahead parser;
        List.rev (ctor_decl::ctor_decls)
      end
      | _ ->
        failwith (expected parser "';' or '}'")

and parse_ctor_decl parser =
  lookahead parser;
  let ctor_name = Names.Id(parse_capid parser) in
  if parser.token <> Token.Reserved("(") then
    (ctor_name, None)
  else begin
    lookahead parser;
    let t = parse_type parser in
    if parser.token <> Token.Reserved(")") then
      failwith (expected parser "')'")
    else begin
      lookahead parser;
      (ctor_name, Some(t))
    end
  end

and parse_type parser =
  parse_tuple_type parser

and parse_tuple_type parser =
  let t = parse_atomic_type parser in
  let rec loop ts =
    match parser.token with
      | Token.MulOp("*") ->
        lookahead parser;
        let t = parse_atomic_type parser in
        loop (t::ts)
      | _ ->
        List.rev ts
  in
  match parser.token with
    | Token.MulOp("*") ->
      Type.Tuple(loop [t])
    | _ ->
      t

and parse_atomic_type parser =
  match parser.token with
    | Token.LowId(_) | Token.CapId(_) ->
      let typector = parse_typector parser [] in
      begin match parser.token with
      | Token.Reserved("(") -> begin
        lookahead parser;
        let args = parse_type_args parser in
        Type.App(typector, args)
      end
      | _ ->
        Type.Con(typector)
      end
    | Token.Reserved("(") -> begin
      lookahead parser;
      let t = parse_type parser in
      if parser.token <> Token.Reserved(")") then
        failwith (expected parser "')'")
      else begin
        lookahead parser;
        t
      end
    end
    | _ ->
      failwith (expected parser "type")

and parse_typector parser mod_names =
  match parser.token with
    | Token.LowId(_) ->
      let typector_name = parse_lowid parser in
      (List.rev mod_names, typector_name)
    | Token.CapId(_) ->
      let capid = parse_capid parser in
      if parser.token <> Token.Reserved(".") then
        failwith (expected parser "'.'")
      else begin
        lookahead parser;
        parse_typector parser (capid::mod_names)
      end
    | _ ->
      failwith (expected parser "identifier")

and parse_type_args parser =
  let is_terminal = function
    | Token.Reserved(")") ->
      true
    | Token.Reserved(",") ->
      false
    | _ -> 
      failwith (expected parser "')' or ','")
  in parse_to_list parser is_terminal parse_type

and parse_expr parser =
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
    | Token.LowId(_) | Token.CapId(_) | Token.Reserved("$") ->
      parse_var_or_ctor_app parser []
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

and parse_var_or_ctor_app parser mod_names =
  match parser.token with
    | Token.LowId(_) | Token.Reserved("$") ->
      let val_name = parse_val_name parser in
      Expr.Var(List.rev mod_names, val_name)
    | Token.CapId(_) ->
      let capid = parse_capid parser in
      if parser.token <> Token.Reserved(".") then
        parse_ctor_app parser (Expr.Ctor(List.rev mod_names, Names.Id(capid)))
      else begin
        lookahead parser;
        parse_var_or_ctor_app parser (capid::mod_names)
      end
    | _ ->
      failwith (expected parser "identifier")

and parse_ctor_app parser ctor =
  match parser.token with
    | Token.Reserved("(") -> begin
      lookahead parser;
      let arg_exprs = parse_args parser in
      Expr.App(ctor, Expr.Tuple(arg_exprs))
    end
    | Token.Reserved("^") -> begin
      let arg_expr = parse_abs parser in
      Expr.App(ctor, arg_expr)
    end
    | _ ->
      ctor

and parse_ctor parser mod_names =
  match parser.token with
    | Token.CapId(_) ->
      let capid = parse_capid parser in
      if parser.token <> Token.Reserved(".") then
        (List.rev mod_names, Names.Id(capid))
      else begin
        lookahead parser;
        parse_ctor parser (capid::mod_names)
      end
    | _ ->
      failwith (expected parser "capitalized identifier")

and parse_mod_path parser mod_names =
  let capid = parse_capid parser in
  match parser.token with
    | Token.Reserved(".") -> begin
      lookahead parser;
      parse_mod_path parser (capid::mod_names)
    end
    | _ ->
      List.rev (capid::mod_names)

and parse_abs parser =
  lookahead parser;
  let params = parse_params parser in
  let body_expr = parse_block parser in
  make_abs params body_expr

and parse_params parser =
  if parser.token <> Token.Reserved("(") then
    failwith (expected parser "'('")
  else begin
    lookahead parser;
    if parser.token = Token.Reserved(")") then begin
      lookahead parser;
      [Pattern.Variant(([], Names.Id("()")), [Pattern.Var(Names.Id("_"))])]
    end
    else
      parse_pattern_list parser
  end

and parse_val_name parser =
  match parser.token with
    | Token.LowId(_) ->
      Names.Id(parse_lowid parser)
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

and parse_lowid parser =
  match parser.token with
    | Token.LowId(str) -> begin
      lookahead parser;
      str
    end
    | _ ->
      failwith (expected parser "lowercase identifier")

and parse_capid parser =
  match parser.token with
    | Token.CapId(str) -> begin
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
  match parser.token with
    | Token.Reserved("var") -> begin
      parse_let_val parser
    end
    | Token.Reserved("def") -> begin
      parse_let_fun parser
    end
    | _ ->
      parse_expr parser

and parse_let_val parser =
  lookahead parser;
  let val_pat = parse_pattern parser in
  if parser.token <> Token.CmpOp("=") then
    failwith (expected parser "'='")
  else begin
    lookahead parser;
    let val_expr = parse_expr parser in
    parse_block_sep parser;
    let cont_expr = parse_block_elem parser in
    Expr.LetVal(val_pat,val_expr,cont_expr)
  end

and parse_let_fun parser =
  lookahead parser;
  let fun_name = parse_val_name parser in
  let params = parse_params parser in
  let body_expr = parse_block parser in
  parse_block_sep parser;
  let cont_expr = parse_block_elem parser in
  Expr.LetFun(fun_name, make_abs params body_expr, cont_expr)

and parse_block_sep parser =
  match parser.token with
    | Token.Reserved(";") -> begin
      lookahead parser;
      skip parser Token.Newline
    end
    | Token.Newline -> begin
      lookahead parser
    end
    | _ ->
      failwith (expected parser "';' or newline")

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
    let list = parse_expr_list parser in
    Expr.Tuple(list)

and parse_args parser =
  if parser.token = Token.Reserved(")") then begin
    lookahead parser;
    [Expr.Var([], Names.Id("()"))]
  end
  else
    parse_expr_list parser

and parse_expr_list parser =
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
  parse_or_pattern parser

and parse_or_pattern parser =
  let lhs = parse_cons_pattern parser in
  let rec loop lhs =
    match parser.token with
      | Token.CmpOp("|") -> begin
        lookahead parser;
        let rhs = parse_cons_pattern parser in
        loop (Pattern.Or(lhs, rhs))
      end
      | _ ->
        lhs
  in loop lhs

and parse_cons_pattern parser =
  let lhs = parse_atomic_pattern parser in
  match parser.token with
    | Token.ConsOp("::") -> begin
      lookahead parser;
      let rhs = parse_cons_pattern parser in
      Pattern.Variant(([], Names.Op("::")), [lhs;rhs])
    end
    | _ ->
      lhs

and parse_atomic_pattern parser =
  match parser.token with
    | Token.Int(_) | Token.String(_) | Token.Char(_) ->
      let lit = parse_literal parser in
      Pattern.Con(lit)
    | Token.LowId(_) ->
      let name = parse_val_name parser in
      Pattern.Var(name)
    | Token.CapId(_) -> begin
      parse_variant_pattern parser
    end
    | Token.Reserved("[") -> begin
      parse_list_pattern parser
    end
    | Token.Reserved("(") -> begin
      parse_parens_pattern parser
    end
    | _ ->
      failwith (expected parser "pattern")

and parse_variant_pattern parser =
  let ctor = parse_ctor parser [] in
  if parser.token <> Token.Reserved("(") then
    Pattern.Variant(ctor, [Pattern.Var(Names.Id("_"))])
  else begin
    lookahead parser;
    let pat_list = parse_pattern_list parser in
    Pattern.Variant(ctor, pat_list)
  end

and parse_list_pattern parser =
  lookahead parser;
  if parser.token = Token.Reserved("]") then begin
    lookahead parser;
    Pattern.Variant(([], Names.Id("[]")), [Pattern.Var(Names.Id("_"))])
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
    let list = parse_to_list parser is_terminal parse_pattern in
    List.fold_right begin fun elem acc ->
      Pattern.Variant(([], Names.Op("::")), [elem;acc])
    end list (Pattern.Variant(([], Names.Id("[]")), [Pattern.Var(Names.Id("_"))]))

and parse_parens_pattern parser =
  lookahead parser;
  if parser.token = Token.Reserved(")") then begin
    lookahead parser;
    Pattern.Variant(([], Names.Id("()")), [Pattern.Var(Names.Id("_"))])
  end
  else
    let list = parse_pattern_list parser in
    Pattern.Tuple(list)

and parse_pattern_list parser =
  let is_terminal = function
    | Token.Reserved(")") ->
      true
    | Token.Reserved(",") ->
      false
    | _ -> 
      failwith (expected parser "')' or ','")
  in parse_to_list parser is_terminal parse_pattern

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
