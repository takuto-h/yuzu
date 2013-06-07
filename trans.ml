
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
  
let translate_val_name = function
  | Names.Id(str) ->
    str
  | Names.Op(str) ->
    sprintf "( %s )" str

let translate_ctor_name = translate_val_name

let translate_mod_path = function
  | [] ->
    ""
  | name::names ->
    List.fold_left begin fun acc elem ->
      sprintf "%s.%s" acc elem
    end name names

let translate_val_path = function
  | ([], val_name) ->
    translate_val_name val_name
  | (mod_path, val_name) ->
    sprintf "%s.%s" (translate_mod_path mod_path) (translate_val_name val_name)

let translate_ctor = translate_val_path

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
    translate_val_name name
  | Pattern.Variant(ctor,pat::pats) ->
    let str_ctor = translate_ctor ctor in
    let str_pat_list = List.fold_left begin fun acc elem ->
      sprintf "%s, %s" acc (translate_pattern elem)
    end (translate_pattern pat) pats
    in sprintf "(%s(%s))" str_ctor str_pat_list
  | Pattern.Variant(ctor,[]) ->
    assert false
  | Pattern.Tuple(pat::pats) ->
    let str_pat_list = List.fold_left begin fun acc elem ->
      sprintf "%s, %s" acc (translate_pattern elem)
    end (translate_pattern pat) pats
    in sprintf "(%s)" str_pat_list
  | Pattern.Tuple([]) ->
    assert false
  | Pattern.Record(fields) ->
    let str_fields = List.fold_left begin fun acc elem ->
      sprintf "%s%s;" acc (translate_field_def elem)
    end "" fields
    in sprintf "{%s}" str_fields
  | Pattern.Or(lhs,rhs) ->
    let str_lhs = translate_pattern lhs in
    let str_rhs = translate_pattern rhs in
    sprintf "(%s | %s)" str_lhs str_rhs

and translate_field_def = function
  | (path, None) ->
    sprintf "%s" (translate_val_path path)
  | (path, Some(pat)) ->
    sprintf "%s=%s" (translate_val_path path) (translate_pattern pat)

let rec translate_expr trans = function
  | Expr.Con(lit) ->
    translate_literal lit
  | Expr.Var(path) ->
    translate_val_path path
  | Expr.Ctor(ctor) ->
    translate_ctor ctor
  | Expr.Abs(param_pat,body_expr) ->
    let str_param = translate_pattern param_pat in
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
  | Expr.Record(field_defs) ->
    let trans_field_def = {trans with indent_level=trans.indent_level+1} in
    let str_field_defs = List.fold_left begin fun acc elem ->
      let str_field_def = translate_field_def trans_field_def elem in
      sprintf "%s%s;\n" acc (indent trans_field_def str_field_def)
    end "" field_defs in
    sprintf "{\n%s%s" str_field_defs (indent trans "}")
  | Expr.Match(target_expr,cases) ->
    let str_target = translate_expr trans target_expr in
    let trans_case = {trans with indent_level=trans.indent_level+1} in
    let str_cases = List.fold_left begin fun acc elem ->
      sprintf "%s%s" acc (translate_case trans_case elem)
    end "" cases in
    sprintf "begin match %s with%s\n%s" str_target str_cases (indent trans "end")
  | Expr.LetVal(pat,val_expr,cont_expr) ->
    let str_pat = translate_pattern pat in
    let str_val = translate_expr trans val_expr in
    let str_cont = translate_expr trans cont_expr in
    sprintf
      "begin let %s = %s in\n%s\n%s"
      str_pat str_val (indent trans str_cont) (indent trans "end")
  | Expr.LetFun((name,val_expr)::defs, cont_expr) ->
    let str_name = Names.show_val_name name in
    let str_val = translate_expr trans val_expr in
    let str_let_rec = sprintf "let rec %s = %s" str_name str_val in
    let str_let_rec = List.fold_left begin fun acc (name,val_expr) ->
      let str_name = translate_val_name name in
      let str_val = translate_expr trans val_expr in
      sprintf "%s\nand %s = %s" acc str_name str_val
    end str_let_rec defs in
    let str_cont = translate_expr trans cont_expr in
    sprintf "begin %s in\n%s\n%s" str_let_rec (indent trans str_cont) (indent trans "end")
  | Expr.LetFun([], cont_expr) ->
    assert false
  | Expr.Seq(lhs,rhs) ->
    let str_lhs = translate_expr trans lhs in
    let str_rhs = translate_expr trans rhs in
    sprintf "begin\n%s;\n%s\n%s"
      (indent trans str_lhs) (indent trans str_rhs) (indent trans "end")
  | Expr.Field(expr,path) ->
    let str_expr = translate_expr trans expr in
    let str_path = translate_val_path path in
    sprintf "%s.%s" str_expr str_path
  | Expr.Assign(lhs,rhs) ->
    let str_lhs = translate_expr trans lhs in
    let str_rhs = translate_expr trans rhs in
    sprintf "(%s <- %s)" str_lhs str_rhs
  | Expr.Try(expr,cases) ->
    let trans_expr = {trans with indent_level=trans.indent_level+1} in
    let str_expr = translate_expr trans_expr expr in
    let str_cases = List.fold_left begin fun acc elem ->
      sprintf "%s%s" acc (translate_case trans_expr elem)
    end "" cases in
    sprintf "begin try\n%s\n%s\n%s\n%s"
      (indent trans_expr str_expr) (indent trans "with") str_cases (indent trans "end")

and translate_field_def trans (path,expr) =
  let str_path = translate_val_path path in
  let str_expr = translate_expr trans expr in
  sprintf "%s = %s" str_path str_expr

and translate_case trans = function
  | (pat,None,body_expr) ->
    let str_pat = sprintf "| %s ->" (translate_pattern pat) in
    let trans_body = {trans with indent_level=trans.indent_level+1} in
    let str_body = translate_expr trans_body body_expr in
    sprintf "\n%s\n%s" (indent trans str_pat) (indent trans_body str_body)
  | (pat,Some(guard),body_expr) ->
    let str_guard = translate_expr trans guard in
    let str_pat = sprintf "| %s when %s ->" (translate_pattern pat) str_guard in
    let trans_body = {trans with indent_level=trans.indent_level+1} in
    let str_body = translate_expr trans_body body_expr in
    sprintf "\n%s\n%s" (indent trans str_pat) (indent trans_body str_body)
 
let translate_typector = function
  | ([], typector_name) ->
    typector_name
  | (mod_path, typector_name) ->
    sprintf "%s.%s" (translate_mod_path mod_path) typector_name

let rec translate_type = function
  | Type.Con(typector) ->
    translate_typector typector
  | Type.App(typector,t::ts) ->
    let str_typector = translate_typector typector in
    let str_types = List.fold_left begin fun acc elem ->
      sprintf "%s, %s" acc (translate_type elem)
    end (translate_type t) ts
    in sprintf "(%s) %s" str_types str_typector
  | Type.App(typector,[]) ->
    assert false
  | Type.Tuple(t::ts) ->
    let str_types = List.fold_left begin fun acc elem ->
      sprintf "%s * %s" acc (translate_type elem)
    end (translate_type t) ts
    in sprintf "(%s)" str_types
  | Type.Tuple([]) ->
    assert false

let translate_ctor_decl = function
  | (ctor_name,None) ->
    sprintf "| %s\n" (translate_ctor_name ctor_name)
  | (ctor_name,Some(t)) ->
    let str_type = translate_type t in
    sprintf "| %s of %s\n" (translate_ctor_name ctor_name) str_type

let translate_field_decl (is_mutable, field_name, t) =
  if is_mutable then
    sprintf "mutable %s : %s;\n" (translate_val_name field_name) (translate_type t)
  else
    sprintf "%s : %s;\n" (translate_val_name field_name) (translate_type t)

let translate_top trans = function
  | Top.Expr(expr) ->
    let str_expr = translate_expr trans expr in
    sprintf "let () = %s\n" str_expr
  | Top.LetVal(pat,expr) ->
    let str_pat = translate_pattern pat in
    let str_expr = translate_expr trans expr in
    sprintf "let %s = %s\n" str_pat str_expr
  | Top.LetFun((name,expr)::defs) ->
    let str_name = translate_val_name name in
    let str_expr = translate_expr trans expr in
    let str_let_rec = sprintf "let rec %s = %s\n" str_name str_expr in
    List.fold_left begin fun acc (name,expr) ->
      let str_name = translate_val_name name in
      let str_expr = translate_expr trans expr in
      sprintf "%s\nand %s = %s\n" acc str_name str_expr
    end str_let_rec defs
  | Top.LetFun([]) ->
    assert false
  | Top.Open(path) ->
    let str_path = translate_mod_path path in
    sprintf "open %s\n" str_path
  | Top.Abbrev(name,t) ->
    let str_type = translate_type t in
    sprintf "type %s = %s\n" name str_type
  | Top.Variant(name,ctor_decls) ->
    let trans_ctor_decl = {trans with indent_level=trans.indent_level+1} in
    let str_ctor_decls = List.fold_left begin fun acc elem ->
      sprintf "%s%s" acc (indent trans_ctor_decl (translate_ctor_decl elem))
    end "" ctor_decls in
    sprintf "type %s =\n%s" name str_ctor_decls
  | Top.Record(name,field_decls) ->
    let trans_field_decl = {trans with indent_level=trans.indent_level+1} in
    let str_field_decls = List.fold_left begin fun acc elem ->
      sprintf "%s%s" acc (indent trans_field_decl (translate_field_decl elem))
    end "" field_decls in
    sprintf "type %s = {\n%s%s\n" name str_field_decls (indent trans "}")

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
      done;
      true
    with
      | Break ->
        close_out chan_out;
        raise Break
      | Failure(message) as exn ->
        close_out chan_out;
        eprintf "%s" message;
        flush stderr;
        raise exn
      | exn ->
        close_out_noerr chan_out;
        raise exn
    end
  with
    | Break ->
      close_in chan_in;
      true
    | Failure(_) ->
      close_in chan_in;
      false
    | exn ->
      close_in_noerr chan_in;
      raise exn

let test () =
  translate_file "test.yz" "test.out"
