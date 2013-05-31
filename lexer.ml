
open Printf

type t = {
  source : Source.t;
  parens : char Stack.t;
  offside_lines : int Stack.t;
  mutable is_bol : bool; (* beginning of line *)
  mutable is_bob : bool; (* beginning of indented block *)
}

let initial_table_size = 16
let initial_buffer_size = 16

let reserved = Hashtbl.create initial_table_size
let () = Hashtbl.add reserved "def" Token.Def
let () = Hashtbl.add reserved "var" Token.Var
let () = Hashtbl.add reserved "if" Token.If
let () = Hashtbl.add reserved "else" Token.Else
  
let create source =
  let lexer = {
    source = source;
    parens = Stack.create ();
    offside_lines = Stack.create ();
    is_bol = false;
    is_bob = false;
  } in
  Stack.push 0 lexer.offside_lines;
  lexer

let indent lexer =
  if Stack.is_empty lexer.parens then
    lexer.is_bob <- true
  else
    let pos = Source.pos lexer.source in
    failwith (sprintf "%s: error: layout inside parentheses\n" (Pos.show pos))

let is_digit c =
  String.contains "0123456789" c

let is_ident_start c =
  String.contains "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_" c

let is_ident_part c =
  is_ident_start c || is_digit c

let is_whitespace c =
  String.contains " \t\r\n" c

let is_op_part c =
  String.contains "=<>|&+-*/%" c

let int_of_digit c =
  Char.code c - Char.code '0'

let is_special_ident str =
  if String.length str = 0 then
    true
  else if not (is_ident_start (String.get str 0)) then
    true
  else
    let rec loop i =
      if i = String.length str then
        false
      else
        let c = String.get str i in
        if is_ident_part c || String.contains "." c then
          loop (i + 1)
        else
          true
    in loop 1

let ident_or_reserved str =
  try
    Hashtbl.find reserved str
  with
    | Not_found ->
      Token.Ident(str)

let rec lex_int lexer n =
  match Source.peek lexer.source with
    | Some(c) when is_digit c ->
      Source.junk lexer.source;
      lex_int lexer (n * 10 + int_of_digit c)
    | Some(_) | None ->
      Token.Int(n)

let rec lex_ident lexer buf =
  match Source.peek lexer.source with
    | Some(c) when is_ident_part c ->
      Buffer.add_char buf c;
      Source.junk lexer.source;
      lex_ident lexer buf
    | Some(_) | None ->
      ident_or_reserved (Buffer.contents buf)

let rec lex_special_ident lexer buf =
  match Source.peek lexer.source with
    | Some(')') -> begin
      Source.junk lexer.source;
      Token.Ident(Buffer.contents buf)
    end
    | Some(c) -> begin
      Source.junk lexer.source;
      Buffer.add_char buf c;
      lex_special_ident lexer buf
    end
    | None ->
      let pos_eof = Source.pos lexer.source in
      failwith (sprintf "%s: error: EOF inside a special identifier\n" (Pos.show pos_eof))
        
let lex_close_paren lexer pos open_paren close_paren =
  if Stack.is_empty lexer.parens || Stack.top lexer.parens <> open_paren then
    failwith (sprintf "%s: error: unmatched parentheses: '%c'\n" (Pos.show pos) close_paren)
  else begin
    ignore (Stack.pop lexer.parens);
    Token.Just(close_paren)
  end

let rec lex_op lexer buf =
  match Source.peek lexer.source with
    | Some(c) when is_op_part c ->
      Buffer.add_char buf c;
      Source.junk lexer.source;
      lex_op lexer buf
    | Some(_) | None ->
      Buffer.contents buf

let rec lex_string lexer buf =
  match Source.peek lexer.source with
    | Some('"') -> begin
      Source.junk lexer.source;
      Token.String(Buffer.contents buf)
    end
    | Some('\\') -> begin
      Source.junk lexer.source;
      begin match Source.peek lexer.source with
        | Some('"') ->
          Source.junk lexer.source;
          Buffer.add_string buf "\\\"";
          lex_string lexer buf
        | Some(_) | None ->
          Buffer.add_char buf '\\';
          lex_string lexer buf
      end
    end
    | Some(c) -> begin
      Source.junk lexer.source;
      Buffer.add_char buf c;
      lex_string lexer buf
    end
    | None ->
      let pos_eof = Source.pos lexer.source in
      failwith (sprintf "%s: error: EOF inside a string literal\n" (Pos.show pos_eof))

let rec lex_char lexer buf =
  match Source.peek lexer.source with
    | Some('\'') -> begin
      Source.junk lexer.source;
      Token.Char(Buffer.contents buf)
    end
    | Some('\\') -> begin
      Source.junk lexer.source;
      begin match Source.peek lexer.source with
        | Some('\'') ->
          Source.junk lexer.source;
          Buffer.add_string buf "\\'";
          lex_char lexer buf
        | Some(_) | None ->
          Buffer.add_char buf '\\';
          lex_char lexer buf
      end
    end
    | Some(c) -> begin
      Source.junk lexer.source;
      Buffer.add_char buf c;
      lex_char lexer buf
    end
    | None ->
      let pos_eof = Source.pos lexer.source in
      failwith (sprintf "%s: error: EOF inside a character literal\n" (Pos.show pos_eof))

let lex_visible_token lexer pos c =
  Source.junk lexer.source;
  match c with
    | ';' | ',' | '^' | '.' ->
      Token.Just(c)
    | '(' | '{' | '[' ->
      Stack.push c lexer.parens;
      Token.Just(c)
    | ')' ->
      lex_close_paren lexer pos '(' ')'
    | '}' ->
      lex_close_paren lexer pos '{' '}'
    | ']' ->
      lex_close_paren lexer pos '[' ']'
    | '=' | '<' | '>' | '|' -> begin
      let buf = Buffer.create initial_buffer_size in
      Buffer.add_char buf c;
      Token.CmpOp(lex_op lexer buf)
    end
    | '+' | '-' -> begin
      let buf = Buffer.create initial_buffer_size in
      Buffer.add_char buf c;
      Token.AddOp(lex_op lexer buf)
    end
    | '*' -> begin
      let buf = Buffer.create initial_buffer_size in
      Buffer.add_char buf c;
      Token.MulOp(lex_op lexer buf)
    end
    | ':' ->
      begin match Source.peek lexer.source with
        | Some(':') ->
          Source.junk lexer.source;
          Token.ConsOp("::")
        | Some(_) | None ->
          Token.Just(':')
      end
    | '$' ->
      begin match Source.peek lexer.source with
        | Some('(') ->
          let buf = Buffer.create initial_buffer_size in
          Source.junk lexer.source;
          lex_special_ident lexer buf
        | Some(_) | None ->
          Token.Just('$')
      end
    | '"' -> begin
      let buf = Buffer.create initial_buffer_size in
      lex_string lexer buf
    end
    | '\'' -> begin
      let buf = Buffer.create initial_buffer_size in
      lex_char lexer buf
    end
    | _ when is_digit c ->
      lex_int lexer (int_of_digit c)
    | _ when is_ident_start c ->
      let buf = Buffer.create initial_buffer_size in
      Buffer.add_char buf c;
      lex_ident lexer buf
    | _ ->
      failwith (sprintf "%s: error: unknown character: '%c'\n" (Pos.show pos) c)

let lex_token lexer pos c =
  let offset = pos.Pos.cnum - pos.Pos.bol in
  if lexer.is_bob then begin
    lexer.is_bob <- false;
    lexer.is_bol <- false;
    Stack.push offset lexer.offside_lines;
    lex_visible_token lexer pos c
  end
  else if lexer.is_bol then
    let offside_line = Stack.top lexer.offside_lines in
    if offset < offside_line then begin
      ignore (Stack.pop lexer.offside_lines);
      Token.Undent
    end
    else if offset = offside_line then begin
      lexer.is_bol <- false;
      Token.Newline
    end
    else begin
      lexer.is_bol <- false;
      lex_visible_token lexer pos c
    end
  else
    lex_visible_token lexer pos c

let rec next lexer =
  let pos = Source.pos lexer.source in
  match Source.peek lexer.source with
    | None when Stack.length lexer.offside_lines = 1 ->
      (None, pos)
    | None ->
      ignore (Stack.pop lexer.offside_lines);
      (Some(Token.Undent), pos)
    | Some('\n') when Stack.is_empty lexer.parens ->
      lexer.is_bol <- true;
      Source.junk lexer.source;
      next lexer
    | Some(c) when is_whitespace c ->
      Source.junk lexer.source;
      next lexer
    | Some(c) ->
      (Some(lex_token lexer pos c), pos)
