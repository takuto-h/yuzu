
open Printf

type t = {
  source : Source.t;
  parens : char Stack.t;
  offside_lines : int Stack.t;
  mutable is_bol : bool; (* beginning of line *)
  mutable is_bob : bool; (* beginning of indented block *)
}

let reserved = Hashtbl.create 11
let () = Hashtbl.add reserved "def" Token.Def

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

let int_of_digit c =
  Char.code c - Char.code '0'

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

let lex_close_paren lexer pos open_paren close_paren =
  if Stack.is_empty lexer.parens || Stack.top lexer.parens <> open_paren then
    failwith (sprintf "%s: error: unmatched parentheses: '%c'\n" (Pos.show pos) close_paren)
  else begin
    ignore (Stack.pop lexer.parens);
    Token.Just(close_paren)
  end

let lex_visible_token lexer pos c =
  Source.junk lexer.source;
  match c with
    | ':' | ';' | ',' | '^' ->
      Token.Just(c)
    | '(' | '{' ->
      Stack.push c lexer.parens;
      Token.Just(c)
    | ')' ->
      lex_close_paren lexer pos '(' ')'
    | '}' ->
      lex_close_paren lexer pos '{' '}'
    | _ when is_digit c ->
      lex_int lexer (int_of_digit c)
    | _ when is_ident_start c ->
      let buf = Buffer.create 10 in
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
