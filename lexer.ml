
open Printf

type t = {
  source : Source.t;
  parens : char Stack.t;
  offside_lines : int Stack.t;
  mutable is_bol : bool; (* beginning of line *)
  mutable is_bob : bool; (* beginning of indented block *)
  mutable prev_pos : Pos.t;
}

let reserved = Hashtbl.create 11
let () = Hashtbl.add reserved "def" Token.Def
let () = Hashtbl.add reserved "var" Token.Var
let () = Hashtbl.add reserved "true" Token.True
let () = Hashtbl.add reserved "false" Token.False
let () = Hashtbl.add reserved "if" Token.If
let () = Hashtbl.add reserved "else" Token.Else

let create src =
  let lexer = {
    source = src;
    parens = Stack.create ();
    offside_lines = Stack.create ();
    is_bol = false;
    is_bob = false;
    prev_pos = Pos.dummy;
  }
  in begin
    Stack.push 0 lexer.offside_lines;
    lexer
  end

let indent lexer =
  lexer.is_bob <- true

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

let is_special_ident str =
  if String.length str = 0 then
    true
  else if not (is_ident_start (String.get str 0)) then
    true
  else
    let rec loop i =
      if i = String.length str then
        false
      else if is_ident_part (String.get str i) then
        loop (i + 1)
      else
        true
    in loop 1

let rec lex_int lexer pos num =
  begin match Source.peek lexer.source with
    | Some(c) when is_digit c -> begin
      Source.junk lexer.source;
      lex_int lexer pos (num * 10 + int_of_digit c)
    end
    | Some(_) | None ->
      (Token.Int(num), pos)
  end

let ident_or_reserved str = begin
  try
    Hashtbl.find reserved str
  with
    | Not_found ->
      Token.Ident(str)
end
    
let rec lex_ident lexer pos buf =
  begin match Source.peek lexer.source with
    | Some(c) when is_ident_part c -> begin
      Buffer.add_char buf c;
      Source.junk lexer.source;
      lex_ident lexer pos buf
    end
    | Some(_) | None ->
      (ident_or_reserved(Buffer.contents buf), pos)
  end

let rec lex_special_ident lexer pos buf =
  begin match Source.peek lexer.source with
    | Some('|') -> begin
      Source.junk lexer.source;
      (Token.Ident(Buffer.contents buf), pos)
    end
    | Some(c) -> begin
      Source.junk lexer.source;
      Buffer.add_char buf c;
      lex_special_ident lexer pos buf
    end
    | None ->
      let pos_eof = Source.pos lexer.source in
      failwith
        (sprintf
           "%s: error: EOF inside an identifier\n%s"
           (Pos.show pos_eof) (Pos.show_source pos_eof))
  end

let lex_close_paren lexer pos open_paren close_paren =
  if Stack.is_empty lexer.parens || Stack.top lexer.parens <> open_paren then
    failwith
      (sprintf
         "%s: error: unmatched parentheses: '%c'\n%s"
         (Pos.show pos) close_paren (Pos.show_source pos))
  else begin
    ignore (Stack.pop lexer.parens);
    (Token.Just(close_paren), pos)
  end

let lex_visible_token lexer c =
  let pos = Source.pos lexer.source in
  Source.junk lexer.source;
  begin match c with
    | ';' | ':' | '^' | '*' | ',' ->
      (Token.Just(c), pos)
    | '{' | '(' -> begin
      Stack.push c lexer.parens;
      (Token.Just(c), pos)
    end
    | '}' ->
      lex_close_paren lexer pos '{' c
    | ')' ->
      lex_close_paren lexer pos '(' c
    | '-' ->
      begin match Source.peek lexer.source with
        | Some('>') -> begin
          Source.junk lexer.source;
          (Token.RArrow, pos)
        end
        | Some(_) | None ->
          (Token.Just('-'), pos)
      end
    | '=' ->
      begin match Source.peek lexer.source with
        | Some('=') -> begin
          Source.junk lexer.source;
          (Token.EQ, pos)
        end
        | Some(_) | None ->
          (Token.Just('='), pos)
      end
    | '$' ->
      begin match Source.peek lexer.source with
        | Some('|') -> begin
          Source.junk lexer.source;
          lex_special_ident lexer pos (Buffer.create 10)
        end
        | Some(_) | None ->
          (Token.Just('$'), pos)
      end
    | _ when is_digit c ->
      lex_int lexer pos (int_of_digit c)
    | _ when is_ident_start c ->
      let buf = Buffer.create 10 in begin
      Buffer.add_char buf c;
      lex_ident lexer pos buf
      end
    | _ ->
      failwith
        (sprintf
           "%s: error: unknown character: '%c'\n%s"
           (Pos.show pos) c (Pos.show_source pos))
  end

let lex_token lexer c =
  let pos = Source.pos lexer.source in
  let offset = pos.Pos.cnum - pos.Pos.bol in
  if lexer.is_bob then begin
    lexer.is_bob <- false;
    lexer.is_bol <- false;
    Stack.push offset lexer.offside_lines;
    lex_visible_token lexer c
  end
  else if lexer.is_bol then
    let offside_line = Stack.top lexer.offside_lines in
    if offset < offside_line then begin
      ignore (Stack.pop lexer.offside_lines);
      (Token.Undent, pos)
    end
    else if offset = offside_line then begin
      lexer.is_bol <- false;
      (Token.Newline, pos)
    end
    else begin
      lexer.is_bol <- false;
      lex_visible_token lexer c
    end
  else
    lex_visible_token lexer c
    
let rec next lexer =
  begin match Source.peek lexer.source with
    | None when Stack.length lexer.offside_lines = 1 ->
      None
    | None ->
      ignore (Stack.pop lexer.offside_lines);
      Some(Token.Undent, lexer.prev_pos)
    | Some('\n') ->
      lexer.is_bol <- true;
      Source.junk lexer.source;
      next lexer
    | Some(c) when is_whitespace c ->
      Source.junk lexer.source;
      next lexer
    | Some(c) ->
      let (token, pos) = lex_token lexer c in
      lexer.prev_pos <- pos;
      Some(token, pos)
  end
