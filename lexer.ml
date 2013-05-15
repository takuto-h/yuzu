
open Printf

type t = {source : Source.t}

let reserved = Hashtbl.create 11
let () = Hashtbl.add reserved "def" Token.Def
let () = Hashtbl.add reserved "var" Token.Var
let () = Hashtbl.add reserved "true" Token.True
let () = Hashtbl.add reserved "false" Token.False
let () = Hashtbl.add reserved "if" Token.If
let () = Hashtbl.add reserved "else" Token.Else

let create src = {source=src}
    
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
    | Some(_) ->
      Some(Token.Int(num), pos)
    | None ->
      Some(Token.Int(num), pos)
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
    | Some(_) ->
      Some(ident_or_reserved(Buffer.contents buf), pos)
    | None ->
      Some(ident_or_reserved(Buffer.contents buf), pos)
  end

let rec lex_special_ident lexer pos buf =
  begin match Source.peek lexer.source with
    | Some('|') -> begin
      Source.junk lexer.source;
      Some(Token.Ident(Buffer.contents buf), pos)
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
    
let rec next lexer =
  begin match Source.peek lexer.source with
    | None ->
      None
    | Some(c) -> begin
      lex_token lexer c
    end
  end

and lex_token lexer c =
  let pos = Source.pos lexer.source in
  Source.junk lexer.source;
  begin match c with
    | '{' | '}' | '(' | ')' | ';' | ':' | '^' | '*' ->
      Some(Token.Just(c), pos)
    | '-' ->
      begin match Source.peek lexer.source with
        | Some('>') -> begin
          Source.junk lexer.source;
          Some(Token.RArrow, pos)
        end
        | Some(_) ->
          Some(Token.Just('-'), pos)
        | None ->
          Some(Token.Just('-'), pos)
      end
    | '=' ->
      begin match Source.peek lexer.source with
        | Some('=') -> begin
          Source.junk lexer.source;
          Some(Token.EQ, pos)
        end
        | Some(_) ->
          Some(Token.Just('='), pos)
        | None ->
          Some(Token.Just('='), pos)
      end
    | '$' ->
      begin match Source.peek lexer.source with
        | Some('|') -> begin
          Source.junk lexer.source;
          lex_special_ident lexer pos (Buffer.create 10)
        end
        | Some(_) ->
          Some(Token.Just('$'), pos)
        | None ->
          Some(Token.Just('$'), pos)
      end
    | _ when is_whitespace c ->
      next lexer
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
           "%s: error: unknown character: %c\n%s"
           (Pos.show pos) c (Pos.show_source pos))
  end
