
type t = {stream : char Stream.t}

let reserved = Hashtbl.create 11
let () = Hashtbl.add reserved "def" Token.Def
let () = Hashtbl.add reserved "var" Token.Var
let () = Hashtbl.add reserved "true" Token.True
let () = Hashtbl.add reserved "false" Token.False
let () = Hashtbl.add reserved "if" Token.If
let () = Hashtbl.add reserved "else" Token.Else

let make_lexer strm = {stream=strm}
    
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
    
let rec lex_int lexer num =
  begin match Stream.peek lexer.stream with
    | Some(c) when is_digit c -> begin
      Stream.junk lexer.stream;
      lex_int lexer (num * 10 + int_of_digit c)
    end
    | Some(_) ->
      Some(Token.Int(num))
    | None ->
      Some(Token.Int(num))
  end

let ident_or_reserved str = begin
  try
    Hashtbl.find reserved str
  with
    | Not_found ->
      Token.Ident(str)
end
    
let rec lex_ident lexer buf =
  begin match Stream.peek lexer.stream with
    | Some(c) when is_ident_part c -> begin
      Buffer.add_char buf c;
      Stream.junk lexer.stream;
      lex_ident lexer buf
    end
    | Some(_) ->
      Some(ident_or_reserved(Buffer.contents buf))
    | None ->
      Some(ident_or_reserved(Buffer.contents buf))
  end
    
let rec next lexer =
  begin match Stream.peek lexer.stream with
    | None ->
      None
    | Some(c) -> begin
      Stream.junk lexer.stream;
      lex_token lexer c
    end
  end

and lex_token lexer c =
  begin match c with
    | '=' ->
      Some(Token.EQ)
    | '{' ->
      Some(Token.LBrace)
    | '}' ->
      Some(Token.RBrace)
    | '(' ->
      Some(Token.LParen)
    | ')' ->
      Some(Token.RParen)
    | ';' ->
      Some(Token.Semi)
    | '^' ->
      Some(Token.Hat)
    | _ when is_whitespace c ->
      next lexer
    | _ when is_digit c ->
      lex_int lexer (int_of_digit c)
    | _ when is_ident_start c ->
      let buf = Buffer.create 10 in begin
      Buffer.add_char buf c;
      lex_ident lexer buf
      end
    | _ ->
      failwith "unknown character"
  end

let of_string str =
  let strm = Stream.of_string str in
  make_lexer strm
