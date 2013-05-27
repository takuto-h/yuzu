
open Printf

type t = {
  src : Source.t;
}

let create src = {
  src = src;
}

let is_digit c =
  String.contains "0123456789" c

let is_whitespace c =
  String.contains " \t\r\n" c

let int_of_digit c =
  Char.code c - Char.code '0'

let rec lex_int lexer n =
  match Source.peek lexer.src with
    | Some(c) when is_digit c ->
      Source.junk lexer.src;
      lex_int lexer (n * 10 + int_of_digit c)
    | Some(_)
    | None ->
      Token.Int(n)

let lex_token lexer = function
  | ';'as c ->
    Token.Just(c)
  | c when is_digit c ->
    lex_int lexer (int_of_digit c)
  | c ->
    let pos = Source.pos lexer.src in
    failwith (sprintf "%s: error: unknown character: '%c'\n" (Pos.show pos) c)

let rec next lexer =
  let pos = Source.pos lexer.src in
  match Source.peek lexer.src with
    | None ->
      (None, pos)
    | Some(c) when is_whitespace c ->
      Source.junk lexer.src;
      next lexer
    | Some(c) ->
      Source.junk lexer.src;
      (Some(lex_token lexer c), pos)
