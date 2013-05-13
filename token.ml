
type t =
  | EOF
  | EQ
  | LBrace
  | RBrace
  | LParen
  | RParen
  | Semi
  | Colon
  | Hat
  | Minus
  | RArrow
  | Int of int
  | Ident of string
  | Def
  | Var
  | True
  | False
  | If
  | Else

let show token =
  begin match token with
    | EOF -> "EOF"
    | EQ -> "'='"
    | LBrace -> "'{'"
    | RBrace -> "'}'"
    | LParen -> "'('"
    | RParen -> "')'"
    | Semi -> "';'"
    | Colon -> "':'"
    | Hat -> "'^'"
    | Minus -> "'-'"
    | RArrow -> "'->'"
    | Int(_) -> "integer"
    | Ident(_) -> "identifier"
    | Def -> "'def'"
    | Var -> "'var'"
    | True -> "'true'"
    | False -> "'false'"
    | If -> "'if'"
    | Else -> "'else'"
  end
