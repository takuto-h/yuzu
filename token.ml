
type t =
  | EOF
  | Def
  | Var
  | EQ
  | LBrace
  | RBrace
  | LParen
  | RParen
  | Semi
  | Hat
  | Int of int
  | Ident of string
