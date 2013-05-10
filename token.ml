
type t =
  | EOF
  | EQ
  | LBrace
  | RBrace
  | LParen
  | RParen
  | Semi
  | Hat
  | Int of int
  | Ident of string
  | Def
  | Var
  | True
  | False
  | If
  | Else
