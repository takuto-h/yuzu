open Printf

type t =   | EOF
  | Newline
  | Undent
  | Int of int
  | String of string
  | Char of string
  | LowId of string
  | CapId of string
  | Reserved of string
  | AssignOp of string
  | OrOp of string
  | AndOp of string
  | CmpOp of string
  | ConsOp of string
  | AddOp of string
  | MulOp of string
  | PowOp of string


let rec get_op = begin fun token ->
  begin match token with
    | ((((((((AssignOp(str)) | (OrOp(str))) | (AndOp(str))) | (CmpOp(str))) | (ConsOp(str))) | (AddOp(str))) | (MulOp(str))) | (PowOp(str))) ->
      (Some (str))
    | _ ->
      None
  end
end

let rec show = begin fun token ->
  begin match token with
    | (EOF(_)) ->
      "EOF"
    | (Newline(_)) ->
      "newline"
    | (Undent(_)) ->
      "undent"
    | (Int(_)) ->
      "integer"
    | (String(_)) ->
      "string"
    | (Char(_)) ->
      "character"
    | (LowId(_)) ->
      "lowercase identifier"
    | (CapId(_)) ->
      "capitalized identifier"
    | (((((((((Reserved(s)) | (AssignOp(s))) | (OrOp(s))) | (AndOp(s))) | (CmpOp(s))) | (ConsOp(s))) | (AddOp(s))) | (MulOp(s))) | (PowOp(s))) ->
      ((sprintf "'%s'") s)
  end
end

