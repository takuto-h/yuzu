
open Printf

type t =
  | EOF
  | Newline
  | Undent
  | Int of int
  | String of string
  | Char of string
  | VarId of string
  | ConId of string
  | Reserved of string
  | OrOp of string
  | AndOp of string
  | CmpOp of string
  | ConsOp of string
  | AddOp of string
  | MulOp of string
  | PowOp of string

let show = function
  | EOF ->
    "EOF"
  | Newline ->
    "newline"
  | Undent ->
    "undent"
  | Int(_) ->
    "integer"
  | String(_) ->
    "string"
  | Char(_) ->
    "character"
  | VarId(_) ->
    "lowercase identifier"
  | ConId(_) ->
    "capitalized identifier"
  | Reserved(s) | OrOp(s) | AndOp(s) | CmpOp(s)
  | ConsOp(s) | AddOp(s) | MulOp(s) | PowOp(s) ->
    sprintf "'%s'" s
