
open Printf

type t =
  | EOF
  | Newline
  | Undent
  | Int of int
  | Ident of string
  | String of string
  | Char of string
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
  | Ident(_) ->
    "identifier"
  | String(_) ->
    "string"
  | Char(_) ->
    "character"
  | Reserved(s) | OrOp(s) | AndOp(s) | CmpOp(s)
  | ConsOp(s) | AddOp(s) | MulOp(s) | PowOp(s) ->
    sprintf "'%s'" s
