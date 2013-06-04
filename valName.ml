
open Printf

type t =
  | Id of string
  | Op of string

let show = function
  | Id(str) ->
    str
  | Op(str) ->
    sprintf "$(%s)" str
