
open Printf

type val_name =
  | Id of string
  | Op of string

type mod_name = string
type mod_path = mod_name list
type val_path = mod_path * val_name

let show_val_name = function
  | Id(str) ->
    str
  | Op(str) ->
    sprintf "$(%s)" str

let show_mod_path = function
  | [] ->
    ""
  | name::names ->
    List.fold_left begin fun acc elem ->
      sprintf "%s.%s" acc elem
    end name names

let show_val_path = function
  | ([], val_name) ->
    show_val_name val_name
  | (mod_path, val_name) ->
    sprintf "%s.%s" (show_mod_path mod_path) (show_val_name val_name)
