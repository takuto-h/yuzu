
open Printf

type val_name =
  | Id of string
  | Op of string

type typector_name = string
type ctor_name = string
type mod_name = string
type mod_path = mod_name list
type val_path = mod_path * val_name
type typector = mod_path * typector_name
type ctor = mod_path * ctor_name

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

let show_ctor = function
  | ([], ctor_name) ->
    ctor_name
  | (mod_path, ctor_name) ->
    sprintf "%s.%s" (show_mod_path mod_path) ctor_name
