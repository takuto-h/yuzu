
open Printf

type t = {
  value : string list;
}

let make mod_names = {
  value = mod_names;
}

let show = function
  | {value=[]} ->
    ""
  | {value=name::names} ->
    List.fold_left begin fun acc elem ->
      sprintf "%s.%s" acc elem
    end name names
