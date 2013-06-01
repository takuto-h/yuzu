
open Printf

type t = string list * Name.t

let make mod_names val_name =
  (mod_names, val_name)

let show (mod_names, val_name) =
  List.fold_right begin fun elem acc ->
    sprintf "%s.%s" elem acc
  end mod_names (Name.show val_name)
