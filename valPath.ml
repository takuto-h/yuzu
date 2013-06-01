
open Printf

type t = ModPath.t * ValName.t

let make mod_path val_name =
  (mod_path, val_name)

let show = function
  | ({ModPath.value=[]}, val_name) ->
    ValName.show val_name
  | (mod_path, val_name) ->
    sprintf "%s.%s" (ModPath.show mod_path) (ValName.show val_name)
