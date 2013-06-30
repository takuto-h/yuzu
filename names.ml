open Printf

type val_name = 
  | Id of string
  | Op of string


type typector_name = string

type ctor_name = val_name

type mod_name = string

type mod_path = (mod_name) list

type val_path = (mod_path * val_name)

type typector = (mod_path * typector_name)

type ctor = (mod_path * ctor_name)

let rec show_val_name = begin fun name ->
  begin match name with
    | (Id str) ->
      str
    | (Op str) ->
      ((sprintf "( %s )") str)
  end
end

let rec show_mod_path = begin fun path ->
  begin match path with
    | ( [] ) ->
      ""
    | (( :: ) (name, names)) ->
      (((List.fold_left (sprintf "%s.%s")) name) names)
  end
end

let rec show_val_path = begin fun path ->
  begin match path with
    | (( [] ), val_name) ->
      (show_val_name val_name)
    | (mod_path, val_name) ->
      (((sprintf "%s.%s") (show_mod_path mod_path)) (show_val_name val_name))
  end
end

let rec show_typector = begin fun typector ->
  begin match typector with
    | (( [] ), typector_name) ->
      typector_name
    | (mod_path, typector_name) ->
      (((sprintf "%s.%s") (show_mod_path mod_path)) typector_name)
  end
end

let show_ctor_name = show_val_name

let show_ctor = show_val_path

