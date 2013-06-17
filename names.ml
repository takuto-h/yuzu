open Printf

type val_name =   | Id of string
  | Op of string


type typector_name = string

type ctor_name = val_name

type mod_name = string

type mod_path = (mod_name) list

type val_path = (mod_path * val_name)

type typector = (mod_path * typector_name)

type ctor = (mod_path * ctor_name)

