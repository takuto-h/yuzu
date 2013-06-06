open Printf

type t =
  | Int of int
  | String of string
  | Char of string

let rec show = begin fun lit ->
  begin match lit with
    | (Int(n)) ->
      ((sprintf "%d") n)
    | (String(str)) ->
      ((sprintf "\"%s\"") str)
    | (Char(str)) ->
      ((sprintf "'%s'") str)
  end
end

