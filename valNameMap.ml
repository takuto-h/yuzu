
include Map.Make(ValName)

let equal_keys map1 map2 =
  let bind1 = bindings map1 in
  let bind2 = bindings map2 in
  try
    List.for_all2 (fun (k1,_) (k2,_) -> k1 = k2) bind1 bind2
  with
    | Invalid_argument(_) ->
      false
