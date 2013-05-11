
open Printf

type t = {gen_num:int; body:Type.t}

let mono t = {gen_num=0; body=t}

let poly n t = {gen_num=n; body=t}

let show {gen_num=n; body=t} =
  let alist_ref = ref [] in
  let type_var_strs = Array.init n (fun i -> sprintf "t%d" i) in
  Type.show alist_ref type_var_strs t
