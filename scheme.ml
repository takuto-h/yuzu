
type t = {gen_num:int; body:Type.t}

let mono t = {gen_num=0; body=t}

let poly n t = {gen_num=n; body=t}
