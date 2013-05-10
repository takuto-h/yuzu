
open Printf

type t = {gen_num:int; body:Type.t}

let mono t = {gen_num=0; body=t}

let poly n t = {gen_num=n; body=t}

let show {gen_num=n; body=t} =
  let alist = ref [] in
  let type_var_strs = Array.init n (fun i -> sprintf "t%d" i) in
  let rec loop t =
    begin match t with
      | Type.Con(ident) ->
        Ident.show ident
      | Type.Var(_,tref) ->
        begin match !tref with
          | Some(tt) ->
            loop tt
          | None ->
            begin try
              List.assq tref !alist
            with
              | Not_found ->
                let str = sprintf "_t%d" (List.length !alist) in begin
                alist := (tref,str)::!alist;
                str
                end
            end
        end
      | Type.Gen(n) ->
        Array.get type_var_strs n
      | Type.App(Type.App(Type.Con({Ident.name="->"}),t1),t2) ->
        sprintf "(%s -> %s)" (loop t1) (loop t2)
      | Type.App(t1,t2) ->
        sprintf "%s(%s)" (loop t1) (loop t2)
    end
  in loop t
