open Printf

type t = 
  | Val of (Names.val_name * Scheme.t)


let rec show = begin fun decl ->
  begin match decl with
    | (Val (name, scm)) ->
      (((sprintf "def %s : %s\n") (Names.show_val_name name)) (Scheme.show scm))
  end
end

