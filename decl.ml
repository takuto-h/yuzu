open Printf

type t = 
  | Anon of Scheme.t


let rec show = begin fun decl ->
  begin match decl with
    | (Anon (scm)) ->
      ((sprintf "def _ : %s\n") (Scheme.show scm))
  end
end

