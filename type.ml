
open Printf

(* Pay attention to *unify* *)
type t =
  | Con of Pos.t * Ident.t
  | Var of int * (t option) ref
  | Gen of int
  | App of t * t

let rec show alist_ref array t =
  begin match t with
    | Con(_,ident) ->
      Ident.show ident
    | Var(_,tref) ->
      begin match !tref with
        | Some(tt) ->
          show alist_ref array tt
        | None ->
          begin try
            List.assq tref !alist_ref
          with
            | Not_found ->
              let str = sprintf "_t%d" (List.length !alist_ref) in begin
              alist_ref := (tref,str)::!alist_ref;
              str
              end
          end
      end
    | Gen(n) ->
      Array.get array n
    | App(App(Con(_,{Ident.name="->"}),t1),t2) ->
      sprintf "(%s -> %s)" (show alist_ref array t1) (show alist_ref array t2)
    | App(t1,t2) ->
      sprintf "%s(%s)" (show alist_ref array t1) (show alist_ref array t2)
  end

let rec show_origin str_t t =
  begin match t with
    | Con(pos,_) ->
      sprintf "%s: %s\n%s" (Pos.show pos) str_t (Pos.show_source pos)
    | Var(_,tref) ->
      begin match !tref with
        | None -> ""
        | Some(tt) -> show_origin str_t tt
      end
    | Gen(_) ->
      ""
    | App(t1,_) ->
      show_origin str_t t1
  end

let rec occurs t1ref t2 =
  begin match t2 with
    | Con(_,_) ->
      false
    | Var(_,t2ref) when t2ref == t1ref ->
      true
    | Var(_,t2ref) ->
      begin match !t2ref with
        | Some(t20) -> occurs t1ref t20
        | None -> false
      end
    | Gen(_) ->
      assert false
    | App(t21,t22) ->
      occurs t1ref t21 || occurs t1ref t22
  end

let unif_failed pos t1 t2 =
  let alist_ref = ref [] in
  let array = Array.make 0 "" in
  let str_t1 = show alist_ref array t1 in
  let str_t2 = show alist_ref array t2 in
  failwith
    (sprintf
       "%s: error: unification failed: %s and %s\n%s%s%s"
       (Pos.show pos) str_t1 str_t2 (Pos.show_source pos)
       (show_origin str_t1 t1) (show_origin str_t2 t2))

let rec unify pos t1 t2 =
  begin match (t1,t2) with
    | (Con(_,id1),Con(_,id2)) when id1 = id2 ->
      ()
    | (Var(_,t1ref),Var(_,t2ref)) when t1ref == t2ref ->
      ()
    | (Var(t1lv,t1ref),Var(t2lv,t2ref)) ->
      begin match (!t1ref,!t2ref) with
        | (Some(t10),_) ->
          unify pos t10 t2
        | (_,Some(t20)) ->
          unify pos t1 t20
        | (None,None) when t1lv > t2lv ->
          t1ref := Some(t2)
        | (None,None) when t1lv < t2lv ->
          t2ref := Some(t1)
        | (None,None) ->
          t2ref := Some(t1)
      end
    | (Var(_,t1ref),_) ->
      begin match !t1ref with
        | Some(t10) ->
          unify pos t10 t2
        | None when occurs t1ref t2 ->
          failwith (unif_failed pos t1 t2)
        | None ->
          t1ref := Some(t2)
      end
    | (_,Var(_,t2ref)) ->
      begin match !t2ref with
        | Some(t20) ->
          unify pos t20 t1
        | None when occurs t2ref t1 ->
          failwith (unif_failed pos t1 t2)
        | None ->
          t2ref := Some(t1)
      end
    | (App(t11,t12),App(t21,t22)) -> begin
      unify pos t11 t21;
      unify pos t12 t22
    end
    | (_,_) ->
      failwith (unif_failed pos t1 t2)
  end
      
module Open = struct
  let (@->) t1 t2 = fun pos -> App(App(Con(pos,Ident.intern "->"),t1),t2)
end
