
open Printf

exception Break

type t = {
  mutable inf : Inferrer.t;
  mutable eva : Eva.t;
}

let create () = {
  inf = Inferrer.empty;
  eva = Eva.empty;
}

let load loader fname =
  let in_channel = open_in fname in
  let source = Source.create fname in_channel in
  let lexer = Lexer.create source in
  let parser = Parser.create lexer in
  begin try
    while true do
      begin match Parser.parse parser with
        | None ->
          raise Break
        | Some(top) ->
          let (scm, inf) = Inferrer.infer_top loader.inf top in
          let (value, eva) = Eva.eval_top loader.eva top in begin
          loader.inf <- inf;
          loader.eva <- eva;
          begin match top.Top.raw with
            | Top.LetVal(ident,_) ->
              eprintf "def %s : %s = %s\n"
                (Ident.show ident) (Scheme.show scm) (Value.show value)
            | Top.LetFun(ident,_) ->
              eprintf "def %s : %s = %s\n"
                (Ident.show ident) (Scheme.show scm) (Value.show value)
            | Top.Expr(_) ->
              eprintf "- : %s = %s\n" (Scheme.show scm) (Value.show value)
          end;
          flush stderr
          end
      end
    done
  with
    | Break -> begin
      close_in in_channel
    end
    | Failure(message) -> begin
      close_in in_channel;
      eprintf "%s" message;
      flush stderr
    end
    | exn -> begin
      close_in_noerr in_channel;
      raise exn
    end
  end

let load_decls loader fname =
  let in_channel = open_in fname in
  let source = Source.create fname in_channel in
  let lexer = Lexer.create source in
  let parser = Parser.create lexer in
  begin try
    while true do
      begin match Parser.parse_decl parser with
        | None ->
          raise Break
        | Some(decl) -> begin
          begin match decl with
            | Decl.Decl(ident,scm) ->
              eprintf "def %s : %s\n"
                (Ident.show ident) (Scheme.show scm)
          end;
          flush stderr
        end
      end
    done
  with
    | Break -> begin
      close_in in_channel
    end
    | Failure(message) -> begin
      close_in in_channel;
      eprintf "%s" message;
      flush stderr
    end
    | exn -> begin
      close_in_noerr in_channel;
      raise exn
    end
  end

let create_and_load decls fname =
  let loader = create () in begin
  load_decls loader decls;
  load loader fname
  end
