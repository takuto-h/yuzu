
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

let load_absolute loader abs_path =
  let in_channel = open_in abs_path in
  let source = Source.create abs_path in_channel in
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
          begin match top with
            | Top.LetVal(ident,_) -> begin
              eprintf "val %s : %s = %s\n"
                (Ident.show ident) (Scheme.show scm) (Value.show value);
              flush stderr
            end
            | Top.LetFun(ident,_) -> begin
              eprintf "val %s : %s = %s\n"
                (Ident.show ident) (Scheme.show scm) (Value.show value);
              flush stderr
            end
            | Top.Expr(_) -> begin
              eprintf "- : %s = %s\n" (Scheme.show scm) (Value.show value);
              flush stderr
            end
          end
        end
      end
    done
  with
    | Break -> begin
      close_in in_channel
    end
    | Failure(message) -> begin
      eprintf "%s" message;
      flush stderr;
      close_in in_channel;
    end
    | exn -> begin
      close_in_noerr in_channel;
      raise exn
    end
  end
