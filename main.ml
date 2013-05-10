
open Printf

let rec repl inf = begin
  eprintf "> ";
  flush stderr;
  let str = read_line () in
  let parser = Parser.of_string str in
  begin try
    let top = Parser.parse parser in
    let (scm, inf) = Inferrer.infer_top inf top in
    eprintf "%s\n" (Scheme.show scm);
    repl inf
  with
    | End_of_file -> ()
  end
end

let rec main () =
  begin try
    repl Inferrer.empty
  with
    | End_of_file -> ()
  end
