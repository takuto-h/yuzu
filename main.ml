
open Printf

let rec read buf = begin
  eprintf "> ";
  flush stderr;
  let str = read_line () in
  let len = String.length str in
  if len >= 2 && String.sub str (len-2) 2 = ";;" then begin
    Buffer.add_string buf (String.sub str 0 (len-2));
    Buffer.contents buf
  end
  else begin
    Buffer.add_string buf str;
    read buf
  end
end
  
let rec repl inf = begin
  let buf = Buffer.create 256 in
  let str = read buf in
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
