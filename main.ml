
open Printf

let rec read buf = begin
  begin try
    while true do
      eprintf "> ";
      flush stderr;
      let str = read_line () in
      let len = String.length str in
      if len >= 2 && String.sub str (len-2) 2 = ";;" then begin
        Buffer.add_string buf (String.sub str 0 (len-2));
        raise End_of_file
      end
      else begin
        Buffer.add_string buf str
      end
    done
  with
    | End_of_file -> ()
  end;
  Buffer.contents buf
end

let rec repl () = begin
  let infref = ref Inferrer.empty in
  let evaref = ref Eva.empty in
  while true do
    let buf = Buffer.create 256 in
    let str = read buf in
    let parser = Parser.of_string str in
    begin try
      let top = Parser.parse parser in
      let (scm, inf) = Inferrer.infer_top !infref top in
      let (value, eva) = Eva.eval_top !evaref top in begin
      infref := inf;
      evaref := eva;
      eprintf "%s : %s\n" (Value.show value) (Scheme.show scm)
      end
    with
      | Failure(message) ->
        eprintf "%s\n" message
    end
  done
end

let rec main () =
  begin try
    repl ()
  with
    | End_of_file -> ()
  end
