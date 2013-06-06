open Printf

let rec main = begin fun (()(_)) ->
  begin if ((( <> ) (Array.length Sys.argv)) 3) then
    ((eprintf "usage: %s in.yz out.ml\n") ((Array.get Sys.argv) 0))
  else
    begin if ((Trans.translate_file ((Array.get Sys.argv) 1)) ((Array.get Sys.argv) 2)) then
      (exit 0)
    else
      (exit 1)
    end
  end
end

let () = (main ())

