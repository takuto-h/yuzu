
open Printf

let main () =
  if Array.length Sys.argv <> 3 then
    eprintf "usage: %s in.yz out.ml\n" (Array.get Sys.argv 0)
  else
    Trans.translate_file (Array.get Sys.argv 1) (Array.get Sys.argv 2)

let () = main ()
