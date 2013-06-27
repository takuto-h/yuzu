open Printf

let rec main = begin fun (() _) ->
  begin let compiler = (Compiler.create ()) in
  begin if ((( = ) (Array.length Sys.argv)) 1) then
    (ignore (Compiler.interactive compiler))
  else
    begin let fnames = (Array.to_list (((Array.sub Sys.argv) 1) ((( - ) (Array.length Sys.argv)) 1))) in
    begin match ((Compiler.compile_files compiler) fnames) with
      | (None _) ->
        (exit 1)
      | (Some (compiler)) ->
        (exit 0)
    end
    end
  end
  end
end

let () = (main ())

