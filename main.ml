open Printf

let rec main = begin fun (() _) ->
  begin let compiler = (Compiler.create ()) in
  begin let result = begin if ((( = ) (Array.length Sys.argv)) 1) then
    ((YzOption.bind ((Compiler.load_iface_file compiler) "pervasives.yzi")) begin fun compiler ->
      (YzOption.return (Compiler.interactive compiler))
    end)
  else
    begin let fnames = (Array.to_list (((Array.sub Sys.argv) 1) ((( - ) (Array.length Sys.argv)) 1))) in
    ((Compiler.compile_files compiler) fnames)
    end
  end in
  begin match result with
    | (None _) ->
      (exit 1)
    | (Some (compiler)) ->
      (exit 0)
  end
  end
  end
end

let () = (main ())

