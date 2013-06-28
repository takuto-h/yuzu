open Printf

let rec main = begin fun () ->
  begin let compiler = (Compiler.create ()) in
  begin let result = ((YzOption.bind ((Compiler.load_iface_file compiler) "pervasives.yzi")) begin fun compiler ->
    ((YzOption.bind ((Compiler.load_iface_file compiler) "list.yzi")) begin fun compiler ->
      begin if ((( = ) (Array.length Sys.argv)) 1) then
        (YzOption.return (Compiler.interactive compiler))
      else
        begin let fnames = (Array.to_list (((Array.sub Sys.argv) 1) ((( - ) (Array.length Sys.argv)) 1))) in
        ((Compiler.compile_files compiler) fnames)
        end
      end
    end)
  end) in
  begin match result with
    | None ->
      (exit 1)
    | (Some (compiler)) ->
      (exit 0)
  end
  end
  end
end

let () = (main ())

