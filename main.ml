let preloads = (( :: ) ("pervasives.yzi", (( :: ) ("array.yzi", (( :: ) ("buffer.yzi", (( :: ) ("char.yzi", (( :: ) ("filename.yzi", (( :: ) ("list.yzi", (( :: ) ("printf.yzi", (( :: ) ("stack.yzi", (( :: ) ("stream.yzi", (( :: ) ("string.yzi", (( :: ) ("sys.yzi", ( [] )))))))))))))))))))))))

let rec main = begin fun () ->
  begin let compiler = (Compiler.create ()) in
  begin let result = ((YzOption.bind ((Compiler.load_and_compile compiler) preloads)) begin fun compiler ->
    begin if ((( = ) (Array.length Sys.argv)) 1) then
      (YzOption.return (Compiler.interactive compiler))
    else
      begin let fnames = (Array.to_list (((Array.sub Sys.argv) 1) ((( - ) (Array.length Sys.argv)) 1))) in
      ((Compiler.load_and_compile compiler) fnames)
      end
    end
  end) in
  begin match result with
    | None ->
      (exit 1)
    | (Some compiler) ->
      (exit 0)
  end
  end
  end
end

let () = (main ())

