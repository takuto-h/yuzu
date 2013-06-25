open Printf

let rec main = begin fun (() _) ->
  begin if ((( = ) (Array.length Sys.argv)) 1) then
    (Compiler.interactive ())
  else
    begin let fnames = (Array.to_list (((Array.sub Sys.argv) 1) ((( - ) (Array.length Sys.argv)) 1))) in
    begin if (Compiler.compile fnames) then
      (exit 0)
    else
      (exit 1)
    end
    end
  end
end

let () = (main ())

