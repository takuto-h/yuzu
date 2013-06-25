open Printf

let rec compile = begin fun fnames ->
  begin let rec translate = begin fun fname_in ->
    begin let fname_out = begin if ((Filename.check_suffix fname_in) ".yz") then
      ((sprintf "%s.ml") ((Filename.chop_suffix fname_in) ".yz"))
    else
      ((sprintf "%s.ml") fname_in)
    end in
    ((Trans.translate_file fname_in) fname_out)
    end
  end in
  ((List.for_all translate) fnames)
  end
end

