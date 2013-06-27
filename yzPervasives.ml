let rec with_open_in = begin fun fname_in ->
  begin fun func ->
    begin let chan_in = (open_in fname_in) in
    begin try
      begin let x = (func chan_in) in
      begin
      (close_in chan_in);
      x
      end
      end
    with

      | exn ->
        begin
        (close_in_noerr chan_in);
        (raise exn)
        end
    end
    end
  end
end

let rec with_open_out = begin fun fname_out ->
  begin fun func ->
    begin let chan_out = (open_out fname_out) in
    begin try
      begin let x = (func chan_out) in
      begin
      (close_out chan_out);
      x
      end
      end
    with

      | exn ->
        begin
        (close_out_noerr chan_out);
        (raise exn)
        end
    end
    end
  end
end

