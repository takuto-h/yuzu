open Printf

type t = {
  source : Source.t;
  parens : (string) Stack.t;
  offside_lines : (int) Stack.t;
  mutable is_bol : bool;
  mutable is_bob : bool;
}

let initial_buffer_size = 16

let reserved = (( :: ) ("open", (( :: ) ("type", (( :: ) ("def", (( :: ) ("rec", (( :: ) ("var", (( :: ) ("if", (( :: ) ("else", (( :: ) ("match", (( :: ) ("case", (( :: ) ("when", (( :: ) ("mutable", []))))))))))))))))))))))

let rec create = begin fun source ->
  begin let lexer = {
    source = source;
    parens = (Stack.create ());
    offside_lines = (Stack.create ());
    is_bol = false;
    is_bob = false;
  } in
  begin
  ((Stack.push 0) lexer.offside_lines);
  lexer
  end
  end
end

let rec indent = begin fun lexer ->
  begin if (Stack.is_empty lexer.parens) then
    (lexer.is_bob <- true)
  else
    begin let pos = (Source.pos lexer.source) in
    (failwith ((sprintf "%s: error: layout inside parentheses\n") (Pos.show pos)))
    end
  end
end

let rec is_digit = begin fun c ->
  ((String.contains "0123456789") c)
end

let rec is_lowid_start = begin fun c ->
  ((String.contains "abcdefghijklmnopqrstuvwxyz_") c)
end

let rec is_capid_start = begin fun c ->
  ((String.contains "ABCDEFGHIJKLMNOPQRSTUVWXYZ") c)
end

let rec is_id_part = begin fun c ->
  ((( || ) (is_lowid_start c)) ((( || ) (is_capid_start c)) (is_digit c)))
end

let rec is_op_part = begin fun c ->
  ((String.contains "=<>|&+-*/%") c)
end

let rec is_whitespace = begin fun c ->
  ((String.contains " \t\r\n") c)
end

let rec int_of_digit = begin fun c ->
  ((( - ) (Char.code c)) (Char.code '0'))
end

let rec lex_close_paren = begin fun lexer ->
  begin fun pos ->
    begin fun open_paren ->
      begin fun close_paren ->
        begin if ((( || ) (Stack.is_empty lexer.parens)) ((( <> ) (Stack.top lexer.parens)) open_paren)) then
          (failwith (((sprintf "%s: error: unmatched parentheses: '%s'\n") (Pos.show pos)) close_paren))
        else
          begin
          (ignore (Stack.pop lexer.parens));
          (Token.Reserved (close_paren))
          end
        end
      end
    end
  end
end

let rec lex_op = begin fun lexer ->
  begin fun buf ->
    begin match (Source.peek lexer.source) with
      | (Some(c)) when (is_op_part c) ->
        begin
        ((Buffer.add_char buf) c);
        begin
        (Source.junk lexer.source);
        ((lex_op lexer) buf)
        end
        end
      | ((Some(_)) | (None(_))) ->
        (Buffer.contents buf)
    end
  end
end

let rec lex_int = begin fun lexer ->
  begin fun n ->
    begin match (Source.peek lexer.source) with
      | (Some(c)) when (is_digit c) ->
        begin
        (Source.junk lexer.source);
        ((lex_int lexer) ((( + ) ((( * ) n) 10)) (int_of_digit c)))
        end
      | ((Some(_)) | (None(_))) ->
        (Token.Int (n))
    end
  end
end

let rec lex_string = begin fun lexer ->
  begin fun buf ->
    begin match (Source.peek lexer.source) with
      | (Some('"')) ->
        begin
        (Source.junk lexer.source);
        (Token.String ((Buffer.contents buf)))
        end
      | (Some('\\')) ->
        begin
        (Source.junk lexer.source);
        begin match (Source.peek lexer.source) with
          | (Some('"')) ->
            begin
            (Source.junk lexer.source);
            begin
            ((Buffer.add_string buf) "\\\"");
            ((lex_string lexer) buf)
            end
            end
          | (Some(c)) ->
            begin
            (Source.junk lexer.source);
            begin
            ((Buffer.add_char buf) '\\');
            begin
            ((Buffer.add_char buf) c);
            ((lex_string lexer) buf)
            end
            end
            end
          | (None(_)) ->
            begin let pos_eof = (Source.pos lexer.source) in
            (failwith ((sprintf "%s: error: EOF inside a string literal\n") (Pos.show pos_eof)))
            end
        end
        end
      | (Some(c)) ->
        begin
        (Source.junk lexer.source);
        begin
        ((Buffer.add_char buf) c);
        ((lex_string lexer) buf)
        end
        end
      | (None(_)) ->
        begin let pos_eof = (Source.pos lexer.source) in
        (failwith ((sprintf "%s: error: EOF inside a string literal\n") (Pos.show pos_eof)))
        end
    end
  end
end

let rec lex_char = begin fun lexer ->
  begin fun buf ->
    begin match (Source.peek lexer.source) with
      | (Some('\'')) ->
        begin
        (Source.junk lexer.source);
        (Token.Char ((Buffer.contents buf)))
        end
      | (Some('\\')) ->
        begin
        (Source.junk lexer.source);
        begin match (Source.peek lexer.source) with
          | (Some('\'')) ->
            begin
            (Source.junk lexer.source);
            begin
            ((Buffer.add_string buf) "\\'");
            ((lex_char lexer) buf)
            end
            end
          | (Some(c)) ->
            begin
            (Source.junk lexer.source);
            begin
            ((Buffer.add_char buf) '\\');
            begin
            ((Buffer.add_char buf) c);
            ((lex_char lexer) buf)
            end
            end
            end
          | (None(_)) ->
            begin let pos_eof = (Source.pos lexer.source) in
            (failwith ((sprintf "%s: error: EOF inside a string literal\n") (Pos.show pos_eof)))
            end
        end
        end
      | (Some(c)) ->
        begin
        (Source.junk lexer.source);
        begin
        ((Buffer.add_char buf) c);
        ((lex_char lexer) buf)
        end
        end
      | (None(_)) ->
        begin let pos_eof = (Source.pos lexer.source) in
        (failwith ((sprintf "%s: error: EOF inside a character literal\n") (Pos.show pos_eof)))
        end
    end
  end
end

let rec lowid_or_reserved = begin fun str ->
  begin if ((List.mem str) reserved) then
    (Token.Reserved (str))
  else
    (Token.LowId (str))
  end
end

let rec lex_lowid = begin fun lexer ->
  begin fun buf ->
    begin match (Source.peek lexer.source) with
      | (Some(c)) when (is_id_part c) ->
        begin
        ((Buffer.add_char buf) c);
        begin
        (Source.junk lexer.source);
        ((lex_lowid lexer) buf)
        end
        end
      | ((Some(_)) | (None(_))) ->
        (lowid_or_reserved (Buffer.contents buf))
    end
  end
end

let rec lex_capid = begin fun lexer ->
  begin fun buf ->
    begin match (Source.peek lexer.source) with
      | (Some(c)) when (is_id_part c) ->
        begin
        ((Buffer.add_char buf) c);
        begin
        (Source.junk lexer.source);
        ((lex_capid lexer) buf)
        end
        end
      | ((Some(_)) | (None(_))) ->
        (Token.CapId ((Buffer.contents buf)))
    end
  end
end

let rec lex_visible_token = begin fun lexer ->
  begin fun pos ->
    begin fun c ->
      begin
      (Source.junk lexer.source);
      begin match c with
        | ((((';' | ',') | '^') | '.') | '$') ->
          (Token.Reserved (((sprintf "%c") c)))
        | (('(' | '{') | '[') ->
          begin
          ((Stack.push ((sprintf "%c") c)) lexer.parens);
          (Token.Reserved (((sprintf "%c") c)))
          end
        | ')' ->
          ((((lex_close_paren lexer) pos) "(") ")")
        | '}' ->
          ((((lex_close_paren lexer) pos) "{") "}")
        | ']' ->
          ((((lex_close_paren lexer) pos) "[") "]")
        | '<' ->
          begin match (Source.peek lexer.source) with
            | (Some('-')) ->
              begin
              (Source.junk lexer.source);
              (Token.AssignOp ("<-"))
              end
            | ((Some(_)) | (None(_))) ->
              begin let buf = (Buffer.create initial_buffer_size) in
              begin
              ((Buffer.add_char buf) c);
              (Token.CmpOp (((lex_op lexer) buf)))
              end
              end
          end
        | '|' ->
          begin match (Source.peek lexer.source) with
            | (Some('|')) ->
              begin
              (Source.junk lexer.source);
              (Token.OrOp ("||"))
              end
            | ((Some(_)) | (None(_))) ->
              begin let buf = (Buffer.create initial_buffer_size) in
              begin
              ((Buffer.add_char buf) c);
              (Token.CmpOp (((lex_op lexer) buf)))
              end
              end
          end
        | '&' ->
          begin match (Source.peek lexer.source) with
            | (Some('&')) ->
              begin
              (Source.junk lexer.source);
              (Token.AndOp ("&&"))
              end
            | ((Some(_)) | (None(_))) ->
              begin let buf = (Buffer.create initial_buffer_size) in
              begin
              ((Buffer.add_char buf) c);
              (Token.CmpOp (((lex_op lexer) buf)))
              end
              end
          end
        | ('=' | '>') ->
          begin let buf = (Buffer.create initial_buffer_size) in
          begin
          ((Buffer.add_char buf) c);
          (Token.CmpOp (((lex_op lexer) buf)))
          end
          end
        | ('+' | '-') ->
          begin let buf = (Buffer.create initial_buffer_size) in
          begin
          ((Buffer.add_char buf) c);
          (Token.AddOp (((lex_op lexer) buf)))
          end
          end
        | '%' ->
          begin let buf = (Buffer.create initial_buffer_size) in
          begin
          ((Buffer.add_char buf) c);
          (Token.MulOp (((lex_op lexer) buf)))
          end
          end
        | '*' ->
          begin match (Source.peek lexer.source) with
            | (Some('*')) ->
              begin
              (Source.junk lexer.source);
              (Token.PowOp ("**"))
              end
            | ((Some(_)) | (None(_))) ->
              begin let buf = (Buffer.create initial_buffer_size) in
              begin
              ((Buffer.add_char buf) c);
              (Token.MulOp (((lex_op lexer) buf)))
              end
              end
          end
        | ':' ->
          begin match (Source.peek lexer.source) with
            | (Some(':')) ->
              begin
              (Source.junk lexer.source);
              (Token.ConsOp ("::"))
              end
            | ((Some(_)) | (None(_))) ->
              (Token.Reserved (":"))
          end
        | '"' ->
          begin let buf = (Buffer.create initial_buffer_size) in
          ((lex_string lexer) buf)
          end
        | '\'' ->
          begin let buf = (Buffer.create initial_buffer_size) in
          ((lex_char lexer) buf)
          end
        | _ when (is_digit c) ->
          ((lex_int lexer) (int_of_digit c))
        | _ when (is_lowid_start c) ->
          begin let buf = (Buffer.create initial_buffer_size) in
          begin
          ((Buffer.add_char buf) c);
          ((lex_lowid lexer) buf)
          end
          end
        | _ when (is_capid_start c) ->
          begin let buf = (Buffer.create initial_buffer_size) in
          begin
          ((Buffer.add_char buf) c);
          ((lex_capid lexer) buf)
          end
          end
        | _ ->
          (failwith (((sprintf "%s: error: unknown character: '%c'\n") (Pos.show pos)) c))
      end
      end
    end
  end
end

let rec lex_token = begin fun lexer ->
  begin fun pos ->
    begin fun c ->
      begin let offset = ((( - ) pos.Pos.cnum) pos.Pos.bol) in
      begin if lexer.is_bob then
        begin
        (lexer.is_bob <- false);
        begin
        (lexer.is_bol <- false);
        begin
        ((Stack.push offset) lexer.offside_lines);
        (((lex_visible_token lexer) pos) c)
        end
        end
        end
      else
        begin if lexer.is_bol then
          begin let offside_line = (Stack.top lexer.offside_lines) in
          begin if ((( < ) offset) offside_line) then
            begin
            (ignore (Stack.pop lexer.offside_lines));
            Token.Undent
            end
          else
            begin if ((( = ) offset) offside_line) then
              begin
              (lexer.is_bol <- false);
              Token.Newline
              end
            else
              begin
              (lexer.is_bol <- false);
              (((lex_visible_token lexer) pos) c)
              end
            end
          end
          end
        else
          (((lex_visible_token lexer) pos) c)
        end
      end
      end
    end
  end
end

let rec skip_single_line_comment = begin fun lexer ->
  begin match (Source.peek lexer.source) with
    | ((None(_)) | (Some('\n'))) ->
      ()
    | (Some(_)) ->
      begin
      (Source.junk lexer.source);
      (skip_single_line_comment lexer)
      end
  end
end

let rec next = begin fun lexer ->
  begin let pos = (Source.pos lexer.source) in
  begin match (Source.peek lexer.source) with
    | (None(_)) when ((( = ) (Stack.length lexer.offside_lines)) 1) ->
      (None, pos)
    | (None(_)) ->
      begin
      (ignore (Stack.pop lexer.offside_lines));
      ((Some (Token.Undent)), pos)
      end
    | (Some('\n')) when (Stack.is_empty lexer.parens) ->
      begin
      (lexer.is_bol <- true);
      begin
      (Source.junk lexer.source);
      (next lexer)
      end
      end
    | (Some(c)) when (is_whitespace c) ->
      begin
      (Source.junk lexer.source);
      (next lexer)
      end
    | (Some('/')) ->
      begin
      (Source.junk lexer.source);
      begin match (Source.peek lexer.source) with
        | (Some('/')) ->
          begin
          (Source.junk lexer.source);
          begin
          (skip_single_line_comment lexer);
          (next lexer)
          end
          end
        | ((Some(_)) | (None(_))) ->
          begin let buf = (Buffer.create initial_buffer_size) in
          begin
          ((Buffer.add_char buf) '/');
          ((Some ((Token.MulOp (((lex_op lexer) buf))))), pos)
          end
          end
      end
      end
    | (Some(c)) ->
      ((Some ((((lex_token lexer) pos) c))), pos)
  end
  end
end

