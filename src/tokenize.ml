open Lexer_generator
open Report
open Utils
open Options
open Symbols

let tokenize_handwritten file =
  Lexer_generator.tokenize_file file >>= fun tokens ->
  OK (List.map (fun tok -> (tok, None)) tokens)

let tokenize_ocamllex file =
  let ic = open_in file in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
      let lexbuf = Lexing.from_channel ic in
      lexbuf.Lexing.lex_curr_p <-
        { lexbuf.Lexing.lex_curr_p with pos_fname = file };
      let rec get_symbols () =
        let s = Lexer.token lexbuf in
        let ss = (s, Lexing.lexeme_start_p lexbuf) in
        if s = SYM_EOF
        then [ss]
        else ss :: get_symbols ()
      in
      let l = get_symbols () in
      OK (List.map (fun (tok, pos) -> (tok, Some pos)) l))

let lexer_exception_message = function
  | Failure message
  | Lexer.SyntaxError message -> message
  | exception_ -> Printexc.to_string exception_

let tokenize file =
  try
    if !Options.handwritten_lexer
    then tokenize_handwritten file
    else tokenize_ocamllex file
  with exception_ -> Error (lexer_exception_message exception_)

let token_category = function
  | SYM_IDENTIFIER _ -> "identifier"
  | SYM_INTEGER _ | SYM_CHARACTER _ | SYM_STRING _ -> "literal"
  | SYM_VOID | SYM_CHAR | SYM_INT | SYM_STRUCT -> "type"
  | SYM_IF | SYM_ELSE | SYM_WHILE | SYM_RETURN
  | SYM_ALLOC | SYM_PRINT | SYM_EXTERN | SYM_INCLUDE _ -> "keyword"
  | SYM_PLUS | SYM_MINUS | SYM_ASTERISK | SYM_DIV | SYM_MOD
  | SYM_EQUALITY | SYM_ASSIGN | SYM_LT | SYM_LEQ | SYM_GT | SYM_GEQ
  | SYM_NOTEQ | SYM_BOOL_NOT | SYM_BOOL_AND | SYM_BOOL_OR | SYM_ARROW
  | SYM_BITWISE_OR | SYM_BITWISE_AND | SYM_BIT_NOT | SYM_XOR
  | SYM_AMPERSAND -> "operator"
  | SYM_SEMICOLON | SYM_POINT | SYM_LPARENTHESIS | SYM_RPARENTHESIS
  | SYM_LBRACE | SYM_RBRACE | SYM_COMMA | SYM_LBRACKET | SYM_RBRACKET ->
    "punctuation"
  | SYM_EOF -> "eof"

let token_value = function
  | SYM_IDENTIFIER value -> Some value
  | SYM_INTEGER value -> Some (string_of_int value)
  | SYM_CHARACTER value -> Some (Printf.sprintf "%C" value)
  | SYM_STRING value | SYM_INCLUDE value -> Some (Printf.sprintf "%S" value)
  | _ -> None

let token_name token =
  let rendered = string_of_symbol token in
  let name_end =
    match String.index_opt rendered '(' with
    | Some index -> index
    | None -> String.length rendered
  in
  let prefix_length = String.length "SYM_" in
  if String.starts_with ~prefix:"SYM_" rendered then
    String.sub rendered prefix_length (name_end - prefix_length)
  else
    String.sub rendered 0 name_end

let lexer_report tokens =
  let render_token index (token, position) =
    let exact_token = string_of_symbol token in
    let tooltip =
      match position with
      | None -> Printf.sprintf "Token %d: %s" (index + 1) exact_token
      | Some position ->
        Printf.sprintf "Token %d: %s — %s"
          (index + 1) exact_token (string_of_position position)
    in
    let value =
      match token_value token with
      | None -> ""
      | Some value ->
        Printf.sprintf "<span class=\"lexer-token-value\">%s</span>"
          (escape_html_text value)
    in
    Printf.sprintf
      "<span class=\"lexer-token lexer-token-%s\" role=\"listitem\" title=\"%s\"><span class=\"lexer-token-index\">%d</span><span class=\"lexer-token-name\">%s</span>%s</span>"
      (token_category token) (escape_html_text tooltip) (index + 1)
      (escape_html_text (token_name token)) value
  in
  Printf.sprintf
    "<div class=\"lexer-view\"><div class=\"lexer-summary\"><span class=\"lexer-token-count\"><i class=\"fa fa-tags\" aria-hidden=\"true\"></i><strong>%d</strong> tokens</span><span class=\"lexer-summary-note\">Hover for the exact token%s</span></div><div class=\"lexer-stream\" role=\"list\" aria-label=\"Lexer token stream\">%s</div></div>"
    (List.length tokens)
    (if List.exists (fun (_, position) -> position <> None) tokens
     then " and source position" else "")
    (String.concat "" (List.mapi render_token tokens))

let pass_tokenize file =
  tokenize file >>* (fun msg ->
      record_compile_result ~error:(Some msg) "Lexing";
      Error msg
    ) $ fun tokens ->
      record_compile_result "Lexing";
      dump !tokens_save (fun oc tokens ->
          List.iter (fun (tok,_) ->
              Format.fprintf oc "%s\n" (string_of_symbol tok)
            ) tokens) tokens
        (fun _file () ->
           add_to_report "lexer" "Lexer" (Paragraph (lexer_report tokens)));
      OK tokens
