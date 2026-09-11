(* Rendering of saved lexer output for the standalone HTML reporter. *)

open Report

let lexer_report_strings rendered_tokens =
  let token_name rendered =
    let name_end = Option.value ~default:(String.length rendered)
        (String.index_opt rendered '(') in
    if String.starts_with ~prefix:"SYM_" rendered then
      String.sub rendered 4 (name_end - 4)
    else String.sub rendered 0 name_end
  in
  let token_value rendered =
    match String.index_opt rendered '(' with
    | None -> None
    | Some start ->
      let length = String.length rendered in
      if length > start + 1 && rendered.[length - 1] = ')' then
        Some (String.sub rendered (start + 1) (length - start - 2))
      else None
  in
  let token_category = function
    | "IDENTIFIER" -> "identifier"
    | "INTEGER" | "CHARACTER" | "STRING" -> "literal"
    | "VOID" | "CHAR" | "INT" | "STRUCT" -> "type"
    | "IF" | "ELSE" | "WHILE" | "RETURN" | "ALLOC" | "PRINT" | "EXTERN" | "INCLUDE" -> "keyword"
    | "PLUS" | "MINUS" | "ASTERISK" | "DIV" | "MOD" | "EQUALITY" | "ASSIGN"
    | "LT" | "LEQ" | "GT" | "GEQ" | "NOTEQ" | "BOOL_NOT" | "BOOL_AND" | "BOOL_OR"
    | "ARROW" | "BITWISE_OR" | "BITWISE_AND" | "BIT_NOT" | "XOR" | "AMPERSAND" -> "operator"
    | "SEMICOLON" | "POINT" | "LPARENTHESIS" | "RPARENTHESIS" | "LBRACE" | "RBRACE"
    | "COMMA" | "LBRACKET" | "RBRACKET" -> "punctuation"
    | "EOF" -> "eof"
    | _ -> "punctuation"
  in
  let render_token index rendered =
    let name = token_name rendered in
    let value = match token_value rendered with
      | None -> ""
      | Some value -> Printf.sprintf "<span class=\"lexer-token-value\">%s</span>"
                        (escape_html_text value) in
    Printf.sprintf
      "<span class=\"lexer-token lexer-token-%s\" role=\"listitem\" title=\"Token %d: %s\"><span class=\"lexer-token-index\">%d</span><span class=\"lexer-token-name\">%s</span>%s</span>"
      (token_category name) (index + 1) (escape_html_text rendered) (index + 1)
      (escape_html_text name) value
  in
  Printf.sprintf
    "<div class=\"lexer-view\"><div class=\"lexer-summary\"><span class=\"lexer-token-count\"><i class=\"fa fa-tags\" aria-hidden=\"true\"></i><strong>%d</strong> tokens</span><span class=\"lexer-summary-note\">Hover for the exact token</span></div><div class=\"lexer-stream\" role=\"list\" aria-label=\"Lexer token stream\">%s</div></div>"
    (List.length rendered_tokens) (String.concat "" (List.mapi render_token rendered_tokens))
