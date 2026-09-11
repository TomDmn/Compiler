open Options
open Utils

type html_node =
  | Img of string
  | DeferredImg of string * int
  | Code of string
  | Paragraph of string
  | List of html_node list

let rec print_html oc = function
    Img s ->
    Printf.fprintf oc
      "<a class=\"report-image-link\" href=\"%s\" target=\"_blank\" title=\"Open diagram\"><img class=\"report-image\" src=\"%s\" loading=\"lazy\" /></a>\n"
      s s
  | DeferredImg (path, bytes) ->
    Printf.fprintf oc
      "<div class=\"deferred-image\" data-image-src=\"%s\"><span class=\"deferred-image-icon\" aria-hidden=\"true\"><i class=\"fa fa-project-diagram\"></i></span><span class=\"deferred-image-description\"><strong>Large diagram</strong><span>This %.2f MiB image is not loaded automatically.</span></span><span class=\"deferred-image-actions\"><button type=\"button\" class=\"deferred-image-action\" onclick=\"loadDeferredImage(this)\"><i class=\"fa fa-image\" aria-hidden=\"true\"></i> Load here</button><a class=\"deferred-image-action\" href=\"%s\" target=\"_blank\" rel=\"noopener\"><i class=\"fa fa-external-link-alt\" aria-hidden=\"true\"></i> Open separately</a></span></div>\n"
      path (float_of_int bytes /. 1024. /. 1024.) path
  | Code s -> Printf.fprintf oc "<pre>%s</pre>\n" s
  | Paragraph s -> Printf.fprintf oc "<div>%s</div>\n" s
  | List l -> List.iter (print_html oc) l

type report_section = { sect_title: string;
                        sect_id: string;
                        sect_content: html_node
                      }

let report = ref ([]: report_section list)

let add_to_report id title content =
  report := !report @ [{ sect_id = id; sect_title = title; sect_content = content }]

let report_section_icon id title =
  if id = "run-summary" then "fa-table"
  else if String.starts_with ~prefix:"Run " title then "fa-play"
  else
    match String.lowercase_ascii id with
    | "source" -> "fa-file-code"
    | "lexer" -> "fa-tags"
    | "test-configuration" -> "fa-sliders-h"
    | "expectation-mismatch" -> "fa-exclamation-triangle"
    | "ast" -> "fa-sitemap"
    | "e" -> "fa-code"
    | "cfg" | "cfg-after-cstprop" | "cfg-after-dae" | "cfg-after-nop" ->
      "fa-project-diagram"
    | "rtl" -> "fa-code-branch"
    | "linear" | "linear-after-dse" -> "fa-list-ol"
    | "regalloc" -> "fa-palette"
    | "ltl" -> "fa-layer-group"
    | "riscv" -> "fa-microchip"
    | _ -> "fa-circle"

let report_dom_id id =
  String.map
    (function
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' as character -> character
      | _ -> '-')
    id

let escape_html_text text =
  let escaped = Buffer.create (String.length text) in
  String.iter (function
      | '&' -> Buffer.add_string escaped "&amp;"
      | '<' -> Buffer.add_string escaped "&lt;"
      | '>' -> Buffer.add_string escaped "&gt;"
      | '"' -> Buffer.add_string escaped "&quot;"
      | '\'' -> Buffer.add_string escaped "&#39;"
      | character -> Buffer.add_char escaped character
    ) text;
  Buffer.contents escaped

let source_span class_name contents =
  Printf.sprintf "<span class=\"%s\">%s</span>"
    class_name (escape_html_text contents)

let source_starts_with source index prefix =
  let prefix_length = String.length prefix in
  index + prefix_length <= String.length source &&
  String.sub source index prefix_length = prefix

let source_find_end predicate source start =
  let rec find index =
    if index < String.length source && predicate source.[index]
    then find (index + 1)
    else index
  in
  find start

let highlight_source source =
  let highlighted = Buffer.create (String.length source * 2) in
  let add_span class_name start finish =
    Buffer.add_string highlighted
      (source_span class_name (String.sub source start (finish - start)))
  in
  let rec find_quoted delimiter index =
    if index >= String.length source then index
    else if source.[index] = '\\' && index + 1 < String.length source then
      find_quoted delimiter (index + 2)
    else if source.[index] = delimiter then index + 1
    else find_quoted delimiter (index + 1)
  in
  let rec find_multiline_comment index =
    if index >= String.length source then index
    else if source_starts_with source index "*/" then index + 2
    else find_multiline_comment (index + 1)
  in
  let is_identifier_start = function
    | 'a'..'z' | 'A'..'Z' | '_' -> true
    | _ -> false
  in
  let is_identifier_char = function
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true
    | _ -> false
  in
  let is_hex_digit = function
    | '0'..'9' | 'a'..'f' | 'A'..'F' -> true
    | _ -> false
  in
  let keywords = ["if"; "else"; "while"; "return"; "print"; "alloc"; "struct"] in
  let types = ["int"; "char"; "void"] in
  let literals = ["true"; "false"; "null"] in
  let class_of_identifier identifier finish =
    if List.mem identifier keywords then "source-keyword"
    else if List.mem identifier types then "source-type"
    else if List.mem identifier literals then "source-literal"
    else
      let next =
        source_find_end (function ' ' | '\t' | '\r' | '\n' -> true | _ -> false)
          source finish
      in
      if next < String.length source && source.[next] = '('
      then "source-function"
      else "source-identifier"
  in
  let two_character_operators = ["->"; "&&"; "||"; "=="; "!="; ">="; "<="] in
  let is_operator = function
    | '+' | '-' | '*' | '/' | '%' | '=' | '<' | '>' | '!'
    | '&' | '|' | '^' | '~' -> true
    | _ -> false
  in
  let is_punctuation = function
    | '{' | '}' | '(' | ')' | '[' | ']' | ';' | ',' | '.' | ':' -> true
    | _ -> false
  in
  let rec highlight index =
    if index < String.length source then begin
      if source_starts_with source index "//" then begin
        let finish =
          source_find_end (fun character -> character <> '\n') source index
        in
        add_span "source-comment" index finish;
        highlight finish
      end else if source_starts_with source index "/*" then begin
        let finish = find_multiline_comment (index + 2) in
        add_span "source-comment" index finish;
        highlight finish
      end else
        match source.[index] with
        | '"' ->
          let finish = find_quoted '"' (index + 1) in
          add_span "source-string" index finish;
          highlight finish
        | '\'' ->
          let finish = find_quoted '\'' (index + 1) in
          add_span "source-character" index finish;
          highlight finish
        | '0' when source_starts_with source index "0x" ->
          let finish = source_find_end is_hex_digit source (index + 2) in
          add_span "source-number" index finish;
          highlight finish
        | '0'..'9' ->
          let finish =
            source_find_end (function '0'..'9' -> true | _ -> false) source index
          in
          add_span "source-number" index finish;
          highlight finish
        | character when is_identifier_start character ->
          let finish = source_find_end is_identifier_char source index in
          let identifier = String.sub source index (finish - index) in
          add_span (class_of_identifier identifier finish) index finish;
          highlight finish
        | _ ->
          let two_characters =
            List.find_opt (source_starts_with source index)
              two_character_operators
          in
          begin match two_characters with
          | Some operator ->
            Buffer.add_string highlighted (source_span "source-operator" operator);
            highlight (index + 2)
          | None when is_operator source.[index] ->
            add_span "source-operator" index (index + 1);
            highlight (index + 1)
          | None when is_punctuation source.[index] ->
            add_span "source-punctuation" index (index + 1);
            highlight (index + 1)
          | None ->
            Buffer.add_string highlighted
              (escape_html_text (String.make 1 source.[index]));
            highlight (index + 1)
          end
    end
  in
  highlight 0;
  Buffer.contents highlighted

let make_report ?output filename report () =
  let html = open_out (Option.value ~default:(filename ^ ".html") output) in
  let display_filename = escape_html_text (Filename.basename filename) in
  Printf.fprintf html "\
<html>\n\
    <head>\n\
    <title>%s - Ecomp report</title>\n\
            <link rel=\"stylesheet\" href=\"https://www.w3schools.com/w3css/4/w3.css\">\n\
    <script src=\"https://kit.fontawesome.com/1f5d81749b.js\" crossorigin=\"anonymous\"></script>\n\
    <style type=\"text/css\">\n%s\n</style>\n\
    <script type=\"text/javascript\">\n%s\n</script>\n\
    </head>\n\
    <body>\n\
" display_filename Report_assets.css Report_assets.javascript;
  let t = Unix.time () in
  let tm = Unix.localtime t in
  let open Unix in
  let display_date =
    Printf.sprintf "%02d/%02d/%04d · %02d:%02d"
    tm.tm_mday
    (tm.tm_mon + 1)
    (tm.tm_year + 1900)
    tm.tm_hour
    tm.tm_min
  in
  let datetime =
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d"
      (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min
  in
  Printf.fprintf html
    "<aside id=\"report-sidebar\" aria-label=\"Report navigation\">\n\
     <header class=\"sidebar-header\">\
       <span class=\"sidebar-brand-icon\" aria-hidden=\"true\"><i class=\"fa fa-code\"></i></span>\
       <span class=\"sidebar-heading\">\
         <span class=\"sidebar-eyebrow\">Ecomp report</span>\
         <span class=\"sidebar-filename\" title=\"%s\">%s</span>\
       </span>\
     </header>\n\
     <div class=\"sidebar-meta\"><i class=\"fa fa-clock\" aria-hidden=\"true\"></i><time datetime=\"%s\">%s</time></div>\n\
     <a class=\"sidebar-results\" href=\"../results.html\"><i class=\"fa fa-arrow-left\" aria-hidden=\"true\"></i><span>Results</span></a>\n\
     <nav class=\"sidebar-nav\" aria-label=\"Compilation stages\">\n\
       <div class=\"sidebar-nav-title\">Compilation stages</div>\n\
       <ul class=\"sidebar-nav-list\">\n"
    display_filename display_filename datetime display_date;
  List.iter
    (fun { sect_id; sect_title; _ } ->
       let dom_id = report_dom_id sect_id in
       Printf.fprintf html
         "<li class=\"sidebar-nav-item\"><a class=\"sidebar-link\" href=\"#%s\" data-section-id=\"%s\"><i class=\"fa %s\" aria-hidden=\"true\"></i><span>%s</span></a></li>\n"
         dom_id dom_id (report_section_icon sect_id sect_title) sect_title
    )
    !report;
  Printf.fprintf html "</ul></nav></aside>\
                       <div class=\"sidebar-backdrop\" aria-hidden=\"true\" onclick=\"setSidebarHidden(true)\"></div>\
                       <button class=\"report-control sidebar-toggle\" id=\"toggle-sidebar\" title=\"Hide sidebar\" aria-label=\"Hide sidebar\" aria-controls=\"report-sidebar\" aria-expanded=\"true\" onclick=\"toggleSidebar()\"><i class=\"fa fa-chevron-left\" aria-hidden=\"true\"></i></button>\
                       <div id=\"report-main\" \
                       class=\"w3-container\" \
                       ><a class=\"anchor\" id=\"top\"></a>\
                       <button class=\"report-control section-toggle\" id=\"toggle-all-sections\" title=\"Fold all sections\" aria-label=\"Fold all sections\" onclick=\"toggleAllSections()\"><i class=\"fa fa-chevron-up\" aria-hidden=\"true\"></i></button>";
  List.iter
    (fun { sect_id; sect_title; sect_content } ->
       let dom_id = report_dom_id sect_id in
       let ltl_group_toggle =
         if sect_id = "ltl" then
           "<button type=\"button\" class=\"report-control\" id=\"toggle-ltl-groups\" title=\"Hide LTL groups\" aria-label=\"Hide LTL groups\" aria-controls=\"report-content-ltl\" aria-pressed=\"false\" onclick=\"toggleLtlGroups(this)\"><i class=\"fa fa-bars\" aria-hidden=\"true\"></i></button>"
         else
           ""
       in
       let rtl_block_toggle =
         if List.mem sect_id ["rtl"; "linear"; "linear-after-dse"] then
           Format.sprintf
             "<button type=\"button\" class=\"report-control\" id=\"toggle-%s-blocks\" title=\"Hide node blocks\" aria-label=\"Hide node blocks\" aria-controls=\"report-content-%s\" aria-pressed=\"false\" onclick=\"toggleRtlBlocks(this, '%s')\"><i class=\"fa fa-bars\" aria-hidden=\"true\"></i></button>"
             sect_id sect_id sect_id
         else
           ""
       in
       let rtl_liveness_toggle =
         if List.mem sect_id ["linear"; "linear-after-dse"] then
           Format.sprintf
             "<button type=\"button\" class=\"report-control\" id=\"toggle-%s-liveness\" title=\"Hide liveness information\" aria-label=\"Hide liveness information\" aria-controls=\"report-content-%s\" aria-pressed=\"false\" onclick=\"toggleRtlLiveness(this, '%s')\"><i class=\"fa fa-eye-slash\" aria-hidden=\"true\"></i></button>"
             sect_id sect_id sect_id
         else
           ""
       in
       Printf.fprintf html "<fieldset class=\"report-section\" id=\"report-section-%s\" data-section-id=\"%s\">\n\
                            <a class=\"anchor\" id=\"%s\"></a>\n\
                            <h3><a class=\"report-control top-link\" href=\"#top\" title=\"Back to top\" aria-label=\"Back to top\"><i class=\"fa fa-arrow-up\" aria-hidden=\"true\"></i></a>\
                            <button class=\"report-control section-toggle report-section-toggle\" id=\"toggle-section-%s\" title=\"Fold section\" aria-label=\"Fold section\" aria-expanded=\"true\" onclick=\"toggleSection('%s')\"><i class=\"fa fa-chevron-up\" aria-hidden=\"true\"></i></button>%s%s%s %s</h3>\n\
                            <div class=\"report-section-content\" id=\"report-content-%s\">%a</div>\n\
                            </fieldset>\n" dom_id dom_id dom_id dom_id dom_id ltl_group_toggle rtl_block_toggle rtl_liveness_toggle sect_title dom_id print_html sect_content
    )
    !report;
  Printf.fprintf html "\
</div>\n\
    </body>\n\
</html>";
  close_out html;
  ()

type run_result = {
  step: string;
  retval: int option;
  output: string;
  error: string option;
  time: float;
}

type expected_run = {
  expected_retval: int option;
  expected_output: string;
  expected_error: string option;
}

type compile_result = {
  step: string;
  error: string option;
  data: Yojson.t
}

type result = RunRes of run_result
            | CompRes of compile_result

let empty_html = "<span class=\"run-empty\">empty</span>"

let present_or_empty text =
  if String.trim text = "" then empty_html else escape_html_text text

let run_metric icon label value =
  Printf.sprintf
    "<span class=\"run-metric\"><i class=\"fa fa-%s\" aria-hidden=\"true\"></i><span class=\"run-metric-label\">%s</span><span class=\"run-metric-value\">%s</span></span>"
    icon label value

let run_panel ?(error=false) icon label contents =
  Printf.sprintf
    "<div class=\"run-panel%s\"><div class=\"run-panel-header\"><i class=\"fa fa-%s\" aria-hidden=\"true\"></i><span>%s</span></div><pre class=\"run-panel-content\">%s</pre></div>"
    (if error then " run-panel-error" else "") icon label contents


let results : result list ref = ref []
let imported_run_number = ref 0
let timed_compile_step : (string * float) option ref = ref None

let reset () =
  report := [];
  results := [];
  imported_run_number := 0;
  timed_compile_step := None

let time_compile_step step f =
  timed_compile_step := Some (step, Unix.gettimeofday ());
  Fun.protect ~finally:(fun () -> timed_compile_step := None) f

let record_compile_result ?error:(error=None) ?data:(data=[]) step =
  let data = match !timed_compile_step with
    | Some (timed_step, started) when step = timed_step ->
      `Assoc [("seconds", `Float (Unix.gettimeofday () -. started))] :: data
    | _ -> data
  in
  let data = if not !Options.nostats then `List data else `Null in
  results := !results @ [CompRes { step; error; data}]

let compile_errors () =
  List.filter_map (function
      | CompRes { step; error = Some message; _ } ->
        Some (step, message)
      | _ -> None) !results

let add_compile_errors () =
  let errors = compile_errors () |> List.map (fun (step, message) ->
      Printf.sprintf "%s:\n%s" step message) in
  if errors <> [] then
    add_to_report "compiler-errors" "Compiler errors"
      (Code (String.concat "\n\n" errors))

let has_compile_errors () =
  compile_errors () <> []

let record_imported_run ~step ~retval ~output ~error ~time =
  results := !results @ [RunRes { step; retval; output; error; time }]

let add_imported_run ~step ~retval ~output ~error ~time =
  record_imported_run ~step ~retval ~output ~error ~time;
  incr imported_run_number;
  let return_value = match retval with
    | Some value -> string_of_int value
    | None -> "<span class=\"run-empty\">none</span>"
  in
  let output_panel = run_panel "terminal" "Output" (present_or_empty output) in
  let error_panel = match error with
    | None -> ""
    | Some message -> run_panel ~error:true "exclamation-triangle" "Error"
                        (present_or_empty message)
  in
  let summary =
    run_metric "reply" "Return" return_value ^
    run_metric "clock" "Time" (Printf.sprintf "%.3f s" time)
  in
  add_to_report
    (Printf.sprintf "run-%d-%s" !imported_run_number (report_dom_id step))
    ("Run " ^ step)
    (Paragraph
       (Printf.sprintf
          "<div class=\"run-result%s\"><div class=\"run-summary\">%s</div><div class=\"run-panels\">%s%s</div></div>"
          (if error = None then "" else " run-result-error") summary output_panel error_panel))

let add_imported_run_group ~step runs =
  let item_number = ref 0 in
  let panel (params, heap, retval, output, error, time, expected) =
    incr item_number;
    record_imported_run ~step ~retval ~output ~error ~time;
    let parameters = Printf.sprintf "[%s]"
        (String.concat ", " (List.map string_of_int params)) in
    let memory = Printf.sprintf "<span title=\"%d bytes\">%.2f KiB</span>"
        heap (float_of_int heap /. 1024.) in
    let return_value = match retval with
      | Some value -> string_of_int value
      | None -> "<span class=\"run-empty\">none</span>" in
    let differs = match expected with
      | None -> false
      | Some expected ->
        (retval, output, error) <> (expected.expected_retval,
                                    expected.expected_output,
                                    expected.expected_error) in
    let expected_summary, expected_panels = match expected with
      | Some expected when differs ->
        let expected_return = match expected.expected_retval with
          | Some value -> string_of_int value
          | None -> "<span class=\"run-empty\">none</span>" in
        let expected_error = match expected.expected_error with
          | None -> ""
          | Some message -> run_panel ~error:true "exclamation-triangle"
                              "Expected error" (present_or_empty message) in
        (run_metric "bullseye" "Expected return" expected_return,
         run_panel "bullseye" "Expected output"
           (present_or_empty expected.expected_output) ^ expected_error)
      | _ -> ("", "") in
    let summary =
      run_metric "list-ol" "Parameters" parameters ^
      run_metric "memory" "Memory" memory ^
      run_metric "reply" "Return" return_value ^
      run_metric "clock" "Time" (Printf.sprintf "%.3f s" time) ^
      expected_summary in
    let error_panel = match error with
      | None -> ""
      | Some message -> run_panel ~error:true "exclamation-triangle" "Error"
                          (present_or_empty message) in
    Printf.sprintf
      "<div id=\"run-%d-%s\" class=\"run-result%s%s\"><div class=\"run-summary\">%s</div><div class=\"run-panels\">%s%s%s</div></div>"
      !item_number (report_dom_id step)
      (if error = None then "" else " run-result-error")
      (if differs then " run-result-mismatch" else "") summary
      (run_panel "terminal" "Output" (present_or_empty output)) error_panel
      expected_panels
  in
  if runs <> [] then
    add_to_report ("runs-" ^ report_dom_id step) ("Runs for " ^ step)
      (Paragraph (String.concat "" (List.map panel runs)))

let kill pid sign =
  try Unix.kill pid sign with
  | Unix.Unix_error (e,f,p) ->
    begin match e with
      | ESRCH -> ()
      | _ -> Printf.printf "%s\n" ((Unix.error_message e)^"|"^f^"|"^p)
    end
  | e -> raise e

let run_exn_to_error f x =
  try f x with
  | e -> Error (Printexc.to_string e)

let timeout (f: 'a -> 'b res) (arg: 'a) (time: float) : ('b * string) res =
  (* Do not let forked timeout workers inherit buffered diagnostics.  Otherwise
     every worker flushes messages such as the lexer and assembler banners. *)
  flush_all ();
  let pipe_r,pipe_w = Unix.pipe () in
  (match Unix.fork () with
   | 0 ->
     let r =
       run_exn_to_error f arg >>= fun v ->
       OK (v, Format.flush_str_formatter ()) in
     let oc = Unix.out_channel_of_descr pipe_w in
     Marshal.to_channel oc r [];
     close_out oc;
     exit 0
   | pid0 ->
     let watchdog =
       match Unix.fork () with
       | 0 -> Unix.sleepf time;
         kill pid0 Sys.sigkill;
         let oc = Unix.out_channel_of_descr pipe_w in
         Marshal.to_channel oc (Error (Printf.sprintf "Timeout after %f seconds." time)) [];
         close_out oc;
         exit 0
       | pid -> pid
     in
     let ic = Unix.in_channel_of_descr pipe_r in
     let result = Marshal.from_channel ic in
     close_in_noerr ic;
     (* A successful worker leaves its watchdog sleeping.  Reap it now:
        otherwise each runner process remains alive until the timeout expires. *)
     kill watchdog Sys.sigkill;
     (try ignore (Unix.waitpid [] watchdog) with Unix.Unix_error _ -> ());
     (try ignore (Unix.waitpid [] pid0) with Unix.Unix_error _ -> ());
     result)

let run step flag eval p =
  if flag then begin
    let starttime = Unix.gettimeofday () in
    let res = timeout
        (fun (p, params) -> eval Format.str_formatter p !heapsize params)
        (p, !params)
        !Options.timeout in
    let timerun = Unix.gettimeofday () -. starttime in
    let rres = { step ; retval = None; output=""; error = None; time = timerun} in
    let rres =
    begin match res with
      | OK (v, output) ->  { rres with retval = v; output }
      | Error msg -> { rres with error = Some msg }
    end in
    results := !results @ [RunRes rres];
    let parameters =
      match !params with
      | [] -> empty_html
      | params ->
        Printf.sprintf "[%s]"
          (String.concat ", " (List.map string_of_int params))
    in
    let memory =
      Printf.sprintf
        "<span title=\"%d bytes\">%.2f KiB</span>"
        !heapsize (float_of_int !heapsize /. 1024.)
    in
    let return_value =
      match rres.retval with
      | Some value -> string_of_int value
      | None -> "<span class=\"run-empty\">none</span>"
    in
    let summary =
      run_metric "list-ol" "Parameters" parameters ^
      run_metric "memory" "Memory" memory ^
      run_metric "reply" "Return" return_value ^
      run_metric "clock" "Time" (Printf.sprintf "%.3f s" timerun)
    in
    let output_panel =
      run_panel "terminal" "Output" (present_or_empty rres.output)
    in
    let error_panel =
      match rres.error with
      | None -> ""
      | Some message ->
        run_panel ~error:true "exclamation-triangle" "Error"
          (present_or_empty message)
    in
    add_to_report step ("Run " ^ step)
      (Paragraph
         (Printf.sprintf
            "<div class=\"run-result%s\"><div class=\"run-summary\">%s</div><div class=\"run-panels\">%s%s</div></div>"
            (if rres.error = None then "" else " run-result-error")
            summary output_panel error_panel))
  end


let json_output_string () =
  let jstring_of_ostring o =
    match o with
    | None -> `Null
    | Some s -> `String s
  in
  let j = `List (List.map (function
      | RunRes { step; retval; output; error; time } ->
        `Assoc [("runstep",`String step);
                ("retval", match retval with Some r -> `Int r | None -> `Null);
                ("output", `String output);
                ("error", jstring_of_ostring error);
                ("params", `List (List.map (fun value -> `Int value) !Options.params));
                ("heap", `Int !Options.heapsize);
                ("time", `Float time)
               ]
      | CompRes { step; error; data } ->
        `Assoc [("compstep",`String step);
                ("error", jstring_of_ostring error);
                ("data", data)
               ]
    ) !results) in
  (Yojson.pretty_to_string j)
