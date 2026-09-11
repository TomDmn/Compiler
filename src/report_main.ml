(* Report executable. Compilation and execution deliberately live in [main.ml]
   and [runner.ml], respectively. *)
open Utils
open Report
open Cli_completion

let e_file : string option ref = ref None
let ast_file : string option ref = ref None
let cfg_file : string option ref = ref None
let cfg_cp_file : string option ref = ref None
let cfg_dae_file : string option ref = ref None
let cfg_ne_file : string option ref = ref None
let rtl_file : string option ref = ref None
let linear_file : string option ref = ref None
let linear_dse_file : string option ref = ref None
let ltl_file : string option ref = ref None
let report_file : string option ref = ref None
let compiler_json : string option ref = ref None
let source_file : string option ref = ref None
let lexer_file : string option ref = ref None
let rig_file : string option ref = ref None
let expectation_file : string option ref = ref None
let resolved_report : string option ref = ref None
let report_source : string option ref = ref None
type imported_run = {
  step : string; params : int list; heap : int; retval : int option;
  output : string; error : string option; time : float;
  expected : expected_run option
}
let imported_runs : imported_run list ref = ref []
let run_jsons : string list ref = ref []
let expected_runs : expected_run list ref = ref []
let expected_rejection : (string * string option) option ref = ref None
let expected_lexer : string list option ref = ref None
let effective_compiler_options : string list ref = ref []
let teaching_compile_failure : string option ref = ref None
let teaching_run_through : string option ref = ref None
let teaching_fail_at : string option ref = ref None

let cli_options = [
  Cli_completion.option ~argument:Output_file "-report" (Arg.String (fun s -> report_file := Some s)) "Output HTML report";
  Cli_completion.option ~argument:(Input_file [".e"]) "-source" (Arg.String (fun s -> source_file := Some s)) "Source file displayed in the report.";
  Cli_completion.option ~argument:(Input_file []) "-lexer" (Arg.String (fun s -> lexer_file := Some s)) "Token listing displayed in the report.";
  Cli_completion.option ~argument:(Input_file [".json"]) "-rig-load" (Arg.String (fun s -> rig_file := Some s)) "Load register interference data saved as JSON.";
  Cli_completion.option ~argument:(Input_file [".json"]) "-expect" (Arg.String (fun s -> expectation_file := Some s)) "Test expectation sidecar used to compare runs.";
  Cli_completion.option ~argument:(Input_file [".json"]) "-compile-json" (Arg.String (fun s -> compiler_json := Some s)) "Compilation manifest produced by ecomp.";
  Cli_completion.option ~argument:(Input_file [".json"]) "-run-json" (Arg.String (fun s -> run_jsons := s :: !run_jsons)) "Run result produced by ecomp-run (repeatable).";
]

let speclist = Cli_completion.speclist cli_options

let provenance : Artifact.provenance option ref = ref None

let check_provenance p =
  match !provenance with
  | None ->
    provenance := Some p;
    Archi.archi := (if p.architecture = "rv32" then Archi.A32 else Archi.A64);
    Archi.target := (if p.target = "xv6" then Archi.Xv6 else Archi.Linux);
    OK ()
  | Some expected when Artifact.compatible expected p -> OK ()
  | Some _ -> Error "Loaded artifacts do not have matching source/build provenance"

let load kind file =
  Artifact.load ~kind file >>= fun (p, value, seconds) ->
  record_compile_result ~data:[`Assoc [("seconds", `Float seconds)]] ("Unmarshal " ^ kind);
  check_provenance p >>= fun () -> OK value

let load_optional kind = function
  | None -> OK None
  | Some file -> load kind file >>= fun value -> OK (Some value)

let deferred_image_threshold = 1024 * 1024

let report_image filename =
  let bytes = (Unix.stat filename).st_size in
  let path = Filename.basename filename in
  if bytes > deferred_image_threshold then DeferredImg (path, bytes) else Img path

let default_report () =
  match !report_file, !provenance with
  | Some file, _ -> file
  | None, Some p -> Filename.concat (Filename.dirname p.source_path)
                      (Filename.basename p.source_path ^ ".report.html")
  | None, None -> "ecomp-report.html"

let add_cfg_graph output id title program =
  let dot_file = Filename.temp_file "ecomp-cfg" ".dot" in
  let svg_file = output ^ "." ^ id ^ ".svg" in
  Fun.protect ~finally:(fun () -> try Sys.remove dot_file with Sys_error _ -> ())
    (fun () ->
       let channel = open_out dot_file in
       let formatter = Format.formatter_of_out_channel channel in
       Format.fprintf formatter "%a" Cfg_pretty_print.dump_cfg_prog program;
       Format.pp_print_flush formatter ();
       close_out channel;
       let command = Printf.sprintf "dot -Tsvg %s -o %s"
           (Filename.quote dot_file) (Filename.quote svg_file) in
       if Sys.command command = 0 then
         add_to_report id title (report_image svg_file)
       else
         add_to_report id title
           (Paragraph "<span class=\"run-empty\">Could not render CFG graph.</span>"))

let add_ast_graph output ast =
  let dot_file = Filename.temp_file "ecomp-ast" ".dot" in
  let svg_file = output ^ ".ast.svg" in
  Fun.protect ~finally:(fun () -> try Sys.remove dot_file with Sys_error _ -> ())
    (fun () ->
       let channel = open_out dot_file in
       let formatter = Format.formatter_of_out_channel channel in
       Format.fprintf formatter "%a" Ast.draw_ast_tree ast;
       Format.pp_print_flush formatter ();
       close_out channel;
       let command = Printf.sprintf "dot -Tsvg %s -o %s"
           (Filename.quote dot_file) (Filename.quote svg_file) in
       if Sys.command command = 0 then
         add_to_report "ast" "AST" (report_image svg_file)
       else
         add_to_report "ast" "AST"
           (Paragraph "<span class=\"run-empty\">Could not render AST graph.</span>"))

let add_rig_graph output rig =
  let render_function (name, graph) =
    let dot_file = Filename.temp_file "ecomp-rig" ".dot" in
    let svg_file =
      output ^ ".regalloc." ^ report_dom_id name ^ ".svg"
    in
    Fun.protect ~finally:(fun () -> try Sys.remove dot_file with Sys_error _ -> ())
      (fun () ->
         let channel = open_out dot_file in
         let formatter = Format.formatter_of_out_channel channel in
         Rig_json.dump_dot formatter graph;
         Format.pp_print_flush formatter (); close_out channel;
         let command = Printf.sprintf "sfdp -Tsvg %s -o %s"
             (Filename.quote dot_file) (Filename.quote svg_file) in
         let heading =
           Paragraph (Printf.sprintf
                        "<h4 class=\"regalloc-function-title\">Function <code>%s</code></h4>"
                        (escape_html_text name))
         in
         if Sys.command command = 0 then
           [heading; report_image svg_file]
         else
           [heading; Paragraph
               "<span class=\"run-empty\">Could not render interference graph.</span>"])
  in
  let graphs = Rig_json.load rig |> Rig_json.function_graphs in
  match List.concat_map render_function graphs with
  | [] -> ()
  | contents ->
    add_to_report "regalloc" "Register allocation" (List contents)

let add_code_section_safely id title render =
  try add_to_report id title (Code (render ())) with error ->
    add_to_report id title
      (Paragraph
         (Printf.sprintf
            "<span class=\"report-render-warning\"><i class=\"fa fa-exclamation-triangle\" aria-hidden=\"true\"></i> This representation cannot be rendered at this teaching stage: %s</span>"
            (escape_html_text (Printexc.to_string error))))

let option_string = function `String value -> Some value | `Null -> None | _ -> None

let import_compiler_results filename =
  try
    match Yojson.Safe.from_file filename with
    | `List entries -> List.iter (function
        | `Assoc fields ->
          begin match List.assoc_opt "compstep" fields with
          | Some (`String step) ->
            let error = Option.bind (List.assoc_opt "error" fields) option_string in
            let data = [] in
            begin match error with
            | None -> record_compile_result ~data step
            | Some message -> record_compile_result ~error:(Some message) ~data step
            end
          | _ -> ()
          end
        | _ -> ()) entries
    | _ -> ()
  with Sys_error message | Yojson.Json_error message ->
    record_compile_result ~error:(Some message) "Compiler result import"

let artifact_path kind fields =
  match List.assoc_opt "data" fields with
  | Some (`List data) ->
    List.find_map (function
        | `Assoc entry ->
          begin match List.assoc_opt "kind" entry, List.assoc_opt "path" entry with
          | Some (`String found_kind), Some (`String path) when found_kind = kind -> Some path
          | _ -> None
          end
        | _ -> None) data
  | _ -> None

let load_manifest filename =
  try match Yojson.Safe.from_file filename with
    | `List entries ->
      List.iter (function
          | `Assoc fields ->
            begin match List.assoc_opt "compstep" fields with
            | Some (`String step) when String.starts_with ~prefix:"Marshal " step ->
              let kind = String.sub step 8 (String.length step - 8) in
              begin match artifact_path kind fields with
              | None -> ()
              | Some path ->
                begin match kind with
                | "ast" -> ast_file := Some path | "e" -> e_file := Some path | "cfg" -> cfg_file := Some path
                | "cfg-after-cp" -> cfg_cp_file := Some path | "cfg-after-dae" -> cfg_dae_file := Some path
                | "cfg-after-ne" -> cfg_ne_file := Some path | "rtl" -> rtl_file := Some path
                | "linear" -> linear_file := Some path | "linear-after-dse" -> linear_dse_file := Some path
                | "ltl" -> ltl_file := Some path | _ -> ()
                end
              end
            | Some (`String "Save rig") ->
              Option.iter (fun path -> rig_file := Some path) (artifact_path "rig" fields)
            | _ -> ()
            end
          | _ -> ()) entries
    | _ -> ()
  with Sys_error _ | Yojson.Json_error _ -> ()

let import_expectations filename =
  let expected fields = {
    expected_retval = (match List.assoc_opt "retval" fields with
      | Some (`Int value) -> Some value | _ -> None);
    expected_output = (match List.assoc_opt "output" fields with
      | Some (`String value) -> value | _ -> "");
    expected_error = Option.bind (List.assoc_opt "error" fields) option_string;
  } in
  try match Yojson.Safe.from_file filename with
    | `Assoc fields ->
      effective_compiler_options :=
        (match List.assoc_opt "effective_compiler_options" fields with
         | Some (`List options) ->
           List.filter_map (function `String option -> Some option | _ -> None) options
         | _ -> []);
      expected_lexer :=
        (match List.assoc_opt "lexer" fields with
         | Some (`List tokens) ->
           Some (List.filter_map
                   (function `String token -> Some token | _ -> None) tokens)
         | _ -> None);
      begin match List.assoc_opt "teaching_expectation" fields with
      | Some (`Assoc expectation) ->
        teaching_compile_failure :=
          Option.bind (List.assoc_opt "compile_failure" expectation) option_string;
        teaching_run_through :=
          Option.bind (List.assoc_opt "run_through" expectation) option_string;
        teaching_fail_at :=
          Option.bind (List.assoc_opt "fail_at" expectation) option_string
      | _ -> ()
      end;
      begin match List.assoc_opt "expect" fields with
      | Some (`Assoc expectation) ->
        begin match List.assoc_opt "kind" expectation,
                    List.assoc_opt "stage" expectation with
        | Some (`String "reject"), Some (`String stage) ->
          let error_contains =
            Option.bind (List.assoc_opt "error_contains" expectation) option_string
          in
          expected_rejection := Some (stage, error_contains)
        | _ -> ()
        end;
        begin match List.assoc_opt "cases" expectation with
        | Some (`List cases) ->
          expected_runs := List.filter_map (function
              | `Assoc case ->
                begin match List.assoc_opt "result" case with
                | Some (`Assoc result) -> Some (expected result)
                | _ -> None
                end
              | _ -> None) cases
        | _ -> ()
        end
      | _ -> ()
      end
    | _ -> ()
  with Sys_error _ | Yojson.Json_error _ -> ()

let add_test_configuration () =
  Option.iter (fun _ ->
      let options = match !effective_compiler_options with
        | [] -> "<span class=\"compiler-options-empty\">none (compiler defaults)</span>"
        | options ->
          String.concat ""
            (List.map (fun option ->
                 Printf.sprintf "<code>%s</code>" (escape_html_text option)) options)
      in
      add_to_report "test-configuration" "Test configuration"
        (Paragraph
           (Printf.sprintf
              "<div class=\"compiler-options-card\"><span class=\"compiler-options-label\"><i class=\"fa fa-sliders-h\" aria-hidden=\"true\"></i>Effective compiler options</span><div class=\"compiler-options-list\">%s</div></div>"
              options))
    ) !expectation_file

let add_teaching_expectation () =
  let items =
    List.filter_map Fun.id [
      Option.map
        (fun stage -> Printf.sprintf
            "Correct execution is required through <strong>%s</strong>."
            (escape_html_text stage))
        !teaching_run_through;
      Option.map
        (fun stage -> Printf.sprintf
            "Execution is expected to fail at <strong>%s</strong>."
            (escape_html_text stage))
        !teaching_fail_at;
      Option.map
        (fun stage -> Printf.sprintf
            "Compilation is expected to stop at <strong>%s</strong>."
            (escape_html_text stage))
        !teaching_compile_failure;
    ]
  in
  match items with
  | [] -> ()
  | _ ->
    add_to_report "teaching-expectation" "Teaching-stage expectation"
      (Paragraph
         (Printf.sprintf
            "<div class=\"teaching-expectation-card\"><i class=\"fa fa-graduation-cap\" aria-hidden=\"true\"></i><div><strong>Expected outcome for this compiler stage</strong><p>%s Errors at the stated boundary are an expected, successful test outcome.</p></div></div>"
            (String.concat " " items)))

let string_contains ~needle haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec search index =
    needle_length = 0 ||
    (index + needle_length <= haystack_length &&
     (String.sub haystack index needle_length = needle || search (index + 1)))
  in
  search 0

let add_expectation_mismatch () =
  let compiler_failure =
    List.find_map (function
        | CompRes { step; error = Some message; _ } -> Some (step, message)
        | _ -> None) !results
  in
  let expected_stage_matches expected_stage actual_stage =
    string_contains
      ~needle:(String.lowercase_ascii expected_stage)
      (String.lowercase_ascii actual_stage)
  in
  let message = match !expected_rejection, compiler_failure with
    | None, _ -> None
    | Some (stage, _), None ->
      Some ("Unexpected compiler acceptance",
            Printf.sprintf
              "This test expects rejection during %s, but compilation succeeded and produced intermediate programs. The runs below are diagnostic observations, not expected successes."
              stage)
    | Some (stage, _error_contains), Some (actual_stage, _actual_error)
      when not (expected_stage_matches stage actual_stage) ->
      Some ("Unexpected rejection stage",
            Printf.sprintf
              "This test expects rejection during %s, but the compiler rejected it during %s."
              stage actual_stage)
    | Some (stage, Some expected_text), Some (actual_stage, actual_error)
      when not (string_contains ~needle:expected_text actual_error) ->
      Some ("Unexpected compiler error",
            Printf.sprintf
              "The compiler rejected this test during %s as expected, but its error did not contain %S. Actual error: %s"
              stage expected_text actual_error)
    | Some _, Some _ -> None
  in
  Option.iter (fun (heading, detail) ->
      add_to_report "expectation-mismatch" "Expectation mismatch"
        (Paragraph (Printf.sprintf
            "<div class=\"expectation-mismatch\"><i class=\"fa fa-exclamation-triangle\" aria-hidden=\"true\"></i><div><strong>%s</strong><span>%s</span></div></div>"
            (escape_html_text heading) (escape_html_text detail))))
    message

let import_run_results expected filename =
  try match Yojson.Safe.from_file filename with
    | `List entries -> List.iter (function
        | `Assoc fields ->
          begin match List.assoc_opt "runstep" fields with
          | Some (`String step) ->
            let retval = match List.assoc_opt "retval" fields with Some (`Int value) -> Some value | _ -> None in
            let output = match List.assoc_opt "output" fields with Some (`String value) -> value | _ -> "" in
            let error = Option.bind (List.assoc_opt "error" fields) option_string in
            let params = match List.assoc_opt "params" fields with
              | Some (`List values) -> List.filter_map (function `Int value -> Some value | _ -> None) values
              | _ -> [] in
            let heap = match List.assoc_opt "heap" fields with Some (`Int value) -> value | _ -> 10240 in
            let time = match List.assoc_opt "time" fields with Some (`Float value) -> value | _ -> 0. in
            imported_runs := !imported_runs @ [{ step; params; heap; retval; output; error; time; expected }]
          | _ -> ()
          end
        | _ -> ()) entries
    | _ -> ()
  with Sys_error _ | Yojson.Json_error _ -> ()

let add_runs step =
  let runs = List.filter (fun run -> run.step = step) !imported_runs in
  add_imported_run_group ~step
    (List.map (fun run -> (run.params, run.heap, run.retval, run.output, run.error,
                            run.time, run.expected)) runs)

let run_steps = [
  "Elang";
  "CFG";
  "CFG after constant propagation";
  "CFG after dead assignment elimination";
  "CFG after nop elimination";
  "RTL";
  "Linear";
  "Linear after DSE";
  "LTL";
  "Risc-V";
]

let same_run_configuration left right =
  left.params = right.params && left.heap = right.heap

let run_matches_expectation run =
  match run.expected with
  | None -> None
  | Some expected ->
    Some ((run.retval, run.output, run.error) =
          (expected.expected_retval, expected.expected_output,
           expected.expected_error))

let run_is_expected_failure run =
  match !teaching_fail_at, run_matches_expectation run with
  | Some stage, Some false when stage = run.step -> true
  | _ -> false

let text_value value =
  if String.trim value = "" then "empty" else Printf.sprintf "%S" value

let optional_int_value = function
  | None -> "none"
  | Some value -> string_of_int value

let optional_error_value = function
  | None -> "none"
  | Some value -> text_value value

let run_comparison_title run =
  match run.expected with
  | None -> "No expected result was provided."
  | Some expected ->
    Printf.sprintf
      "Actual: return %s; output %s; error %s\nExpected: return %s; output %s; error %s"
      (optional_int_value run.retval) (text_value run.output)
      (optional_error_value run.error)
      (optional_int_value expected.expected_retval)
      (text_value expected.expected_output)
      (optional_error_value expected.expected_error)

let add_run_summary () =
  let add_unique same items item =
    if List.exists (same item) items then items else items @ [item]
  in
  let configurations =
    List.fold_left (add_unique same_run_configuration) [] !imported_runs
  in
  let encountered_steps =
    List.fold_left (add_unique String.equal) []
      (List.map (fun run -> run.step) !imported_runs)
  in
  let executed_steps =
    List.filter (fun step -> List.mem step encountered_steps) run_steps @
    List.filter (fun step -> not (List.mem step run_steps)) encountered_steps
  in
  let status_cell item_number configuration step =
    match List.find_opt
            (fun run -> run.step = step &&
                        same_run_configuration run configuration)
            !imported_runs with
    | None ->
      "<td class=\"run-matrix-cell\"><span class=\"run-matrix-status is-missing\" title=\"This IR was not executed for this configuration.\"><i class=\"fa fa-minus\" aria-hidden=\"true\"></i><span class=\"visually-hidden\">Not executed</span></span></td>"
    | Some run ->
      let class_name, icon, label =
        if run_is_expected_failure run then
          "is-expected-failure", "fa-exclamation", "Expected failure"
        else match run_matches_expectation run with
        | Some true -> "is-pass", "fa-check", "Matches expectations"
        | Some false -> "is-fail", "fa-times", "Does not match expectations"
        | None -> "is-missing", "fa-minus", "No expectation"
      in
      Printf.sprintf
        "<td class=\"run-matrix-cell\"><a class=\"run-matrix-status %s\" href=\"#run-%d-%s\" title=\"%s\"><i class=\"fa %s\" aria-hidden=\"true\"></i><span class=\"visually-hidden\">%s</span></a></td>"
        class_name item_number (report_dom_id step)
        (escape_html_text (run_comparison_title run)) icon label
  in
  let row item_number configuration =
    let parameters = Printf.sprintf "[%s]"
        (String.concat ", " (List.map string_of_int configuration.params)) in
    let memory = Printf.sprintf
        "<span class=\"run-matrix-memory\" title=\"%d bytes\"><i class=\"fa fa-memory\" aria-hidden=\"true\"></i> %.2f KiB</span>"
        configuration.heap (float_of_int configuration.heap /. 1024.) in
    Printf.sprintf
      "<tr><th scope=\"row\"><span class=\"run-matrix-parameters\">%s</span>%s</th>%s</tr>"
      (escape_html_text parameters) memory
      (String.concat ""
         (List.map (status_cell item_number configuration) executed_steps))
  in
  if configurations <> [] && executed_steps <> [] then
    let headings =
      String.concat ""
        (List.map
           (fun step -> Printf.sprintf "<th scope=\"col\">%s</th>"
               (escape_html_text step))
           executed_steps)
    in
    let rows =
      String.concat "" (List.mapi (fun index -> row (index + 1)) configurations)
    in
    add_to_report "run-summary" "Run summary"
      (Paragraph
         (Printf.sprintf
            "<div class=\"run-matrix-scroll\"><table class=\"run-matrix\"><thead><tr><th scope=\"col\">Run configuration</th>%s</tr></thead><tbody>%s</tbody></table></div>"
            headings rows))

let lexer_difference_report actual expected =
  let length = max (List.length actual) (List.length expected) in
  let token_at tokens index =
    match List.nth_opt tokens index with
    | Some token -> Printf.sprintf "<code>%s</code>" (escape_html_text token)
    | None -> "<span class=\"lexer-token-missing\">missing</span>"
  in
  let differences =
    List.init length Fun.id
    |> List.filter (fun index -> List.nth_opt actual index <> List.nth_opt expected index)
  in
  match differences with
  | [] -> ""
  | first :: _ ->
    let rows = String.concat ""
        (List.map (fun index ->
             Printf.sprintf
               "<tr><th scope=\"row\">%d</th><td>%s</td><td>%s</td></tr>"
               (index + 1) (token_at expected index) (token_at actual index))
           differences)
    in
    Printf.sprintf
      "<div class=\"lexer-difference\"><div class=\"lexer-difference-heading\"><i class=\"fa fa-exclamation-triangle\" aria-hidden=\"true\"></i><div><strong>Lexer output differs starting at token %d</strong><p>Expected %d token%s; observed %d token%s. Every differing position is listed below.</p></div></div><div class=\"lexer-difference-table-scroll\"><table class=\"lexer-difference-table\"><thead><tr><th>Position</th><th>Expected</th><th>Observed</th></tr></thead><tbody>%s</tbody></table></div><details><summary>Expected token stream</summary>%s</details></div>"
      (first + 1)
      (List.length expected) (if List.length expected = 1 then "" else "s")
      (List.length actual) (if List.length actual = 1 then "" else "s")
      rows (Lexer_pretty_print.lexer_report_strings expected)

let lexer_report tokens =
  let observed = Lexer_pretty_print.lexer_report_strings tokens in
  match !expected_lexer with
  | Some expected when expected <> tokens ->
    lexer_difference_report tokens expected ^
    "<h4 class=\"lexer-stream-heading\">Observed token stream</h4>" ^ observed
  | _ -> observed

let () =
  let synopsis =
    "ecomp-report -compile-json FILE [-run-json FILE ...] [-report FILE]" in
  Cli_completion.handle "ecomp-report" cli_options;
  require_cli_arguments "ecomp-report" synopsis;
  Arg.parse speclist (fun _ -> ()) ("Usage: " ^ synopsis);
  Option.iter import_compiler_results !compiler_json;
  Option.iter load_manifest !compiler_json;
  Option.iter import_expectations !expectation_file;
  List.iteri (fun number filename ->
      import_run_results (List.nth_opt !expected_runs number) filename)
    (List.rev !run_jsons);
  Option.iter (fun source ->
      report_source := Some source;
      try add_to_report "Source" "Source" (Code (highlight_source (file_contents source)))
      with Sys_error message -> record_compile_result ~error:(Some message) "Read source") !source_file;
  Option.iter (fun lexer ->
      try
        let tokens = file_contents lexer |> String.split_on_char '\n'
                     |> List.filter (fun token -> String.trim token <> "") in
        add_to_report "lexer" "Lexer" (Paragraph (lexer_report tokens))
      with Sys_error _ | Failure _ -> ()) !lexer_file;
  add_test_configuration ();
  add_teaching_expectation ();
  add_expectation_mismatch ();
  add_run_summary ();
  let result =
    load_optional "ast" !ast_file >>= fun ast ->
    load_optional "e" !e_file >>= fun ep ->
    load_optional "cfg" !cfg_file >>= fun cfg ->
    load_optional "cfg-after-cp" !cfg_cp_file >>= fun cfg_cp ->
    load_optional "cfg-after-dae" !cfg_dae_file >>= fun cfg_dae ->
    load_optional "cfg-after-ne" !cfg_ne_file >>= fun cfg_ne ->
    load_optional "rtl" !rtl_file >>= fun rtl ->
    load_optional "linear" !linear_file >>= fun linear ->
    load_optional "linear-after-dse" !linear_dse_file >>= fun linear_dse ->
    load_optional "ltl" !ltl_file >>= fun ltl ->
    begin match !provenance with
    | None when has_compile_errors () -> OK ()
    | None -> Error "No artifacts loaded."
    | Some p ->
      report_source := Some p.source_path;
      let output = default_report () in
      resolved_report := Some output;
      let report_started = Unix.gettimeofday () in
      if !source_file = None then
        add_to_report "Source" "Source" (Code (highlight_source p.source_text));
      Option.iter (add_ast_graph output) ast;
      Option.iter (fun program ->
          add_to_report "e" "E"
            (Code (highlight_source (Format.asprintf "%a" Elang_pretty_print.dump_eprog program))))
        ep;
      add_runs "Elang";
      Option.iter (fun program ->
          add_cfg_graph output "cfg" "CFG" program)
        cfg;
      add_runs "CFG";
      Option.iter (fun program ->
          add_cfg_graph output "cfg-after-cstprop" "CFG after constant propagation" program)
        cfg_cp;
      add_runs "CFG after constant propagation";
      Option.iter (fun program ->
          add_cfg_graph output "cfg-after-dae" "CFG after dead assignment elimination" program)
        cfg_dae;
      add_runs "CFG after dead assignment elimination";
      Option.iter (fun program ->
          add_cfg_graph output "cfg-after-nop" "CFG after nop elimination" program)
        cfg_ne;
      add_runs "CFG after nop elimination";
      Option.iter (fun program ->
          add_code_section_safely "rtl" "RTL" (fun () ->
              Format.asprintf "%a" Rtl_pretty_print.dump_rtl_prog_html program))
        rtl;
      add_runs "RTL";
      Option.iter (fun program ->
          add_code_section_safely "linear" "Linear" (fun () ->
              let lives = Linear_liveness.liveness_linear_prog program in
              Format.asprintf "%a"
                (fun formatter -> Linear_pretty_print.dump_linear_prog_html formatter (Some lives)) program))
        linear;
      add_runs "Linear";
      Option.iter (fun program ->
          add_code_section_safely "linear-after-dse" "Linear after DSE" (fun () ->
              let lives = Linear_liveness.liveness_linear_prog program in
              Format.asprintf "%a"
                (fun formatter -> Linear_pretty_print.dump_linear_prog_html formatter (Some lives)) program))
        linear_dse;
      add_runs "Linear after DSE";
      Option.iter (fun rig ->
          if Sys.file_exists rig then add_rig_graph output rig)
        !rig_file;
      Option.iter (fun program ->
          add_code_section_safely "ltl" "LTL" (fun () ->
              Format.asprintf "%a" Ltl_pretty_print.dump_ltl_prog_html program))
        ltl;
      add_runs "LTL";
      Option.iter (fun program ->
          add_code_section_safely "riscv" "RISC-V" (fun () ->
              Format.asprintf "%a"
                (Riscv.dump_riscv_prog_html !Archi.target) program))
        ltl;
      add_runs "Risc-V";
      record_compile_result
        ~data:[`Assoc [("seconds", `Float (Unix.gettimeofday () -. report_started))]]
        "Build report";
      OK ()
    end
  in
  begin match result with
  | OK () -> ()
  | Error message -> record_compile_result ~error:(Some message) "Report"; prerr_endline message
  end;
  let output = Option.value ~default:(default_report ()) !resolved_report in
  add_compile_errors ();
  make_report ~output (Option.value ~default:"ecomp-report" !report_source) report ();
  match result with OK () -> () | Error _ -> exit 2
