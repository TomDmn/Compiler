(* Artifact replay executable.  Compilation deliberately lives in [main.ml]. *)
open Utils
open Options
open Report
open Cli_completion

let e_file : string option ref = ref None
let cfg_file : string option ref = ref None
let cfg_cp_file : string option ref = ref None
let cfg_dae_file : string option ref = ref None
let cfg_ne_file : string option ref = ref None
let rtl_file : string option ref = ref None
let linear_file : string option ref = ref None
let linear_dse_file : string option ref = ref None
let ltl_file : string option ref = ref None
let elf_file : string option ref = ref None

let e_run = ref false
let cfg_run = ref false
let cfg_run_after_cp = ref false
let cfg_run_after_dae = ref false
let cfg_run_after_ne = ref false
let rtl_run = ref false
let linear_run = ref false
let linear_run_after_dse = ref false
let ltl_run = ref false
let riscv_run = ref false

let all_run () =
  e_run := true; cfg_run := true; cfg_run_after_cp := true;
  cfg_run_after_dae := true; cfg_run_after_ne := true; rtl_run := true;
  linear_run := true; linear_run_after_dse := true; ltl_run := true; riscv_run := true

let cli_options = [
  Cli_completion.option ~argument:(Input_file []) "-e-load" (Arg.String (fun s -> e_file := Some s)) "Load Elang cache artifact.";
  Cli_completion.option ~argument:(Input_file []) "-cfg-load" (Arg.String (fun s -> cfg_file := Some s)) "Load CFG cache artifact.";
  Cli_completion.option ~argument:(Input_file []) "-cfg-load-after-cp" (Arg.String (fun s -> cfg_cp_file := Some s)) "Load CFG after constant propagation.";
  Cli_completion.option ~argument:(Input_file []) "-cfg-load-after-dae" (Arg.String (fun s -> cfg_dae_file := Some s)) "Load CFG after dead assignment elimination.";
  Cli_completion.option ~argument:(Input_file []) "-cfg-load-after-ne" (Arg.String (fun s -> cfg_ne_file := Some s)) "Load CFG after nop elimination.";
  Cli_completion.option ~argument:(Input_file []) "-rtl-load" (Arg.String (fun s -> rtl_file := Some s)) "Load RTL cache artifact.";
  Cli_completion.option ~argument:(Input_file []) "-linear-load" (Arg.String (fun s -> linear_file := Some s)) "Load Linear cache artifact.";
  Cli_completion.option ~argument:(Input_file []) "-linear-load-after-dse" (Arg.String (fun s -> linear_dse_file := Some s)) "Load Linear after dead store elimination.";
  Cli_completion.option ~argument:(Input_file []) "-ltl-load" (Arg.String (fun s -> ltl_file := Some s)) "Load LTL cache artifact.";
  Cli_completion.option ~argument:(Input_file []) "-riscv-exe-load" (Arg.String (fun s -> elf_file := Some s)) "Load RISC-V ELF with ecomp metadata.";
  Cli_completion.option "-e-run" (Arg.Set e_run) "Run loaded Elang program.";
  Cli_completion.option "-cfg-run" (Arg.Set cfg_run) "Run loaded CFG program.";
  Cli_completion.option "-cfg-run-after-cp" (Arg.Set cfg_run_after_cp) "Run loaded CFG after constant propagation.";
  Cli_completion.option "-cfg-run-after-dae" (Arg.Set cfg_run_after_dae) "Run loaded CFG after dead assignment elimination.";
  Cli_completion.option "-cfg-run-after-ne" (Arg.Set cfg_run_after_ne) "Run loaded CFG after nop elimination.";
  Cli_completion.option "-rtl-run" (Arg.Set rtl_run) "Run loaded RTL program.";
  Cli_completion.option "-linear-run" (Arg.Set linear_run) "Run loaded Linear program.";
  Cli_completion.option "-linear-run-after-dse" (Arg.Set linear_run_after_dse) "Run loaded Linear after dead store elimination.";
  Cli_completion.option "-ltl-run" (Arg.Set ltl_run) "Run loaded LTL program.";
  Cli_completion.option "-ltl-debug" (Arg.Set ltl_debug) "Debug the loaded LTL program.";
  Cli_completion.option "-riscv-run" (Arg.Set riscv_run) "Run loaded RISC-V ELF.";
  Cli_completion.option "-all-run" (Arg.Unit all_run) "Run every loaded stage.";
  Cli_completion.option ~argument:Integer "-heap" (Arg.Set_int heapsize) "Heap size";
  Cli_completion.option "-m32" (Arg.Unit (fun () -> Archi.archi := Archi.A32)) "32-bit mode";
  Cli_completion.option "-linux" (Arg.Unit (fun () -> Archi.target := Archi.Linux)) "Use Linux target";
  Cli_completion.option "-xv6" (Arg.Unit (fun () -> Archi.target := Archi.Xv6)) "Use xv6 target";
  Cli_completion.option ~argument:Output_file "-json" (Arg.String (fun s -> output_json := s)) "Output JSON summary";
  Cli_completion.option ~argument:Integer "--" (Arg.Rest (fun p -> params := int_of_string p :: !params)) "Run parameters."
]

let speclist = Cli_completion.speclist cli_options

let provenance : Artifact.provenance option ref = ref None

let check_provenance p =
  match !provenance with
  | None -> provenance := Some p; OK ()
  | Some expected when Artifact.compatible expected p -> OK ()
  | Some _ -> Error "Loaded artifacts do not have matching source/build provenance"

let load kind file =
  Artifact.load ~kind file >>= fun (p, value, seconds) ->
  record_compile_result ~data:[`Assoc [("seconds", `Float seconds)]] ("Unmarshal " ^ kind);
  check_provenance p >>= fun () -> OK value

let require flag file name kind =
  if not !flag then OK None else
    match !file with
    | None -> Error (Printf.sprintf "%s requested but no %s artifact was loaded" name kind)
    | Some file -> load kind file >>= fun value -> OK (Some value)

let run_elf oc file _memsize parameters =
  let command = String.concat " "
      (Filename.quote (Archi.qemu ()) :: Filename.quote file :: List.map string_of_int parameters) in
  let lines = cmd_to_list command in
  match List.rev lines with
  | [] -> OK None
  | last :: reversed_output ->
    List.rev reversed_output |> Utils.print_list
      (fun formatter line -> Format.fprintf formatter "%s" line) "" "\n" "" oc;
    (try OK (Some (int_of_string last)) with Failure _ ->
       Format.fprintf oc "%s\n" last; OK None)

let () =
  let synopsis = "ecomp-run -e-load FILE -e-run [-- ARGUMENT ...]" in
  Cli_completion.handle "ecomp-run" cli_options;
  require_cli_arguments "ecomp-run" synopsis;
  Arg.parse speclist (fun _ -> ()) ("Usage: " ^ synopsis);
  params := List.rev !params;
  let need_ltl = ref (!ltl_run || !ltl_debug) in
  let result =
    require e_run e_file "Elang run" "e" >>= fun ep ->
    require cfg_run cfg_file "CFG run" "cfg" >>= fun cfg ->
    require cfg_run_after_cp cfg_cp_file "CFG after CP run" "cfg-after-cp" >>= fun cfg_cp ->
    require cfg_run_after_dae cfg_dae_file "CFG after DAE run" "cfg-after-dae" >>= fun cfg_dae ->
    require cfg_run_after_ne cfg_ne_file "CFG after NE run" "cfg-after-ne" >>= fun cfg_ne ->
    require rtl_run rtl_file "RTL run" "rtl" >>= fun rtl ->
    require linear_run linear_file "Linear run" "linear" >>= fun linear ->
    require linear_run_after_dse linear_dse_file "Linear after DSE run" "linear-after-dse" >>= fun linear_dse ->
    require need_ltl ltl_file "LTL run or debugging" "ltl" >>= fun ltl ->
    (if not !riscv_run then OK None else match !elf_file with
     | None -> Error "RISC-V run requested but no RISC-V ELF was loaded"
     | Some file -> OK (Some file)) >>= fun elf ->
    begin match elf with
    | None -> OK ()
    | Some file ->
      Artifact.load_elf_metadata file >>= fun (p, seconds) ->
      record_compile_result ~data:[`Assoc [("seconds", `Float seconds)]]
        "Read RISC-V metadata";
      check_provenance p
    end >>= fun () ->
    begin match !provenance with
    | None -> Error "No artifacts loaded."
    | Some p ->
      Option.iter (fun program ->
          if !ltl_debug then Ltl_debug.debug_ltl_prog p.source_path program !heapsize !params)
        ltl;
      Option.iter (fun p -> run "Elang" true Elang_run.eval_eprog p) ep;
      Option.iter (fun p -> run "CFG" true Cfg_run.eval_cfgprog p) cfg;
      Option.iter (fun p -> run "CFG after constant propagation" true Cfg_run.eval_cfgprog p) cfg_cp;
      Option.iter (fun p -> run "CFG after dead assignment elimination" true Cfg_run.eval_cfgprog p) cfg_dae;
      Option.iter (fun p -> run "CFG after nop elimination" true Cfg_run.eval_cfgprog p) cfg_ne;
      Option.iter (fun p -> run "RTL" true Rtl_run.exec_rtl_prog p) rtl;
      Option.iter (fun p -> run "Linear" true Linear_run.exec_linear_prog p) linear;
      Option.iter (fun p -> run "Linear after DSE" true Linear_run.exec_linear_prog p) linear_dse;
      Option.iter (fun p -> if !ltl_run then run "LTL" true Ltl_run.exec_ltl_prog p) ltl;
      Option.iter (fun p -> run "Risc-V" true run_elf p) elf;
      OK ()
    end
  in
  begin match result with
  | OK () -> ()
  | Error message -> record_compile_result ~error:(Some message) "Replay"; prerr_endline message
  end;
  if !output_json = "-" then print_endline (json_output_string ())
  else begin
    let oc = open_out !output_json in
    output_string oc (json_output_string ()); output_char oc '\n'; close_out oc
  end;
  match result with OK () -> () | Error _ -> exit 2
