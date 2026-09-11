open Symbols
open Parser
open Ast
open Elang
open Elang_gen
open Cfg
open Cfg_gen
open Cfg_constprop
open Cfg_dead_assign
open Cfg_nop_elim
open Rtl
open Rtl_gen
open Linear
open Linear_gen
open Linear_liveness
open Linear_dse
open Ltl
open Ltl_gen
open Riscv
open Utils
open Archi
open Report
open Options
open Lexer_generator
open Tokenize
open Cli_completion

let cli_options =
  [
    Cli_completion.option ~argument:Output_file "-tokens-save" (Arg.String (fun s -> tokens_save := Some s)) "Save the recognized token stream.";
    Cli_completion.option ~argument:Output_file "-ast-save" (Arg.String (fun s -> ast_save := Some s)) "Save AST cache artifact.";
    Cli_completion.option ~argument:Output_file "-e-save" (Arg.String (fun s -> e_save := Some s)) "Save Elang cache artifact.";
    Cli_completion.option ~argument:Output_file "-cfg-save" (Arg.String (fun s -> cfg_save := Some s)) "Save CFG cache artifact.";
    Cli_completion.option ~argument:Output_file "-cfg-save-after-cp" (Arg.String (fun s -> cfg_save_after_cp := Some s)) "Save CFG after constant propagation.";
    Cli_completion.option ~argument:Output_file "-cfg-save-after-dae" (Arg.String (fun s -> cfg_save_after_dae := Some s)) "Save CFG after dead assignment elimination.";
    Cli_completion.option ~argument:Output_file "-cfg-save-after-ne" (Arg.String (fun s -> cfg_save_after_ne := Some s)) "Save CFG after nop elimination.";
    Cli_completion.option ~argument:Output_file "-rtl-save" (Arg.String (fun s -> rtl_save := Some s)) "Save RTL cache artifact.";
    Cli_completion.option ~argument:Output_file "-linear-save" (Arg.String (fun s -> linear_save := Some s)) "Save Linear cache artifact.";
    Cli_completion.option ~argument:Output_file "-linear-save-after-dse" (Arg.String (fun s -> linear_save_after_dse := Some s)) "Save Linear after dead store elimination.";
    Cli_completion.option ~argument:Output_file "-ltl-save" (Arg.String (fun s -> ltl_save := Some s)) "Save LTL cache artifact.";
    Cli_completion.option ~argument:Output_file "-riscv-dump" (Arg.String (fun s -> riscv_dump := Some s)) "Output RISC-V file.";
    Cli_completion.option ~argument:Output_file "-riscv-exe-save" (Arg.String (fun s -> riscv_exe_save := Some s)) "Save runnable RISC-V ELF with ecomp metadata.";
    Cli_completion.option "-clever-regalloc" (Arg.Unit (fun () -> naive_regalloc := false)) "Use the graph coloring algorithm for register allocation.";
    Cli_completion.option "-naive-regalloc" (Arg.Unit (fun () -> naive_regalloc := true)) "Use the naive algorithm for register allocation (all pseudo-registers go on the stack).";
    Cli_completion.option "-no-cfg-constprop" (Arg.Set no_cfg_constprop) "Disable CFG constprop";
    Cli_completion.option "-no-cfg-dae" (Arg.Set no_cfg_dae) "Disable CFG Dead Assign Elimination";
    Cli_completion.option "-no-cfg-ne" (Arg.Set no_cfg_ne) "Disable CFG Nop Elimination";
    Cli_completion.option "-no-linear-dse" (Arg.Set no_linear_dse) "Disable Linear Dead Store Elimination";
    Cli_completion.option ~argument:Output_file "-rig-save" (Arg.String (fun s -> rig_save := Some s)) "Save register allocation and interference data as JSON.";
    Cli_completion.option "-m32" (Arg.Unit (fun _ -> Archi.archi := A32)) "32bit mode";
    Cli_completion.option "-v" (Arg.Unit (fun _ -> Options.verbose := true)) "verbose mode (show external commands)";
    Cli_completion.option ~argument:(Input_file [".e"]) "-f" (Arg.String (fun s -> input_file := Some s)) "file to compile";
    Cli_completion.option "-alloc-order-ts" (Arg.Unit (fun _ -> Options.alloc_order_st := false)) "Allocate t regs before s regs";
    Cli_completion.option ~argument:Output_file "-json" (Arg.String (fun s -> output_json := s)) "Output JSON summary";
    Cli_completion.option "-nostart" (Arg.Set nostart) "Don't output _start code.";
    Cli_completion.option "-nostats" (Arg.Set nostats) "Don't output stats.";
    Cli_completion.option "-nomul" (Arg.Unit (fun _ -> has_mul := false)) "Target architecture without mul instruction.";
    Cli_completion.option "-lex-hand" (Arg.Unit (fun _ -> Options.handwritten_lexer := true)) "Use handwritten lexer generator";
    Cli_completion.option "-lex-auto" (Arg.Unit (fun _ -> Options.handwritten_lexer := false)) "Use OCamlLex lexer";
    Cli_completion.option "-linux" (Arg.Unit (fun _ -> target := Linux)) "emit linux syscalls";
    Cli_completion.option "-xv6" (Arg.Unit (fun _ -> target := Xv6)) "emit xv6 syscalls";
  ]

let speclist = Cli_completion.speclist cli_options

let set_default r v suff =
  match !r with
    None -> r := Some (v ^ suff)
  | _ -> ()

let save_artifact kind provenance destination value =
  Option.iter (fun filename ->
      let seconds = Artifact.save ~kind ~provenance filename value in
      record_compile_result ~data:[`Assoc [("seconds", `Float seconds);
                                            ("kind", `String kind);
                                            ("path", `String filename)]]
        ("Marshal " ^ kind)) destination

let process_status = function
  | Unix.WEXITED code -> Printf.sprintf "exit status %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stop signal %d" signal

let run_tool label command =
  let started = Unix.gettimeofday () in
  if !Options.verbose then Printf.printf "%s: %s\n" label command;
  let output, status = process_output_to_list2 (command ^ " 2>&1") in
  let data = [`Assoc [
      ("seconds", `Float (Unix.gettimeofday () -. started))
    ]] in
  match status with
  | Unix.WEXITED 0 ->
    record_compile_result ~data label;
    OK ()
  | _ ->
    let details = match output with
      | [] -> ""
      | lines -> "\n" ^ String.concat "\n" lines
    in
    let message = Printf.sprintf "%s failed with %s:%s"
        label (process_status status) details in
    record_compile_result ~error:(Some message) ~data label;
    Error message

let protect_compile_step label f =
  try time_compile_step label f with exception_raised ->
    let message =
      Printexc.to_string exception_raised ^ "\n" ^ Printexc.get_backtrace () in
    record_compile_result ~error:(Some message) label;
    Error message

let compile_rv ?output basename asmfile () =
  if not !Options.nostart then begin
    let obj_file_prog = Filename.temp_file ~temp_dir:"/tmp" "" ".o" in
    let cmdas_prog = Format.sprintf "%s -I%s -o %s %s"
        (Archi.assembler ())
        (Archi.runtime_lib_include_path ())
        obj_file_prog asmfile in
    let obj_file_lib = Filename.temp_file ~temp_dir:"/tmp" "" ".o" in
    let cmdas_lib = Format.sprintf "%s -I%s -o %s %s"
        (Archi.assembler ())
        (Archi.runtime_lib_include_path ())
        obj_file_lib (Archi.runtime_lib_path ()) in
    let executable = Option.value ~default:(basename ^ ".exe") output in
    let cmdld = Format.sprintf "%s -T %s/link.ld %s %s -o %s"
        (Archi.linker ())
        Config.runtime_dir
        obj_file_prog obj_file_lib
        executable in
    run_tool "Assemble RISC-V program" cmdas_prog >>= fun () ->
    run_tool "Assemble RISC-V runtime" cmdas_lib >>= fun () ->
    run_tool "Link RISC-V executable" cmdld >>= fun () ->
    OK (Some executable)
  end
  else OK None

let _ =
  let synopsis = "ecomp -f SOURCE [OPTIONS]" in
  Cli_completion.handle "ecomp" cli_options;
  require_cli_arguments "ecomp" synopsis;
  Arg.parse speclist (fun _ -> ()) ("Usage: " ^ synopsis);
  Archi.archi := !archi;
  match !input_file with
  | None -> failwith "No input file specified.\n"
  | Some input ->
    let source = file_contents input in
    let provenance = Artifact.provenance ~source_path:input ~source_text:source in
    match Filename.chop_suffix_opt ~suffix:".e" input with
      None -> failwith
                (Format.sprintf "File (%s) should end in .e" input)
    | Some basename ->
      set_default riscv_dump basename ".s";

      Printexc.record_backtrace true;
      let compiler_res =
        try
        protect_compile_step "Lexing" (fun () -> pass_tokenize input) >>= fun tokens ->
        protect_compile_step "Parsing" (fun () -> pass_parse tokens) >>= fun (ast, _) ->
        save_artifact "ast" provenance !ast_save ast;
        protect_compile_step "Elang" (fun () -> pass_elang ast) >>= fun ep ->
        save_artifact "e" provenance !e_save ep;
        protect_compile_step "CFG" (fun () -> pass_cfg_gen ep) >>= fun cfg ->
        save_artifact "cfg" provenance !cfg_save cfg;
        protect_compile_step "Constprop" (fun () -> pass_constant_propagation cfg) >>= fun cfg ->
        save_artifact "cfg-after-cp" provenance !cfg_save_after_cp cfg;
        protect_compile_step "DeadAssign" (fun () -> pass_dead_assign_elimination cfg) >>= fun cfg ->
        save_artifact "cfg-after-dae" provenance !cfg_save_after_dae cfg;
        protect_compile_step "NopElim" (fun () -> pass_nop_elimination cfg) >>= fun cfg ->
        save_artifact "cfg-after-ne" provenance !cfg_save_after_ne cfg;
        protect_compile_step "RTL" (fun () -> pass_rtl_gen cfg) >>= fun rtl ->
        save_artifact "rtl" provenance !rtl_save rtl;
        protect_compile_step "Linear" (fun () -> pass_linearize rtl) >>= fun (linear, lives) ->
        save_artifact "linear" provenance !linear_save linear;
        protect_compile_step "DSE" (fun () -> pass_linear_dse linear lives) >>= fun linear ->
        save_artifact "linear-after-dse" provenance !linear_save_after_dse linear;
        protect_compile_step "LTL" (fun () -> pass_ltl_gen linear) >>= fun (ltl, allocations) ->
        Option.iter (fun filename ->
            let seconds = Rig_json.save ~provenance filename allocations in
            record_compile_result ~data:[`Assoc [("seconds", `Float seconds);
                                                  ("kind", `String "rig");
                                                  ("path", `String filename)]] "Save rig") !rig_save;
        OK ltl
        with e ->
          let emsg = Printexc.to_string e ^ "\n" ^ Printexc.get_backtrace () in
          record_compile_result ~error:(Some emsg) "global";
          Error emsg
      in
      begin
        match compiler_res with
        | Error msg -> ()
        | OK ltl ->
          save_artifact "ltl" provenance !ltl_save ltl;
          ignore (protect_compile_step "Risc-V" (fun () ->
              dump !riscv_dump (dump_riscv_prog !Archi.target) ltl (fun file () ->
                  match compile_rv ?output:!riscv_exe_save basename file () with
                  | Error _message -> ()
                  | OK None -> ()
                  | OK (Some executable) ->
                    Option.iter (fun _ ->
                        match Artifact.save_elf_metadata executable provenance with
                        | OK seconds ->
                          record_compile_result ~data:[`Assoc [("seconds", `Float seconds);
                                                                ("kind", `String "riscv-exe");
                                                                ("path", `String executable)]]
                            "Embed RISC-V metadata"
                        | Error message ->
                          record_compile_result ~error:(Some message)
                            "Embed RISC-V metadata")
                      !riscv_exe_save);
              OK ()))
      end;
      dump (Some !output_json) (fun oc p ->
          Format.fprintf oc "%s\n" p
        ) (json_output_string ()) (fun _ () -> ());
      match compile_errors () with
      | [] -> ()
      | errors ->
        List.iter (fun (step, message) ->
            Printf.eprintf "%s:\n%s\n" step message) errors;
        exit 2
