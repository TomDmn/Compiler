open Utils

(* Trusted, local compiler caches.  These are deliberately Marshal-based: they
   are only accepted from the same ecomp build and must never be loaded from an
   untrusted source. *)

type provenance = {
  source_path : string;
  source_text : string;
  source_digest : string;
  ocaml_version : string;
  build_id : string;
  architecture : string;
  target : string;
}

type header = { magic : string; revision : int; kind : string; provenance : provenance }

let magic = "ECOMP-ARTIFACT"
let revision = 1

let architecture () = match !Archi.archi with Archi.A32 -> "rv32" | Archi.A64 -> "rv64"
let target () = match !Archi.target with Archi.Linux -> "linux" | Archi.Xv6 -> "xv6"

let build_id () =
  Digest.to_hex (Digest.string (String.concat ":" [Sys.ocaml_version; Config.runtime_dir]))

let provenance ~source_path ~source_text =
  { source_path; source_text; source_digest = Digest.to_hex (Digest.string source_text);
    ocaml_version = Sys.ocaml_version; build_id = build_id ();
    architecture = architecture (); target = target () }

let header kind provenance = { magic; revision; kind; provenance }

let save ~kind ~provenance filename value =
  let started = Unix.gettimeofday () in
  let oc = open_out_bin filename in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () ->
      Marshal.to_channel oc (header kind provenance, value) [];
      Unix.gettimeofday () -. started)

let compatible expected actual =
  expected.source_digest = actual.source_digest &&
  expected.ocaml_version = actual.ocaml_version &&
  expected.build_id = actual.build_id &&
  expected.architecture = actual.architecture && expected.target = actual.target

let load ~kind filename =
  try
    let started = Unix.gettimeofday () in
    let ic = open_in_bin filename in
    Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
        let header, value = Marshal.from_channel ic in
        if header.magic <> magic || header.revision <> revision then
          Error (Printf.sprintf "%s is not an ecomp artifact (or uses an unsupported format)" filename)
        else if header.kind <> kind then
          Error (Printf.sprintf "%s contains %s, not %s" filename header.kind kind)
        else if header.provenance.ocaml_version <> Sys.ocaml_version ||
                header.provenance.build_id <> build_id () then
          Error (Printf.sprintf "%s was produced by an incompatible ecomp build" filename)
        else OK (header.provenance, value, Unix.gettimeofday () -. started))
  with Sys_error message | Failure message -> Error message

let save_elf_metadata filename provenance =
  let started = Unix.gettimeofday () in
  let metadata = Filename.temp_file "ecomp-meta" ".bin" in
  let output = Filename.temp_file ~temp_dir:(Filename.dirname filename) "ecomp-elf" ".exe" in
  Fun.protect ~finally:(fun () ->
      (try Sys.remove metadata with Sys_error _ -> ());
      (try Sys.remove output with Sys_error _ -> ())) (fun () ->
      let oc = open_out_bin metadata in
      Marshal.to_channel oc (header "riscv-exe" provenance) [];
      close_out oc;
      let command = Printf.sprintf "%s --add-section .ecomp.meta=%s --set-section-flags .ecomp.meta=readonly,data %s %s"
          (Filename.quote (Archi.objcopy ())) (Filename.quote metadata)
          (Filename.quote filename) (Filename.quote output) in
      if Sys.command command <> 0 then Error "Could not add .ecomp.meta to RISC-V executable"
      else (Sys.rename output filename; OK (Unix.gettimeofday () -. started)))

let load_elf_metadata filename =
  let started = Unix.gettimeofday () in
  let metadata = Filename.temp_file "ecomp-meta" ".bin" in
  Fun.protect ~finally:(fun () -> try Sys.remove metadata with Sys_error _ -> ()) (fun () ->
      let command = Printf.sprintf "%s --dump-section .ecomp.meta=%s %s"
          (Filename.quote (Archi.objcopy ())) (Filename.quote metadata) (Filename.quote filename) in
      if Sys.command command <> 0 then Error (Printf.sprintf "%s has no readable .ecomp.meta section" filename)
      else
        try
          let ic = open_in_bin metadata in
          let header : header = Marshal.from_channel ic in
          close_in ic;
          if header.magic <> magic || header.revision <> revision || header.kind <> "riscv-exe" then
            Error (Printf.sprintf "%s has invalid ecomp metadata" filename)
          else if header.provenance.ocaml_version <> Sys.ocaml_version || header.provenance.build_id <> build_id () then
            Error (Printf.sprintf "%s was produced by an incompatible ecomp build" filename)
          else OK (header.provenance, Unix.gettimeofday () -. started)
        with Sys_error message | Failure message -> Error message)
