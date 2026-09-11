open Regalloc

let location_json = function
  | Reg register -> `Assoc [("kind", `String "register"); ("number", `Int register)]
  | Stk offset -> `Assoc [("kind", `String "stack"); ("offset", `Int offset)]

let provenance_json (provenance : Artifact.provenance) =
  `Assoc [
    ("source_path", `String provenance.source_path);
    ("source_digest", `String provenance.source_digest);
    ("ocaml_version", `String provenance.ocaml_version);
    ("build_id", `String provenance.build_id);
    ("architecture", `String provenance.architecture);
    ("target", `String provenance.target);
  ]

let graph_json name (rig, allocation, next_stack_slot) =
  let registers =
    Seq.append (Hashtbl.to_seq_keys rig) (Hashtbl.to_seq_keys allocation) |> List.of_seq
    |> List.sort_uniq Int.compare
    |> List.map (fun register ->
        `Assoc (("register", `Int register) ::
                match Hashtbl.find_opt allocation register with
                | None -> []
                | Some location -> [("allocation", location_json location)])) in
  let edges =
    Hashtbl.to_seq rig |> Seq.flat_map (fun (left, neighbours) ->
        Collections.IntSet.to_seq neighbours
        |> Seq.filter_map (fun right ->
            if left < right then Some (`List [`Int left; `Int right]) else None))
    |> List.of_seq in
  `Assoc [
    ("name", `String name);
    ("registers", `List registers);
    ("interferences", `List edges);
    ("next_stack_slot", `Int next_stack_slot);
  ]

let save ~provenance filename (allocations : allocations) =
  let started = Unix.gettimeofday () in
  let functions = Hashtbl.to_seq allocations |> List.of_seq
                  |> List.sort (fun (left, _) (right, _) -> String.compare left right)
                  |> List.map (fun (name, graph) -> graph_json name graph) in
  let channel = open_out filename in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
      Yojson.Safe.pretty_to_channel channel
        (`Assoc [("format", `String "ecomp-rig"); ("version", `Int 1);
                 ("provenance", provenance_json provenance); ("functions", `List functions)]));
  Unix.gettimeofday () -. started

let load filename = Yojson.Safe.from_file filename

let function_graphs = function
  | `Assoc fields ->
    begin match List.assoc_opt "functions" fields with
    | Some (`List functions) ->
      List.filter_map (function
          | `Assoc function_fields as graph ->
            begin match List.assoc_opt "name" function_fields with
            | Some (`String name) -> Some (name, graph)
            | _ -> None
            end
          | _ -> None)
        functions
    | _ -> []
    end
  | _ -> []

let dump_dot formatter json =
  let colors = [|"#dbeafe"; "#bfdbfe"; "#bae6fd"; "#a7f3d0"; "#bbf7d0";
                 "#fef3c7"; "#fde68a"; "#fed7aa"; "#fecdd3"; "#e9d5ff"|] in
  let field name fields = List.assoc_opt name fields in
  let string_field name fields = match field name fields with Some (`String value) -> value | _ -> "?" in
  let int_field name fields = match field name fields with Some (`Int value) -> value | _ -> 0 in
  let allocation fields = match field "allocation" fields with
    | Some (`Assoc allocation) ->
      begin match string_field "kind" allocation with
      | "register" ->
        let register = int_field "number" allocation in
        Printf.sprintf "x%d" register, colors.(register mod Array.length colors), "filled"
      | "stack" -> Printf.sprintf "stk(%d)" (int_field "offset" allocation), "#f8fafc", "filled,dashed"
      | _ -> "unallocated", "#f8fafc", "filled,dashed"
      end
    | _ -> "unallocated", "#f8fafc", "filled,dashed" in
  Format.fprintf formatter "digraph RIG {\ngraph [bgcolor=\"#ffffff\", pad=\"0.35\", fontname=\"Arial\"];\nnode [shape=circle, fontname=\"Arial\", fontsize=11, fontcolor=\"#0f172a\", color=\"#64748b\", penwidth=1.3];\nedge [dir=none, color=\"#cbd5e166\", penwidth=0.9];\n";
  begin match json with
  | `Assoc function_fields ->
    begin match field "registers" function_fields with Some (`List registers) ->
      List.iter (function `Assoc register_fields ->
          let register = int_field "register" register_fields in
          let location, fill, style = allocation register_fields in
          Format.fprintf formatter "r%d [label=\"r%d\\n%s\", fillcolor=\"%s\", style=\"%s\"];\n"
            register register location fill style
        | _ -> ()) registers | _ -> () end;
    begin match field "interferences" function_fields with Some (`List edges) ->
      List.iter (function `List [`Int left; `Int right] ->
          Format.fprintf formatter "r%d -> r%d;\n" left right
        | _ -> ()) edges | _ -> () end
  | _ -> ()
  end;
  Format.fprintf formatter "}\n"
