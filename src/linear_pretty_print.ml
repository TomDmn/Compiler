(* HTML presentation for Linear.  Plain text dumping remains in [Linear_print]. *)
open Linear
open Rtl
open Prog
open Rtl_pretty_print

let dump_linear_prog_html formatter lives program =
  dump_prog (fun formatter name function_ ->
      let function_lives = Option.bind lives (fun values -> Hashtbl.find_opt values name) in
      Format.fprintf formatter "<span class=\"rtl-function linear-function\"><span class=\"rtl-function-signature\"><span class=\"rtl-function-name\">%s</span>(%s):</span>"
        (escape name) (String.concat ", " (List.map reg function_.linearfunargs));
      let opened = ref false in
      let close () = if !opened then begin Format.fprintf formatter "</span></span>"; opened := false end in
      let open_node node classes =
        close (); opened := true;
        Format.fprintf formatter "<span class=\"rtl-node %s\"><span class=\"rtl-node-label\">%s</span><span class=\"rtl-node-body\">" classes node in
      List.iteri (fun index instruction -> match instruction with
          | Rlabel value -> open_node (label (Format.sprintf "%s_%d:" name value)) "linear-node"; dump_row name function_lives index formatter instruction
          | _ ->
            if not !opened then open_node (span "rtl-node-entry-label" "Function entry") "linear-node rtl-node-entry";
            dump_row name function_lives index formatter instruction) function_.linearfunbody;
      close (); Format.fprintf formatter "</span>") formatter program
