open Rtl_print
open Rtl
open Linear
open Prog
open Utils


let dump_linear_fun oc lives lfname l =
  Format.fprintf oc "%s(%s):\n" lfname
    (String.concat ", " $ List.map print_reg l.linearfunargs);
  let lives = match lives with
    | None -> None
    | Some lives -> 
      (Hashtbl.find_opt lives lfname)
  in
  dump_rtl_node lfname lives oc l.linearfunbody

let dump_linear_prog oc lives lp =
  dump_prog (fun oc -> dump_linear_fun oc lives) oc lp

let dump_linear_fun_html oc lives name linear_fun =
  let function_lives =
    match lives with
    | None -> None
    | Some lives -> Hashtbl.find_opt lives name
  in
  Format.fprintf oc
    "<span class=\"rtl-function linear-function\"><span class=\"rtl-function-signature\"><span class=\"rtl-function-name\">%s</span>(%s):</span>"
    (html_escape name)
    (String.concat ", " (List.map html_reg linear_fun.linearfunargs));
  let node_is_open = ref false in
  let close_node () =
    if !node_is_open then begin
      Format.fprintf oc "</span></span>";
      node_is_open := false
    end
  in
  let open_node label extra_class =
    close_node ();
    Format.fprintf oc
      "<span class=\"rtl-node %s\"><span class=\"rtl-node-label\">%s</span><span class=\"rtl-node-body\">"
      extra_class label;
    node_is_open := true
  in
  List.iteri (fun index instruction ->
      match instruction with
      | Rlabel label ->
        open_node
          (html_label (Format.sprintf "%s_%d:" name label)) "linear-node";
        dump_liveness_row_html function_lives index oc
      | _ ->
        if not !node_is_open then
          open_node
            (html_span "rtl-node-entry-label" "Function entry")
            "linear-node rtl-node-entry";
        dump_rtl_instruction_row_html name function_lives index oc instruction
    ) linear_fun.linearfunbody;
  close_node ();
  Format.fprintf oc "</span>"

let dump_linear_prog_html oc lives program =
  dump_prog (fun oc -> dump_linear_fun_html oc lives) oc program
