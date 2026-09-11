open Elang_print
open Rtl
open Prog
open Utils
module Set = Collections.IntSet


let print_reg r =
  Format.sprintf "r%d" r

let print_cmpop (r: rtl_cmp) =
  (match r with
  | Rclt -> "<"
  | Rcle -> "<="
  | Rcgt -> ">"
  | Rcge -> ">="
  | Rceq -> "=="
  | Rcne -> "!=")

let dump_rtl_instr name (live_in, live_out) ?(endl="\n") oc (i: rtl_instr) =
  let print_node s = Format.sprintf "%s_%d" name s in

  let dump_liveness live where =
    match live with
      Some live ->
        Format.fprintf oc "// Live %s : { %s }\n" where
          (String.concat ", " (List.map string_of_int (Set.to_list live)))
    | None -> ()
  in
  dump_liveness live_in "before";
  begin match i with
  | Rbinop (b, rd, rs1, rs2) ->
    Format.fprintf oc "%s <- %s(%s, %s)" (print_reg rd) (dump_binop b) (print_reg rs1) (print_reg rs2)
  | Runop (u, rd, rs) ->
    Format.fprintf oc "%s <- %s(%s)" (print_reg rd) (dump_unop u) (print_reg rs)
  | Rconst (rd, i) ->
    Format.fprintf oc "%s <- %d" (print_reg rd) i
  | Rbranch (cmpop, r1, r2, s1) ->
    Format.fprintf oc "%s %s %s ? jmp %s" (print_reg r1) (print_cmpop cmpop) (print_reg r2) (print_node s1)
  | Rjmp s ->
    Format.fprintf oc "jmp %s" (print_node s)
  | Rmov (rd, rs) -> Format.fprintf oc "%s <- %s" (print_reg rd) (print_reg rs)
  | Rret r -> Format.fprintf oc "ret %s" (print_reg r)
  | Rprint r -> Format.fprintf oc "print %s" (print_reg r)
  | Rlabel n -> Format.fprintf oc "%s_%d:" name n
  end;
  Format.fprintf oc "%s" endl;
  dump_liveness live_out "after"

let dump_rtl_node name lives =
  print_listi (fun i ->
      dump_rtl_instr name
        (match lives with
           None -> (None, None)
         | Some (lin, lout) ->
           Hashtbl.find_opt lin i, Hashtbl.find_opt lout i)
        ~endl:"\n"
    ) "" "" ""

let dump_rtl_fun oc rtlfunname ({ rtlfunargs; rtlfunbody; rtlfunentry }: rtl_fun) =
  Format.fprintf oc "%s(%s):\n" rtlfunname
    (String.concat ", " $ List.map print_reg rtlfunargs);
  Hashtbl.iter (fun n node ->
      Format.fprintf oc "%s_%d:\n" rtlfunname n;
      dump_rtl_node rtlfunname None oc node) rtlfunbody

let dump_rtl_prog oc cp =
  dump_prog dump_rtl_fun oc cp

let html_escape s =
  let escaped = Buffer.create (String.length s) in
  String.iter (function
      | '&' -> Buffer.add_string escaped "&amp;"
      | '<' -> Buffer.add_string escaped "&lt;"
      | '>' -> Buffer.add_string escaped "&gt;"
      | '"' -> Buffer.add_string escaped "&quot;"
      | '\'' -> Buffer.add_string escaped "&#39;"
      | c -> Buffer.add_char escaped c
    ) s;
  Buffer.contents escaped

let html_span class_name contents =
  Format.sprintf "<span class=\"%s\">%s</span>"
    class_name (html_escape contents)

let html_reg r = html_span "rtl-register" (print_reg r)
let html_opcode opcode = html_span "rtl-opcode" opcode
let html_number number = html_span "rtl-number" (string_of_int number)
let html_label label = html_span "rtl-label" label
let html_operator operator = html_span "rtl-operator" operator

let dump_rtl_instr_html name oc = function
  | Rbinop (operator, destination, source1, source2) ->
    Format.fprintf oc "%s %s %s(%s, %s)"
      (html_reg destination) (html_operator "<-")
      (html_opcode (dump_binop operator))
      (html_reg source1) (html_reg source2)
  | Runop (operator, destination, source) ->
    Format.fprintf oc "%s %s %s(%s)"
      (html_reg destination) (html_operator "<-")
      (html_opcode (dump_unop operator)) (html_reg source)
  | Rconst (destination, value) ->
    Format.fprintf oc "%s %s %s"
      (html_reg destination) (html_operator "<-") (html_number value)
  | Rbranch (comparison, source1, source2, successor) ->
    Format.fprintf oc "%s %s %s %s %s %s"
      (html_reg source1) (html_operator (print_cmpop comparison))
      (html_reg source2) (html_operator "?") (html_opcode "jmp")
      (html_label (Format.sprintf "%s_%d" name successor))
  | Rjmp successor ->
    Format.fprintf oc "%s %s"
      (html_opcode "jmp")
      (html_label (Format.sprintf "%s_%d" name successor))
  | Rmov (destination, source) ->
    Format.fprintf oc "%s %s %s"
      (html_reg destination) (html_operator "<-") (html_reg source)
  | Rret source ->
    Format.fprintf oc "%s %s" (html_opcode "ret") (html_reg source)
  | Rprint source ->
    Format.fprintf oc "%s %s" (html_opcode "print") (html_reg source)
  | Rlabel label ->
    Format.fprintf oc "%s"
      (html_label (Format.sprintf "%s_%d:" name label))

let dump_live_set_html oc live =
  if Set.is_empty live then
    Format.fprintf oc "<span class=\"rtl-liveness-empty\">empty</span>"
  else
    Set.iter (fun register ->
        Format.fprintf oc "<span class=\"rtl-live-register\">%s</span>"
          (html_reg register)
      ) live

let dump_liveness_html oc where = function
  | None -> ()
  | Some live ->
    Format.fprintf oc
      "<span class=\"rtl-liveness rtl-liveness-%s\"><span class=\"rtl-liveness-label\">Live %s</span><span class=\"rtl-liveness-registers\">"
      where where;
    dump_live_set_html oc live;
    Format.fprintf oc "</span></span>"

let liveness_at_index lives index =
  match lives with
  | None -> None, None
  | Some (live_in, live_out) ->
    Hashtbl.find_opt live_in index, Hashtbl.find_opt live_out index

let dump_liveness_row_html lives index oc =
  let live_in, live_out =
    liveness_at_index lives index
  in
  if live_in <> None || live_out <> None then begin
    Format.fprintf oc "<span class=\"rtl-liveness-row\">";
    dump_liveness_html oc "before" live_in;
    dump_liveness_html oc "after" live_out;
    Format.fprintf oc "</span>"
  end

let dump_rtl_instruction_row_html name lives index oc instruction =
  Format.fprintf oc "<span class=\"rtl-instruction\"><span class=\"rtl-instruction-code\">";
  dump_rtl_instr_html name oc instruction;
  Format.fprintf oc "</span>";
  dump_liveness_row_html lives index oc;
  Format.fprintf oc "</span>"

let dump_rtl_node_html name lives oc instructions =
  List.iteri (fun index instruction ->
      dump_rtl_instruction_row_html name lives index oc instruction
    ) instructions

let dump_rtl_fun_html oc name
    ({ rtlfunargs; rtlfunbody; rtlfunentry = _; rtlfuninfo = _ }: rtl_fun) =
  Format.fprintf oc
    "<span class=\"rtl-function\"><span class=\"rtl-function-signature\"><span class=\"rtl-function-name\">%s</span>(%s):</span>"
    (html_escape name)
    (String.concat ", " (List.map html_reg rtlfunargs));
  Hashtbl.iter (fun node instructions ->
      Format.fprintf oc
        "<span class=\"rtl-node\"><span class=\"rtl-node-label\">%s</span><span class=\"rtl-node-body\">"
        (html_label (Format.sprintf "%s_%d:" name node));
      dump_rtl_node_html name None oc instructions;
      Format.fprintf oc "</span></span>"
    ) rtlfunbody;
  Format.fprintf oc "</span>"

let dump_rtl_prog_html oc program =
  dump_prog dump_rtl_fun_html oc program
