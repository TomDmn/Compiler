open Cfg
open Elang_print
open Prog
module Set = Collections.StringSet

let rec dump_cfgexpr : expr -> string = function
  | Ebinop(b, e1, e2) -> Format.sprintf "(%s %s %s)" (dump_cfgexpr e1) (dump_binop b) (dump_cfgexpr e2)
  | Eunop(u, e) -> Format.sprintf "(%s %s)" (dump_unop u) (dump_cfgexpr e)
  | Eint i -> Format.sprintf "%d" i
  | Evar s -> Format.sprintf "%s" s

let dump_list_cfgexpr l =
  l |> List.map dump_cfgexpr |> String.concat ", "

let escape_graphviz_html text =
  let escaped = Buffer.create (String.length text) in
  String.iter (function
      | '&' -> Buffer.add_string escaped "&amp;"
      | '<' -> Buffer.add_string escaped "&lt;"
      | '>' -> Buffer.add_string escaped "&gt;"
      | '"' -> Buffer.add_string escaped "&quot;"
      | character -> Buffer.add_char escaped character
    ) text;
  Buffer.contents escaped

let cfg_html color ?(bold=false) contents =
  Format.sprintf "<FONT COLOR=\"%s\">%s%s%s</FONT>"
    color (if bold then "<B>" else "")
    (escape_graphviz_html contents) (if bold then "</B>" else "")

let rec dump_cfgexpr_html = function
  | Ebinop (operator, left, right) ->
    Format.sprintf "%s%s %s %s%s"
      (cfg_html "#94a3b8" "(") (dump_cfgexpr_html left)
      (cfg_html "#be185d" ~bold:true (dump_binop operator))
      (dump_cfgexpr_html right) (cfg_html "#94a3b8" ")")
  | Eunop (operator, expression) ->
    Format.sprintf "%s%s %s%s"
      (cfg_html "#94a3b8" "(")
      (cfg_html "#be185d" ~bold:true (dump_unop operator))
      (dump_cfgexpr_html expression) (cfg_html "#94a3b8" ")")
  | Eint value -> cfg_html "#b45309" (string_of_int value)
  | Evar variable -> cfg_html "#0369a1" ~bold:true variable

let dump_cfg_node_html = function
  | Cassign (variable, expression, _) ->
    Format.sprintf "%s %s %s"
      (cfg_html "#0369a1" ~bold:true variable)
      (cfg_html "#64748b" "=") (dump_cfgexpr_html expression)
  | Cprint (expression, _) ->
    Format.sprintf "%s %s"
      (cfg_html "#7c3aed" ~bold:true "print") (dump_cfgexpr_html expression)
  | Creturn expression ->
    Format.sprintf "%s %s"
      (cfg_html "#047857" ~bold:true "return") (dump_cfgexpr_html expression)
  | Ccmp (expression, _, _) -> dump_cfgexpr_html expression
  | Cnop _ -> cfg_html "#64748b" "nop"

let cfg_node_style entry = function
  | Ccmp _ ->
    ("diamond", "filled", "#fff7ed", "#fb923c")
  | Creturn _ ->
    ("oval", "filled", "#ecfdf5", "#34d399")
  | Cprint _ ->
    ("box", "rounded,filled", "#faf5ff", "#c4b5fd")
  | Cnop _ ->
    ("box", "rounded,dashed,filled", "#f8fafc", "#94a3b8")
  | Cassign _ when entry ->
    ("box", "rounded,filled", "#eff6ff", "#3b82f6")
  | Cassign _ ->
    ("box", "rounded,filled", "#ffffff", "#cbd5e1")


let dump_arrows oc fname n (node: cfg_node) =
  match node with
  | Cassign (_, _, succ)
  | Cprint (_, succ)
  | Cnop succ ->
    Format.fprintf oc
      "n_%s_%d -> n_%s_%d [color=\"#94a3b8\"];\n"
      fname n fname succ
  | Creturn _ -> ()
  | Ccmp (_, succ1, succ2) ->
    Format.fprintf oc
      "n_%s_%d -> n_%s_%d [label=<<B>true</B>>, color=\"#16a34a\", fontcolor=\"#15803d\"];\n"
      fname n fname succ1;
    Format.fprintf oc
      "n_%s_%d -> n_%s_%d [label=<<B>false</B>>, color=\"#dc2626\", fontcolor=\"#b91c1c\"];\n"
      fname n fname succ2


let dump_cfg_node oc (node: cfg_node) =
  match node with
  | Cassign (v, e, _) -> Format.fprintf oc "%s = %s" v (dump_cfgexpr e)
  | Cprint (e, _) -> Format.fprintf oc "print %s" (dump_cfgexpr e)
  | Creturn e -> Format.fprintf oc "return %s" (dump_cfgexpr e)
  | Ccmp (e, _, _) -> Format.fprintf oc "%s" (dump_cfgexpr e)
  | Cnop _ -> Format.fprintf oc "nop"


let dump_liveness_state oc ht state =
  Hashtbl.iter (fun n cn ->
      Format.fprintf oc "%a : " dump_cfg_node cn;
      let vs = Collections.hashtbl_find_default state n Set.empty in
      Set.iter (fun v ->Format.fprintf oc "%s, " v) vs;
      Format.fprintf oc "\n";
      flush_all ()
    ) ht

let dump_cfg_fun oc cfgfunname ({ cfgfunbody; cfgentry; _ }: cfg_fun) =
  Format.fprintf oc
    "subgraph cluster_%s {\n label=<<B>%s</B>>; labelloc=\"t\"; labeljust=\"l\"; style=\"rounded,filled\"; color=\"#cbd5e1\"; fillcolor=\"#f8fafc\"; penwidth=\"1.2\"; margin=\"18\";\n"
    cfgfunname (escape_graphviz_html cfgfunname);
  Hashtbl.iter (fun n node ->
      let entry = n = cfgentry in
      let shape, style, fillcolor, color = cfg_node_style entry node in
      Format.fprintf oc
        "n_%s_%d [shape=%s style=\"%s\" fillcolor=\"%s\" color=\"%s\" penwidth=\"%s\" margin=\"0.16,0.10\" label=<<FONT POINT-SIZE=\"9\" COLOR=\"#64748b\">#%d%s</FONT><BR/><FONT FACE=\"DejaVu Sans Mono\" POINT-SIZE=\"11\">%s</FONT>>];\n"
        cfgfunname n shape style fillcolor
        (if entry then "#2563eb" else color)
        (if entry then "2.4" else "1.2") n
        (if entry then "  ENTRY" else "")
        (dump_cfg_node_html node);
      dump_arrows oc cfgfunname n node
    ) cfgfunbody;
  Format.fprintf oc "}\n"

let dump_cfg_prog oc (cp: cprog) =
  Format.fprintf oc
    "digraph G {\n graph [bgcolor=\"transparent\" pad=\"0.25\" nodesep=\"0.38\" ranksep=\"0.55\" fontname=\"DejaVu Sans\" fontcolor=\"#334155\"];\n node [fontname=\"DejaVu Sans\"];\n edge [fontname=\"DejaVu Sans\" fontsize=\"10\" color=\"#94a3b8\" arrowsize=\"0.72\" penwidth=\"1.25\"];\n";
  dump_prog dump_cfg_fun oc cp;
  Format.fprintf oc "}\n"
