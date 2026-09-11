
(* ASTs are trees, of type [tree], labeled by [tag].

   A tree [tree] is either a node [Node(t, children)] where [t] is a tag and
   [children] is a list of subtrees, or a leaf containing a string
   ([StringLeaf]), an integer ([IntLeaf]), a character
   ([CharLeaf]), or nothing at all ([NullLeaf]).

   The meaning of the different tags:

   - is largely up to you: you may define new tags if that seems useful, as
   long as you complete the [string_of_tag] function below.

   - should be fairly clear from the tag name or its use in the example given
   in the handout.

   - can be clarified by your favorite lab instructor (or whichever instructor
   is present).


*)

type tag = Tassign | Tif | Twhile | Tblock | Treturn | Tprint
         | Tint
         | Tadd | Tmul | Tdiv | Tmod | Txor | Tsub
         | Tclt | Tcgt | Tcle | Tcge | Tceq | Tne
         | Tneg
         | Tlistglobdef
         | Tfundef | Tfunname | Tfunargs | Tfunbody
         | Tassignvar
         | Targ

type tree = | Node of tag * tree list
            | StringLeaf of string
            | IntLeaf of int
            | NullLeaf
            | CharLeaf of char

let string_of_stringleaf = function
  | StringLeaf s -> s
  | _ -> failwith "string_of_stringleaf called on non-stringleaf nodes."

type astfun = (string list * tree)
type ast = (string * astfun) list

let string_of_tag = function
  | Tassign -> "Tassign"
  | Tif -> "Tif"
  | Twhile -> "Twhile"
  | Tblock -> "Tblock"
  | Treturn -> "Treturn"
  | Tprint -> "Tprint"
  | Tint -> "Tint"
  | Tadd -> "Tadd"
  | Tmul -> "Tmul"
  | Tdiv -> "Tdiv"
  | Tmod -> "Tmod"
  | Txor -> "Txor"
  | Tsub -> "Tsub"
  | Tclt -> "Tclt"
  | Tcgt -> "Tcgt"
  | Tcle -> "Tcle"
  | Tcge -> "Tcge"
  | Tceq -> "Tceq"
  | Tne -> "Tne"
  | Tneg -> "Tneg"
  | Tlistglobdef -> "Tlistglobdef"
  | Tfundef -> "Tfundef"
  | Tfunname -> "Tfunname"
  | Tfunargs -> "Tfunargs"
  | Tfunbody -> "Tfunbody"
  | Tassignvar -> "Tassignvar"
  | Targ -> "Targ"


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

let tag_style = function
  | Tlistglobdef | Tfundef | Tfunname | Tfunargs | Tfunbody | Tblock | Targ ->
    ("structure", "#6d28d9", "#f5f3ff", "#c4b5fd")
  | Tassign | Tif | Twhile | Treturn | Tprint | Tassignvar ->
    ("statement", "#0369a1", "#eff6ff", "#93c5fd")
  | Tadd | Tmul | Tdiv | Tmod | Txor | Tsub | Tclt | Tcgt | Tcle | Tcge
  | Tceq | Tne | Tneg ->
    ("operator", "#be185d", "#fff1f2", "#fda4af")
  | Tint ->
    ("literal", "#b45309", "#fffbeb", "#fcd34d")

let tag_node root tag =
  let category, text_color, fill_color, border_color = tag_style tag in
  Format.sprintf
    "shape=box style=\"rounded,filled\" fillcolor=\"%s\" color=\"%s\" penwidth=\"%s\" margin=\"0.15,0.10\" label=<<FONT POINT-SIZE=\"9\" COLOR=\"%s\">%s</FONT><BR/><B><FONT POINT-SIZE=\"11\">%s</FONT></B>>"
    fill_color (if root then "#2563eb" else border_color)
    (if root then "2.2" else "1.15") text_color
    (String.uppercase_ascii category) (escape_graphviz_html (string_of_tag tag))

let leaf_node category value text_color fill_color border_color =
  Format.sprintf
    "shape=box style=\"rounded,filled\" fillcolor=\"%s\" color=\"%s\" penwidth=\"1.1\" margin=\"0.14,0.09\" label=<<FONT POINT-SIZE=\"8\" COLOR=\"%s\">%s</FONT><BR/><FONT FACE=\"DejaVu Sans Mono\" POINT-SIZE=\"11\">%s</FONT>>"
    fill_color border_color text_color category (escape_graphviz_html value)

(* Writes a .dot file which corresponds to an AST. *)
let rec draw_ast ?(root=true) a next =
  match a with
  | Node (t, l) ->

    let (code, nodes, next) =
      List.fold_left (fun (code, nodes, nextnode) n ->
          let (node, next, ncode) = draw_ast ~root:false n nextnode in
          (code @ ncode, node::nodes, next)
        ) ([], [], next)
        l in
    (next, next+1, code @ [
         Format.sprintf "n%d [%s];\n" next (tag_node root t)
       ] @ List.map (fun n ->
        Format.sprintf "n%d -> n%d;\n" next n
      ) (List.rev nodes))

  | StringLeaf s ->
    (next, next+1, [Format.sprintf "n%d [%s];\n" next
                       (leaf_node "TEXT" s "#0369a1" "#f0f9ff" "#7dd3fc")])
  | IntLeaf i ->
    (next, next+1, [Format.sprintf "n%d [%s];\n" next
                       (leaf_node "INTEGER" (string_of_int i)
                          "#b45309" "#fffbeb" "#fcd34d")])
  | NullLeaf ->
    (next, next+1, [Format.sprintf
                       "n%d [shape=box style=\"rounded,dashed,filled\" fillcolor=\"#f8fafc\" color=\"#94a3b8\" fontcolor=\"#64748b\" label=\"empty\"];\n"
                       next])
  | CharLeaf i ->
    (next, next+1, [Format.sprintf "n%d [%s];\n" next
                       (leaf_node "CHARACTER" (Format.sprintf "%C" i)
                          "#047857" "#ecfdf5" "#6ee7b7")])

let draw_ast_tree oc ast =
  let (_, _, s) = draw_ast ast 1 in
  let s = String.concat "" s in
  Format.fprintf oc
    "digraph AST {\n graph [bgcolor=\"transparent\" pad=\"0.25\" nodesep=\"0.22\" ranksep=\"0.52\" ordering=\"out\" fontname=\"DejaVu Sans\"];\n node [fontname=\"DejaVu Sans\"];\n edge [color=\"#94a3b8\" arrowsize=\"0.62\" penwidth=\"1.15\"];\n%s}\n"
    s

let rec string_of_ast a =
  match a with
  | Node (t, l) ->
    Format.sprintf "Node(%s,%s)" (string_of_tag t)
      (String.concat ", " (List.map string_of_ast l))
  | StringLeaf s -> Format.sprintf "\"%s\"" s
  | IntLeaf i -> Format.sprintf "%d" i
  | CharLeaf i -> Format.sprintf "%c" i
  | NullLeaf -> "null"
