
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


(* Writes a .dot file which corresponds to an AST*)
let rec draw_ast a next =
  match a with
  | Node (t, l) ->

    let (code, nodes, next) =
      List.fold_left (fun (code, nodes, nextnode) n ->
          let (node, next, ncode) = draw_ast n nextnode in
          (code @ ncode, node::nodes, next)
        ) ([], [], next)
        l in
    (next, next+1, code @ [
         Format.sprintf "n%d [label=\"%s\"]\n" next (string_of_tag t)
       ] @ List.map (fun n ->
        Format.sprintf "n%d -> n%d\n" next n
      )nodes)

  | StringLeaf s ->
    (next, next+1, [         Format.sprintf "n%d [label=\"%s\"]\n" next s])
  | IntLeaf i ->
    (next, next+1, [         Format.sprintf "n%d [label=\"%d\"]\n" next i])
  | NullLeaf ->
    (next, next+1, [         Format.sprintf "n%d [label=\"null\"]\n" next])
  | CharLeaf i ->
    (next, next+1, [         Format.sprintf "n%d [label=\"%c\"]\n" next i])

let draw_ast_tree oc ast =
  let (_, _, s) = draw_ast ast 1 in
  let s = String.concat "" s in
  Format.fprintf oc "digraph G{\n%s\n}\n" s

let rec string_of_ast a =
  match a with
  | Node (t, l) ->
    Format.sprintf "Node(%s,%s)" (string_of_tag t)
      (String.concat ", " (List.map string_of_ast l))
  | StringLeaf s -> Format.sprintf "\"%s\"" s
  | IntLeaf i -> Format.sprintf "%d" i
  | CharLeaf i -> Format.sprintf "%c" i
  | NullLeaf -> "null"
