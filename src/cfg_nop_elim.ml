open Prog
open Utils
open Cfg
open Report
open Options
module Set = Collections.IntSet

(* Elimination of NOPs.*)

(* [nop_transitions cfg] gives the list of NOP transitions in a CFG.

   If node [n] contains [Cnop s], then [(n,s)] should be in the result.
*)
let nop_transitions (cfgfunbody: (int, cfg_node) Hashtbl.t) : (int * int) list =
   (* TODO *)
   []


(* [follow n l visited] gives the first successor reachable from [n] that is
   not a NOP. To find the successor of a NOP node, we use the list
   [l] as produced previously. As a reminder [(x,y)] in [l] means
   that there is a transition from a node [x] which contains an instruction [Cnop
   y].

   The [visited] set is used to avoid loops.
   *)
let rec follow (n: int) (l: (int * int) list) (visited: Set.t) : int =
   (* TODO *)
   n

(* [nop_transitions_closed] contains pairs [(n,s)] such that node [n] starts a
   chain of NOPs ending at node [s]. The course instructors are happy to
   provide this function. *)
let nop_transitions_closed cfgfunbody =
  List.map (fun (node_id, node) ->
      (node_id, follow node_id (nop_transitions cfgfunbody) Set.empty))
    (nop_transitions cfgfunbody)

(* We will now rewrite our program to replace the
   successors [s] of each node of the CFG in the following way: if [s] is the
   beginning of a chain of NOPs, we replace [s] with the end of that chain,
   thereby eliminating the NOP nodes. *)

(* [replace_succ nop_succs s] gives the new name of the node [s], using the
   list [nop_succs] (as returned by [nop_transitions_closed]).*)
let replace_succ nop_succs s =
   (* TODO *)
   s

(* [replace_succs nop_succs n] replaces node [n] with an equivalent node whose
   successors have been replaced using [nop_succs]. *)
let replace_succs nop_succs (n: cfg_node) =
   (* TODO *)
   n

(* [nop_elim_fun f] applies the [replace_succs] function to each node in the CFG.*)
let nop_elim_fun ({ cfgfunargs; cfgfunbody; cfgentry } as f: cfg_fun) =
  let nop_transf = nop_transitions_closed cfgfunbody in
  (* We use the [Collections.hashtbl_filter_map f h] function which allows you to apply a
     function at each node of [h] and eliminate those for which [f] returns
     [None].

     We want to eliminate nodes that have no predecessors
     (inaccessible), and apply the [replace_succs] function to the nodes which
     will remain.
  *)
  let cfgfunbody = Collections.hashtbl_filter_map (fun n node ->
         (* TODO *)
         Some node
    ) cfgfunbody in
  (* The returned function contains the new [cfgfunbody] computed above, and
     its entry point is transformed accordingly. *)
  {f with cfgfunbody; cfgentry = replace_succ nop_transf cfgentry }

let nop_elim_gdef gd =
  match gd with
    Gfun f -> Gfun (nop_elim_fun f)

let nop_elimination cp =
  if !Options.no_cfg_ne
  then cp
  else assoc_map nop_elim_gdef cp

let pass_nop_elimination cfg =
  let cfg = nop_elimination cfg in
  record_compile_result "NopElim";
  OK cfg
