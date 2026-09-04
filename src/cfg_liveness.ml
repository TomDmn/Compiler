open Cfg
module Set = Collections.StringSet

(* Liveness analysis*)

(* [vars_in_expr e] returns the set of variables that appear in [e].*)
let rec vars_in_expr (e: expr) =
   (* TODO *)
   Set.empty

(* [live_after_node cfg n] returns all live variables after the
   node [n] in a CFG [cfg]. [lives] is the current state of the analysis,
   that is to say a table whose keys are node identifiers of the CFG and
   the values are the sets of live variables before each node.*)
let live_after_node cfg n (lives: (int, Set.t) Hashtbl.t) : Set.t =
   (* TODO *)
   Set.empty

(* [live_cfg_node node live_after] returns all live variables
   before a node [node], given the set [live_after] of variables
   alive after this node.*)
let live_cfg_node (node: cfg_node) (live_after: Set.t) =
   (* TODO *)
   live_after

(* [live_cfg_nodes cfg lives] performs one iteration of the fixed point calculation.

   This function updates the current analysis state [lives] and returns a
   boolean indicating whether the computation progressed during this iteration
   (i.e. whether the set of live variables before at least one node changed). *)
let live_cfg_nodes cfg (lives : (int, Set.t) Hashtbl.t) =
   (* TODO *)
   false

(* [live_cfg_fun f] calculates the set of live variables before each node
   of the CFG by iterating [live_cfg_nodes] until a fixed point is reached.
   *)
let live_cfg_fun (f: cfg_fun) : (int, Set.t) Hashtbl.t =
  let lives = Hashtbl.create 17 in
     (* TODO *)
 lives
