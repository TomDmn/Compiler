open Cfg
open Prog
open Utils
open Cfg_liveness
open Report
open Options
module Set = Collections.StringSet

(* Dead Assign Elimination -- Elimination of dead assignments*)

(* [dead_assign_elimination_fun f] eliminates dead assignments in the
   function [f]. This function returns a pair [(f',c)] where [f'] is the
   new function, and [c] indicates whether any progress was made. *)
let dead_assign_elimination_fun ({ cfgfunbody; _ } as f: cfg_fun) =
  let changed = ref false in
  let cfgfunbody =
    Collections.hashtbl_map (fun (n: int) (m: cfg_node) ->
        match m with
           (* TODO *)
        | _ -> m
      ) cfgfunbody in
  ({ f with cfgfunbody }, !changed )

(* Applies dead code elimination as many times as necessary. Test
   in particular on the test file [basic/useless_assigns.e].*)
let rec iter_dead_assign_elimination_fun f =
  let f, c = dead_assign_elimination_fun f in
   (* TODO *)
   f

let dead_assign_elimination_gdef = function
    Gfun f -> Gfun (iter_dead_assign_elimination_fun f)

let dead_assign_elimination p =
  if !Options.no_cfg_dae
  then p
  else assoc_map dead_assign_elimination_gdef p

let pass_dead_assign_elimination cfg =
  let cfg = dead_assign_elimination cfg in
  record_compile_result "DeadAssign";
  OK cfg
