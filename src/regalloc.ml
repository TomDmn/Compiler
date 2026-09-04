open Prog
open Linear
open Rtl
open Linear_liveness
open Utils
open Report
open Options
module Set = Collections.IntSet



(* Register allocation *)

(* We allocate registers by coloring an interference graph.

   The goal of the allocator is to associate with each pseudo-register used in
   a Linear function, a location (type [loc]).*)

type loc = Reg of int | Stk of int

(* A location is either a machine register (identified by its number [r]
   between 0 and 31 inclusive), [Reg r], or a stack location [Stk o], meaning
   an offset of [o] bytes relative to the frame pointer in register [s0] (also
   called [fp]). *)

(* We provide you, below, with a naive implementation which evicts all
   pseudo-registers on the stack.*)

let regs_in_instr i =
  Set.union (gen_live i) (kill_live i)

let regs_in_instr_list (l: rtl_instr list) : Set.t =
  List.fold_left
    (fun acc i -> Set.union acc (regs_in_instr i))
    Set.empty l

let regalloc_on_stack_fun (f: linear_fun) : ((reg, loc) Hashtbl.t * int)=
  let allocation = Hashtbl.create 10 in
  let regs = regs_in_instr_list f.linearfunbody in
  let regs = Set.diff regs (Set.of_list f.linearfunargs) in
  let next_stack_slot =
    List.fold_left (fun next_stack_slot r ->
        Hashtbl.replace allocation r (Stk (next_stack_slot));
        next_stack_slot - 1
      ) (-1) (Set.to_list regs) in
  (allocation, next_stack_slot)


(* We will now construct a register interference graph (RIG). Its OCaml type is
   [(reg, Set.t) Hashtbl.t], i.e. a table whose keys are
   registers and values are sets of registers that "interfere"
   with the key register. This corresponds to the adjacency relation in the
   interference graph.*)

(* The [add_to_interf rig x y] function adds [y] to the list of registers that
   interfere with [x] in the [rig] graph.

   We can use the [Collections.hashtbl_modify_default] function which allows us to modify the
   value associated with a key.

   For example, calling [Collections.hashtbl_modify_default def k f rig] modifies the value
   associated with the key [k] in the [rig] graph.

   [f] is a function which takes the old value as input, and which returns
   the new value (type ['b -> 'b], if [rig] is of type [('a,'b)
   Hashtbl.t], i.e. ['b] is the type of the values).

   [def] is the default value given to [f] if there is no old
   value for key [k].

   Interference must exist in both directions: if [x] is in the interference
   set of [y], then [y] must be in the interference set of [x].

*)

let add_interf (rig : (reg, Set.t) Hashtbl.t) (x: reg) (y: reg) : unit =
    (* TODO *)
    ()


(* [make_interf_live rig live] adds edges in the interference graph
   for every pair of registers alive at the same time at a program point.
   *)
let make_interf_live
    (rig: (reg, Set.t) Hashtbl.t)
    (live : (int, Set.t) Hashtbl.t) : unit =
    (* TODO *)
   ()

(* [build_interference_graph live_out] uses the functions written above to
   construct the interference graph from the variables live at the exit of
   each node, as given by [live_out]. This function is provided. *)
let build_interference_graph (live_out : (int, Set.t) Hashtbl.t) code : (reg, Set.t) Hashtbl.t  =
  let interf = Hashtbl.create 17 in
  (* We add a vertex for each variable that appears in the program.*)
  Hashtbl.iter (fun _ s ->
      Set.iter (fun v -> Hashtbl.replace interf v Set.empty) s
    ) live_out;
  make_interf_live interf live_out;
(* Registers that are written to but are never alive should be considered as interfering with all others.*)
  let written_regs = written_rtl_regs code in
  let written_regs_never_live =
    Hashtbl.fold (fun _ regset_live_together acc -> Set.diff acc regset_live_together) live_out
      written_regs in
  let other_regs = Hashtbl.to_seq_keys interf |> Set.of_seq in
  Set.iter (fun r ->
      Set.iter (fun r_other ->
          add_interf interf r r_other
        ) other_regs
    ) written_regs_never_live;
  interf

(* [remove_from_rig rig v] removes vertex [v] from the interference graph
   [rig].*)
let remove_from_rig (rig : (reg, Set.t) Hashtbl.t)  (v: reg) : unit =
   (* TODO *)
   ()


(* Type representing the different decisions that can be taken by
   the register allocator.

   - [Spill r] means that the pseudo-register [r] will be evicted (spilled) on the stack.
    
   - [NoSpill r] means that the pseudo-register [r] will be allocated in a real
   physical register.
*)
type regalloc_decision =
    Spill of reg
  | NoSpill of reg

(* Reminder of the register stacking algorithm*)

(* Once the interference graph has been constructed, we must go through this
   graph in order to color it, with [n] colors. We build a stack of
   [regalloc_decision].

   As long as the graph is not empty:

   - choose a vertex [s] with strictly fewer than [n] neighbors (this will be the
   work of the [pick_node_with_fewer_than_n_neighbors] function), stack the
   decision [NoSpill s] and remove [s] from the graph.

   - if no such vertex exists in the graph, choose a vertex [s]
   corresponding to a register that will be evicted (this will be the work of the
   function [pick_spilling_candidate]). Push decision [Spill s] and remove
   [s] from the graph.

*)

(* [pick_node_with_fewer_than_n_neighbors rig n] chooses a node from graph
   [rig] having strictly fewer than [n] neighbors. It returns [None] if no
   vertex satisfies this condition. *)
let pick_node_with_fewer_than_n_neighbors (rig : (reg, Set.t) Hashtbl.t) (n: int) : reg option =
   (* TODO *)
   None

(* When the previous function fails (i.e. no vertex has fewer than [n]
   neighbors), we choose a pseudo-register to evict.

   One possible heuristic is to evict the pseudo-register with the most
   neighbors in graph [rig].

   [pick_spilling_candidate rig] therefore returns the pseudo-register [r] with
   the most neighbors in [rig], or [None] if [rig] is empty. *)
let pick_spilling_candidate (rig : (reg, Set.t) Hashtbl.t)  : reg option =
   (* TODO *)
   None

(* [make_stack rig stack ncolors] constructs the stack according to the
   algorithm presented in the register-allocation lecture (slide 26 on
   Edunao). *)
let rec make_stack (rig : (reg, Set.t) Hashtbl.t)  (stack : regalloc_decision list) (ncolors: int) : regalloc_decision list =
   (* TODO *)
   stack

(* Now that we have a stack of [regalloc_decision], it's time to
   color the graph, i.e. associate a color (a physical register number) with
   each pseudo-register. We iterate through the stack and handle each decision:

   -  [Spill r]: associate a location on the stack with pseudo-register [r]. We
   will choose the location [next_stack_slot].

   - [NoSpill r]: associate a physical color (a register) with the
   pseudo-register [r]. We will choose a color that is not already associated with a
   neighbor of [r] in [rig].

   This function takes as input:

   - [allocation]: the current allocation, which is updated and used to find
   colors not already assigned to neighbors.

   - [rig]: the interference graph, used to find the neighbors of a register.

   - [all_colors]: the set of colors that can be allocated.

   - [next_stack_slot]: the next available slot on the stack. This
   represents negative offsets relative to fp, so we update it
   by decrementing this value by 1.

   - [decision]: one decision among those stacked.

   This function updates [allocation] and returns the new value of
   [next_stack_slot].

*)
let allocate (allocation: (reg, loc) Hashtbl.t) (rig: (reg, Set.t) Hashtbl.t)
    (all_colors: Set.t)
    (next_stack_slot: int) (decision: regalloc_decision)
  : int =
   (* TODO *)
   next_stack_slot

(* [regalloc_fun f live_out all_colors] performs register allocation for
   the [f] function.

   - [live_out] is a mapping of instruction numbers in the function
   Linear to all live registers after this instruction.

   - [all_colors] is the set of registers that can be used.

   This function returns a triple [(rig, allocation, next_stack_slot)]:

   - [rig] is the interference graph (simply for display)

   - [allocation] is the register allocation that you will have constructed

   - [next_stack_slot] is the next available slot on the stack
   (used in [ltl_gen], which is provided to you.)
*)
let regalloc_fun (f: linear_fun)
    (live_out: (int, Set.t) Hashtbl.t)
    (all_colors: Set.t) :
  (reg, Set.t) Hashtbl.t      (* the RIG *)
  * (reg, loc) Hashtbl.t          (* the allocation *)
  * int                         (* the next stack slot *)
  =
  let rig = build_interference_graph live_out f.linearfunbody in

  let allocation = Hashtbl.create 17 in
  (* The pseudo-registers that contain the arguments are treated separately
     in [ltl_gen.ml]. We therefore remove them from the graph.*)
  List.iter (fun p -> remove_from_rig rig p) f.linearfunargs;
  (* We make a copy [g] of the interference graph [rig]. Indeed, as
     we will delete vertices from the graph, we would otherwise lose the
     interference information needed during coloring. *)
  let g = Hashtbl.copy rig in
  let stack = make_stack g [] (Set.cardinal all_colors) in
  let next_stack_slot =
    List.fold_left (fun next_stack_slot decision ->
        allocate allocation rig all_colors next_stack_slot decision
      ) (-1) stack in
  (rig, allocation, next_stack_slot)


(* [dump_interf_graph fname rig] displays the interferences associated with
   each register. It may be useful for debugging; there is no need to inspect
   this function unless it is buggy. *)
let dump_interf_graph oc (fname, rig, allocation) =
  let colors = Array.of_list [
      "blue"; "red"; "orange"; "pink"; "green"; "purple";
      "brown"; "turquoise"; "gray"; "gold"; "darkorchid"; "bisque";
      "darkseagreen"; "cornsilk"; "burlywood"; "dodgerblue"; "antiquewhite"; "firebrick";
      "deepskyblue"; "darkolivegreen"; "hotpink"; "lightsalmon"; "magenta"; "lawngreen";
    ] in
  let color_of_allocation r =
    match Hashtbl.find_opt allocation r with
    | Some (Reg r) ->
      Array.get colors (r mod Array.length colors)
    | _ -> "white"
  in
  Format.fprintf oc "subgraph cluster_%s{\n" fname;
  Format.fprintf oc "label=\"%s\";\n" fname;
  Hashtbl.to_seq_keys rig |> Seq.iter (fun r ->
      Format.fprintf oc "%s_r%d [label=\"r%d\",style=filled,fillcolor=\"%s\"];\n" fname r r (color_of_allocation r)
    );
  Hashtbl.iter
    (fun i s ->
       Set.iter (fun x ->
           Format.fprintf oc "%s_r%d -> %s_r%d;\n" fname i fname x
         ) s;)
    rig;
  Format.fprintf oc "}\n"

let dump_interf_graphs oc allocations =
  Format.fprintf oc "digraph RIGS {\n";
  Hashtbl.iter (fun fname (rig, allocation, next_stack_slot) ->
      dump_interf_graph oc (fname, rig, allocation)
    ) allocations;
  Format.fprintf oc "}\n"

(* We apply register allocation to the entire Linear program, and we
   displays all of this in the report (the HTML page of each file).*)
let regalloc lp lives all_colors =
  let allocations = Hashtbl.create 17 in
  List.iter (function (fname,Gfun f) ->
      begin match Hashtbl.find_opt lives fname with
      | Some (live_in, live_out) ->
        let (rig, allocation, curstackslot) =
          if !Options.naive_regalloc
          then let (al, nss) = regalloc_on_stack_fun f in
            (Hashtbl.create 0, al, nss)
          else regalloc_fun f live_out all_colors
        in
        Hashtbl.replace allocations fname (rig, allocation, curstackslot)
      | None -> ()
      end
    ) lp;
  dump !Options.rig_dump dump_interf_graphs allocations
    (call_dot "regalloc" "Register Allocation");
  allocations
