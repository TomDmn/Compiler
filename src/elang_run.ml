open Elang
open Prog
open Utils

let binop_bool_to_int f x y = if f x y then 1 else 0

(* [eval_binop b x y] evaluates the binary operation [b] on the arguments [x]
   and [y].*)
let eval_binop (b: binop) : int -> int -> int =
  match b with
   | _ -> fun x y -> 0

(* [eval_unop u x] evaluates the unary operation [u] on the argument [x].*)
let eval_unop (u: unop) : int -> int =
  match u with
   | _ -> fun x -> 0

(* [eval_eexpr st e] evaluates the expression [e] in state [st]. Returns a
   error if necessary.*)
let rec eval_eexpr st (e : expr) : int res =
   Error "eval_eexpr not implemented yet."

(* [eval_einstr oc st ins] evaluates instruction [ins] starting from state [st].

   The [oc] parameter is an "output channel", in which the "print" function
   writes its output, using the [Format.fprintf] instruction.

   This function returns [(ret, st')]:

   - [ret] is of type [int option]. [Some v] should be returned when a
   [return] instruction is evaluated. [None] means that no [return] occurred
   and execution must continue.

   - [st'] is the updated state.*)
let rec eval_einstr oc (st: int state) (ins: instr) :
  (int option * int state) res =
   Error "eval_einstr not implemented yet."

(* [eval_efun oc st f fname vargs] evaluates the function [f] (whose name is
   [fname]) starting from the state [st], with the arguments [vargs].

   This function returns a pair (ret, st') with the same meaning as
   for [eval_einstr].*)
let eval_efun oc (st: int state) ({ funargs; funbody}: efun)
    (fname: string) (vargs: int list)
  : (int option * int state) res =
  (* The environment of a function (mapping local variables to their
     values) is local and a function call should not modify the
     caller variables. So, we save the caller's environment
     in [env_save], call the function in a clean environment (with
     only its arguments), then we restore the caller's environment.*)
  let env_save = Hashtbl.copy st.env in
  let env = Hashtbl.create 17 in
  match List.iter2 (fun a v -> Hashtbl.replace env a v) funargs vargs with
  | () ->
    eval_einstr oc { st with env } funbody >>= fun (v, st') ->
    OK (v, { st' with env = env_save })
  | exception Invalid_argument _ ->
    Error (Format.sprintf
             "E: Called function %s with %d arguments, expected %d.\n"
             fname (List.length vargs) (List.length funargs)
          )

(* [eval_eprog oc ep memsize params] evaluates a complete program [ep], with the
   [params] arguments.

   The [memsize] parameter gives the amount of memory available to the program.
   This is not immediately useful (our programs do not use
   memory), but it will be when we add dynamic allocation in
   our programs.

   Returns:

   - [OK (Some v)] when the function evaluates successfully and returns a value [v].

   - [OK None] when the function finishes without returning a value.

   - [Error msg] when an error occurs.
   *)
let eval_eprog oc (ep: eprog) (memsize: int) (params: int list)
  : int option res =
  let st = init_state memsize in
  find_function ep "main" >>= fun f ->
  (* only keeps the necessary number of parameters for the "main" function.*)
  let n = List.length f.funargs in
  let params = take n params in
  eval_efun oc st f "main" params >>= fun (v, _) ->
  OK v
