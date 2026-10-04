open Symbols
open Utils
open E_regexp
module Set = Collections.IntSet
module CharSet = Collections.CharSet

(* Non-deterministic Finite Automata (NFA) *)

(* The states of an NFA [nfa_state] are integers.

   An NFA is modeled as a record with the following four fields:
   - [nfa_states] contains the list of automaton states.
   - [nfa_initial] contains the list of initial states of the automaton.
   - [nfa_final] contains the list of final states of the automaton in the
     form (q, t), where q is a state of the automaton, and t, of type
     [string -> token option] is a function that constructs a token
     from a string.
   - [nfa_step q] gives the list of transitions from state [q] as a list
     [(charset, q')]. [charset] is the set of characters that enable the
     transition to state [q']. It may be [None], indicating an epsilon
     transition.
*)

type nfa_state = int

type nfa =
  {
    nfa_states: nfa_state list;
    nfa_initial: nfa_state list;
    nfa_final: (nfa_state * (string -> token option)) list;
    nfa_step: nfa_state -> (CharSet.t option * nfa_state) list
  }

(* [empty_nfa] is an empty NFA.*)
let empty_nfa =
  {
    nfa_states = [];
    nfa_initial = [];
    nfa_final = [];
    nfa_step = fun q -> [];
  }

(* Concatenation of NFAs.*)
let cat_nfa n1 n2 =
  {
    nfa_states = n1.nfa_states @ n2.nfa_states;
    nfa_initial = n1.nfa_initial;
    nfa_final = n2.nfa_final;

    nfa_step = fun q -> 
      let normal_states = n1.nfa_step q @ n2.nfa_step q in (*normal transitions from the initial NFA*)

      if List.mem_assoc q n1.nfa_final (* if q is in the final states of n1*)
        then normal_states @ List.map (fun s -> (None, s)) n2.nfa_initial(*then we add the possibility to do an esp transition toward the initial states of n2*)
      else normal_states;
  }
(* Alternation of NFAs *)
let alt_nfa n1 n2 =
  {
    nfa_states = n1.nfa_states @ n2.nfa_states;
    nfa_initial = n1.nfa_initial @ n2.nfa_initial;
    nfa_final = n1.nfa_final @ n2.nfa_final;
    nfa_step = fun q -> n1.nfa_step q @ n2.nfa_step q
  }

(* Repetition of NFAs*)
(* t is of type [string -> token option]*)
let star_nfa n t =
  {
    nfa_states = n.nfa_states;
    nfa_initial = n.nfa_initial;
    nfa_final = List.map (fun q -> (q, t)) (n.nfa_initial @ List.map fst n.nfa_final); (*we add the initials states to the final states for the 0 repition case, then we add the t fct*)
    nfa_step = fun q -> 
      if List.mem_assoc q n.nfa_final (* if q is in the final states of n*)
        then (n.nfa_step q) @ List.map (fun s -> (None, s)) n.nfa_initial(*then we add the possibility to do an esp transition toward the initial states of n*)
      else n.nfa_step q;
  }


(* [nfa_of_regexp r freshstate t] constructs an NFA that recognizes the same
   language as the regular expression [r].
   [freshstate] corresponds to an integer for which there is no state yet in
   the nfa. Just increment [freshstate] to get new unused states.
   [t] is a function of type [string -> token option] useful for final states.
*)
let rec nfa_of_regexp r freshstate t =
  match r with
  | Eps -> { nfa_states = [freshstate];
             nfa_initial = [freshstate];
             nfa_final = [(freshstate,t)];
             nfa_step = fun q -> []}, freshstate + 1
  | Charset c -> { nfa_states = [freshstate; freshstate + 1];
                nfa_initial = [freshstate];
                nfa_final = [freshstate + 1, t];
                nfa_step = fun q -> if q = freshstate then [(Some c, freshstate + 1)] else []
              }, freshstate + 2
  | Cat (r1, r2) -> let temp_n1, temp_freshstate1 = (nfa_of_regexp r1 freshstate t) in       (*We translate r1, without forgetting to increment freshstate*)
                    let temp_n2, temp_freshstate2 = (nfa_of_regexp r2 temp_freshstate1 t) in  (*same for r2*)
                    cat_nfa temp_n1 temp_n2, temp_freshstate2                                 (*Then we use our previous function to concatenate them*)
  | Alt (r1, r2) -> let temp_n1, temp_freshstate1 = (nfa_of_regexp r1 freshstate t) in
                    let temp_n2, temp_freshstate2 = (nfa_of_regexp r2 temp_freshstate1 t) in
                    alt_nfa temp_n1 temp_n2, temp_freshstate2
  | Star r1 -> let temp_n, temp_freshstate = (nfa_of_regexp r1 freshstate t) in
               star_nfa temp_n t, temp_freshstate

(* Deterministic Finite Automaton (DFA) *)

(* The states of a DFA [dfa_state] are sets of integers.

   Like an NFA, a DFA is modeled as a record with the following four fields:

   - [dfa_states] contains the list of automaton states.
   - [dfa_initial] contains the initial state of the automaton.
   - [dfa_final] contains the list of final states of the automaton in the
     form (q, t), where q is a state of the automaton, and t, of type
     [string -> token option] is a function that constructs a token
     from a string.
   - [dfa_step q c] gives the state [q'] accessible after reading the character
     [c], from state [q]. [charset] can optionally be [None], which
     indicates that no transition is possible from this state, and with this
     character.
*)

type dfa_state = Set.t

type dfa =
  {
    dfa_states: dfa_state list;
    dfa_initial: dfa_state;
    dfa_final: (dfa_state * (string -> token option)) list;
    dfa_step: dfa_state -> char -> dfa_state option
  }

(* We will now determinize the NFA to obtain a DFA. *)


(* [epsilon_closure] calculates the epsilon-closure of a state [s] in an NFA [n],
   that is, the set of states reachable from [s] using only epsilon
   transitions. *)
let epsilon_closure (n: nfa) (s: nfa_state) : Set.t =
  let rec traversal (visited: Set.t) (s: nfa_state) : Set.t =
    if Set.mem s visited then       (*If s is already in visited we stop*)
      visited
    else
      let visited = Set.add s visited in (*Else we add s in visited*)
      
      (*We only keep the state in which we can advance from s with an esp transition*)
      let eps_step = List.filter_map (fun (transition, destination) -> if transition = None then Some destination else None) (n.nfa_step s) in

      List.fold_left (fun acc q -> traversal acc q) visited eps_step (*Then we do this to all the state we gathered since the beginning*)
  in
  traversal Set.empty s

(* [epsilon_closure_set n ls] computes the union of the epsilon closures of all
   NFA states in [ls]. *)
let epsilon_closure_set (n: nfa) (ls: Set.t) : Set.t =
   Set.fold (fun q acc -> Set.union acc (epsilon_closure n q)) ls Set.empty
   

(* [dfa_initial_state n] computes the initial state of the determinized
   automaton. *)
let dfa_initial_state (n: nfa) : dfa_state =
   epsilon_closure_set n (Set.of_list n.nfa_initial)

(* Construction of the DFA automaton transition table.*)

(* As seen in class, to build the DFA automaton table from
   the NFA [n], we start from a state [q] of the automaton (initially, the
   initial state computed above).

   We calculate the set [t] of transitions in [n] of each of the states of
   [q]. This set is of type [(char set option * nfa_state) list].

   We transform this set [t] in the following way:
   - we throw away the epsilon-transitions: [assoc_throw_none]
   - we transform each transition ({c1,c2,..,cn}, q) into a list of
     transitions [(c1,q); (c2,q); ...; (cn,q)]: [assoc_distribute_key]
   - we merge the transitions which consume the same character:
     [(c1,q1);(c1,q2);...;(c1,qn);(c2,q'1);...(c2,q'm)] ->
     [(c1,{q1,q2,...,qn});(c2,{q'1,...,q'm})]: [assoc_merge_vals]
   - we apply epsilon-closure on all states:
     [(c1,{q1,q2,...,qn});...;(cn,{qn}])] -> [(c1, eps({q1,q2,...,qn})); ...; (cn, eps({qn}))]:
     [epsilon_closure_set]

   We then obtain all the transitions from the state [q] in
   the DFA automaton.

   We repeat this process for all the new states we reach.
*)

let assoc_throw_none (l : ('a option * 'b) list) : ('a * 'b) list =
  List.filter_map (fun (o,n) ->
      match o with
        None -> None
      | Some x -> Some (x,n)
    ) l

let assoc_distribute_key (l : (CharSet.t * 'a) list) : (char * 'a) list =
  List.fold_left (fun (acc : (char * 'a) list) (k, v) ->
      CharSet.fold (fun c acc -> (c, v)::acc) k acc)
    [] l

let assoc_merge_vals (l : ('a * nfa_state) list) : ('a * Set.t) list =
  List.fold_left (fun (acc : ('a * Set.t) list) (k, v) ->
      match List.assoc_opt k acc with
      | None -> (k, Set.singleton v)::acc
      | Some vl -> (k, Set.add v vl)::List.remove_assoc k acc
    ) [] l

let rec build_dfa_table (table: (dfa_state, (char * dfa_state) list) Hashtbl.t)
    (n: nfa)
    (ds: dfa_state) : unit =
  match Hashtbl.find_opt table ds with
  | Some _ -> ()
  | None ->
    (* [transitions] contains the constructed DFA transitions
     * from the NFA transitions as described previously*)
    let transitions : (char * dfa_state) list =
      let all_steps = Set.fold (fun q acc -> n.nfa_step q @ acc) ds [] in  (* We collect all transitions that start from a state in ds*)
      let merged = assoc_merge_vals (assoc_distribute_key (assoc_throw_none all_steps)) in (* We do the next operation of the determinization*)
      List.map (fun (c, states) -> (c, epsilon_closure_set n states)) merged (*then the epsilon closure*)
    in
    Hashtbl.replace table ds transitions;
    List.iter (build_dfa_table table n) (List.map snd transitions)

(* Calculation of the final states of the DFA automaton*)

(* As seen in class, a DFA state [q] is final if and only if it contains a state
   [q'] that is final in the NFA.

   We must also compute the token recognized by each final state.

   Suppose we have two final states [q1, fun s -> SYM_IDENTIFIER s] and
   [q2, fun s -> SYM_WHILE] in our NFA.
   State [q = {q1,q2}] is final, but which token should it recognize?

   In this case, we want to recognize the keyword 'while' rather than an
   arbitrary identifier.

   More generally, we introduce a priority function to choose between tokens.
   The [priority : token -> int] function
   gives a smaller value to the highest priority tokens.

*)

let priority t =
  match t with
  | SYM_EOF -> 100
  | SYM_IDENTIFIER _ -> 50
  | _ -> 0

(* [min_priority l] returns the token of [l] that has the lowest priority, or
   [None] if the [l] list is empty.*)
let min_priority (l: token list) : token option =
  match l with
  | [] -> None
  | x :: a -> 
    Some (List.fold_left
        (fun min_token token ->
          if priority token < priority min_token
          then token
          else min_token) x a) 
        

(* [dfa_final_states n dfa_states] returns the list of final DFA states,
   accompanied by the token they recognize.*)
let dfa_final_states (n: nfa) (dfa_states: dfa_state list) :
  (dfa_state * (string -> token option)) list  =
  List.filter_map (fun q -> (* We only keep the dfa states that are final *)
      let q_finals = Set.fold (fun s acc -> (* We go through every nfa state s inside the dfa state q *)
          match List.assoc_opt s n.nfa_final with (* We check if s is a final state of the nfa *)
          | Some f -> f :: acc (* If it is, we keep its function (there can be several of them in q) *)
          | None -> acc) q [] in (* Else we ignore it *)
      match q_finals with
      | [] -> None (* No nfa final state inside q, so q is not a final state of the dfa *)
      | _ -> Some (q, fun w -> min_priority (List.filter_map (fun f -> f w) q_finals)) (* Else q is final : we apply every function to the word w and keep the token with the highest priority *)
    ) dfa_states

(* Construction of the DFA transition relationship.*)

(* [make_dfa_step table] constructs the DFA transition function, where [table]
   is the table generated by [build_dfa_table], defined above.*)
let make_dfa_step (table: (dfa_state, (char * dfa_state) list) Hashtbl.t) =
  fun (q: dfa_state) (a: char) ->
   (* TODO *)
   None

(* Finally, we assemble all these pieces to build the automaton. The
   function [dfa_of_nfa n] is provided. *)
let dfa_of_nfa (n: nfa) : dfa =
  let table : (dfa_state, (char * dfa_state) list) Hashtbl.t =
    Hashtbl.create (List.length n.nfa_states) in
  let dfa_initial = dfa_initial_state n in
  build_dfa_table table n dfa_initial;
  let dfa_states = Hashtbl.to_seq_keys table |> List.of_seq in
  let dfa_final = dfa_final_states n dfa_states in
  let dfa_step = make_dfa_step table in
  {
    dfa_states  ;
    dfa_initial ;
    dfa_final   ;
    dfa_step    ;
  }

(* Lexical analysis *)

(* Now that everything is in place, we will be able to write a lexical analyzer,
   which will split our source program into a list of tokens.*)

(* The [tokenize_one d w] function attempts to match the largest prefix
   possible from [w]. It returns a pair [(res,w')], where [res] is the result
   of the lexical analysis of a word and [w'] is the rest of the program to analyze.

   The result is of type [lexer_result], defined below:
   - [LRToken tok] indicates that the automaton has recognized the [tok] token
   - [LRskip] indicates that the automaton has recognized a word which does not generate a token
     (this is the case, for example, for spaces, tabs, newlines and
     comments)
   - [LRerror] indicates that the automaton has not recognized anything at all,
     and therefore reports an error.

*)

type lexer_result =
  | LRtoken of token
  | LRskip
  | LRerror

(* The [tokenize_one] function uses an internal function [recognize q w
   current_word last_accepted] which tries to read the largest prefix of [w]
   recognized by the automaton.

   - [q] is the current state of the automaton.
   - [w] is the rest of the source program to analyze.
   - [current_word] is the word recognized from the initial state of the automaton.
   - [last_accepted] is of type [lexer_result * char list]. The first
     component is the last valid result of the analyzer: that towards
     which we will fall back on when we are stuck in a non-final state of
     the automaton. The second component is the rest of the program to analyze,
     after this last recognized token.

   The recognize function is launched with [q = d.dfa_initial], the initial state of the
   DFA, the program to analyze [w], an empty current word, and a last state
   accepted denoting an error (if we do not go through any final state, it is
   indeed a lexical error).

*)

let tokenize_one (d : dfa) (w: char list) : lexer_result * char list =
  let rec recognize (q: dfa_state) (w: char list)
      (current_token: char list) (last_accepted: lexer_result * char list)
    : lexer_result * char list =
         (* TODO *)
         last_accepted
  in
  recognize d.dfa_initial w [] (LRerror, w)

(* [tokenize_all d w] repeatedly applies [tokenize_one] until it reaches the end
   of the file (token [SYM_EOF]). This function is provided. *)
let rec tokenize_all (d: dfa) (w: char list) : (token list * char list) =
  match tokenize_one d w with
  | LRerror, w -> [], w
  | LRskip, w -> tokenize_all d w
  | LRtoken token, w ->
    let (tokens, w) =
      if token = SYM_EOF
      then ([], w)
      else tokenize_all d w in
    (token :: tokens, w)



(* Display Functions - Useful for debugging*)


let char_list_to_char_ranges s =
  let rec recognize_range (cl: int list) l opt_c n =
    match cl with
    | [] -> (match opt_c with
          None -> l
        | Some c -> (c,n)::l
      )
    | c::r -> (match opt_c with
        | None -> recognize_range r l (Some c) 0
        | Some c' ->
          if c' + n + 1 = c
          then recognize_range r l (Some c') (n + 1)
          else recognize_range r ((c',n)::l) (Some c) 0
      )
  in
  let l = recognize_range (List.sort Stdlib.compare (List.map Char.code s)) [] None 0 in
  let escape_char c =
    if c = '"' then "\\\""
    else if c = '\\' then "\\\\"
    else if c = '\x00' then "\\\\0"
    else if c = '\t' then "\\\\t"
    else if c = '\n' then "\\\\n"
    else Printf.sprintf "%c" c in
  List.fold_left (fun acc (c,n) ->
      match n with
      | 0 -> Printf.sprintf "%s%s" (escape_char (Char.chr c)) acc
      | 1 -> Printf.sprintf "%s%s%s" (escape_char (Char.chr c)) (c + 1 |> Char.chr |> escape_char) acc
      | _ -> Printf.sprintf "%s-%s%s" (escape_char (Char.chr c))
          (escape_char (Char.chr (c + n))) acc
    ) "" l


(* Displaying an NFA*)
let nfa_to_string (n : nfa) : string =
  Printf.sprintf "===== NFA\nStates : %s\nInitial states : %s\nFinal states : %s\n%s"
    (String.concat " " (List.map (fun q -> string_of_int q) n.nfa_states))
    (String.concat " " (List.map (fun q -> string_of_int q) n.nfa_initial))
    (String.concat " " (List.map (fun (q,_) -> string_of_int q) n.nfa_final)) 
    (String.concat ""
       (List.map (fun q ->
            let l = n.nfa_step q in
            String.concat ""
              (List.map (fun (oa, q') ->
                   Printf.sprintf "step(%d, %s) = [%d]\n" q (match oa with Some a -> Printf.sprintf "[%s]" (string_of_char_set a) | _ -> "eps")
                     q'
                 ) l)
          ) n.nfa_states))

let nfa_to_dot oc (n : nfa) : unit =
  Printf.fprintf oc "digraph {\n";
  List.iter (fun n -> Printf.fprintf oc "N%d [shape=\"house\" color=\"red\"]\n" n) (n.nfa_initial);
  List.iter (fun (q,t) ->
      Printf.fprintf oc "N%d [shape=\"rectangle\", label=\"%s\"]\n"
        q (match t "0" with | Some s -> string_of_symbol s | None -> "" )) n.nfa_final;
  List.iter (fun q ->
      List.iter (fun (cso, q') ->
          match cso with
          | None ->
            Printf.fprintf oc "N%d -> N%d [label=\"[epsilon]\"]\n" q q'
          | Some cs ->
            Printf.fprintf oc "N%d -> N%d [label=\"[%s]\"]\n" q q'
              (char_list_to_char_ranges (CharSet.to_list cs))
        ) (n.nfa_step q);
    ) n.nfa_states;
  Printf.fprintf oc "}\n"


(* Displaying a DFA *)
let dfa_to_string (n : dfa) (alphabet: char list): string =
  Printf.sprintf "===== DFA\nStates : %s\nInitial state : %s\nFinal states : [%s]\n%s"
    (String.concat " " (List.map (fun q -> string_of_int_set q) n.dfa_states))
    (string_of_int_set n.dfa_initial)
    (String.concat " " (List.map (fun (q,_) -> string_of_int_set q) n.dfa_final))
    (String.concat "" (List.map (fun q ->
         String.concat "" (List.map (fun a ->
             let l = n.dfa_step q a in
             match l with
             | None -> ""
             | Some q' ->
               if not (Set.is_empty q') then
                 Printf.sprintf "step(%s, %c) = %s\n"
                   (string_of_int_set q)
                   a (string_of_int_set q')
               else ""
           ) alphabet);
       ) n.dfa_states))

(* Graphical display of a DFA. Generates a .dot file that can then be converted
   to PDF with 'dot file.dot -Tsvg -o file.svg', or by copying the DOT code into
   an online converter (for example:
   http://proto.informatics.jax.org/prototypes/dot2svg/). *)

let dfa_to_dot oc (n : dfa) (cl: char list): unit =
  Printf.fprintf oc "digraph {\n";
  Printf.fprintf oc "N%s [shape=\"house\" color=\"red\"]\n" (string_of_int_set n.dfa_initial);
  List.iter (fun (q,t) ->
      Printf.fprintf oc "N%s [shape=\"rectangle\", label=\"%s\"]\n"
        (string_of_int_set q) (match t "0" with | Some s -> string_of_symbol s | None -> "" )) n.dfa_final;
  List.iter (fun q ->
      let l = List.fold_left (fun l a ->
          match n.dfa_step q a with
            None -> l
          | Some q' ->
            match List.assoc_opt q' l with
            | None -> (q', [a])::l
            | Some ql -> (q', a::ql)::List.remove_assoc q' l
        ) [] cl in
      List.iter (fun (q', cl) ->
          Printf.fprintf oc "N%s -> N%s [label=\"[%s]\"]\n"
            (string_of_int_set q)
            (string_of_int_set q') (char_list_to_char_ranges cl)
        ) l;
    ) n.dfa_states;
  Printf.fprintf oc "}\n"

let nfa_of_list_regexp l =
  let (n, fs) = List.fold_left (fun (nfa, fs) (r,t) ->
      let n,fs = nfa_of_regexp r fs t in
      (alt_nfa nfa n, fs)
    ) ({ nfa_states = []; nfa_initial = []; nfa_final = []; nfa_step = fun _ -> [] },1)
      l in n

let dfa_of_list_regexp l =
  let n = nfa_of_list_regexp l in
  dfa_of_nfa n

let tokenize_list_regexp l s =
  let d = dfa_of_list_regexp l in
  let tokens, leftover = tokenize_all d (char_list_of_string s) in
  if leftover <> []
  then Error (Printf.sprintf "Lexer failed to recognize string starting with '%s'\n"
                   (string_of_char_list (take 20 leftover))
                )
  else OK tokens

let file_contents file =
  let ic = open_in file in
  let rec aux s () =
    try
      let line = input_line ic in  (* read line from in_channel and discard \n *)
      aux (s ^ line ^ "\n") ()   (* close the input channel *)
    with e ->                      (* some unexpected exception occurs *)
      close_in_noerr ic;           (* emergency closing *)
      s in
  aux "" ()


let tokenize_file f =
  tokenize_list_regexp list_regexp (file_contents f)
