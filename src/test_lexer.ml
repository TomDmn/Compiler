open E_regexp
open Lexer_generator
open Utils
open Symbols
module Set = Collections.IntSet
module CharSet = Collections.CharSet

let nfa_accepts (n: nfa) (w: char list) : bool =
  let rec trav vis s =
    if Set.mem s vis then vis
    else let en = List.filter_map (fun (oa, n) -> if oa = None then Some n else None) (n.nfa_step s) in
      List.fold_left trav (Set.add s vis) en in
  let ec s = trav Set.empty s in
  let ecs ls = Set.fold (fun q -> Set.union (ec q)) ls Set.empty in

  let rec walk (q: Set.t) (w: char list) =
    let q = ecs q in
    match w with
    | [] -> Set.exists (fun q -> List.mem q (List.map fst n.nfa_final)) q
    | c::w ->
      let q' =
        Set.fold
          (fun q acc ->
             let successors =
               List.filter_map
                 (fun (cso,q') ->
                    match cso with
                    | None -> None
                    | Some cs -> if CharSet.mem c cs then Some q' else None)
                 (n.nfa_step q)
               |> Set.of_list
             in
             Set.union successors acc)
          q Set.empty

      in walk q' w in
  walk (Set.of_list n.nfa_initial) w

let () =
  let regexp_list = [
    (keyword_regexp "while",    fun s -> Some (SYM_WHILE));
    (keyword_regexp "if",    fun s -> Some (SYM_IF));
    (Cat(letter_regexp,
         Star(identifier_material)),
     fun s -> Some (SYM_IDENTIFIER s));

  ] in
  (* Uncomment the following line to test with the actual list of regular
     expressions. *)
  (* let regexp_list = list_regexp in *)
  List.iteri
    (fun i (rg, _) -> Printf.printf "%d: %s\n" i (string_of_regexp rg))
    regexp_list;

  let nfa = nfa_of_list_regexp regexp_list in

  Printf.printf "%s\n" (nfa_to_string nfa);

  let oc = open_out "/tmp/nfa.dot" in
  nfa_to_dot oc nfa;
  close_out oc;

  let dfa = dfa_of_nfa nfa in
  let oc = open_out "/tmp/dfa.dot" in
  dfa_to_dot oc dfa alphabet;
  close_out oc;

  let n =
    {
      nfa_states = [1; 2; 3; 4] ;
      nfa_initial = [1] ;
      nfa_final = [(3, fun s -> None); (4, fun s -> None)];
      nfa_step = fun q ->
        match q with
        | 1 -> [(Some (CharSet.singleton '0'), 2); (None, 3)]
        | 2 -> [(Some (CharSet.singleton '1'), 2); (Some (CharSet.singleton '1'), 4)]
        | 3 -> [(Some (CharSet.singleton '0'), 4); (None, 2)]
        | 4 -> [(Some (CharSet.singleton '0'), 2)]
        | _ -> []

    } in

  let expect_set str s_got s_exp =
    if Set.equal s_got s_exp
    then Printf.printf "[OK] %s\n" str
    else Printf.printf "[KO] %s : got %s, expected %s\n" str (string_of_int_set s_got)
        (string_of_int_set s_exp) in

  let ec1 = epsilon_closure n 1 in
  let ec2 = epsilon_closure n 2 in
  let ec3 = epsilon_closure n 3 in
  let ec4 = epsilon_closure n 4 in

  expect_set "epsilon_closure 1" ec1 (Set.of_list [1;2;3]);
  expect_set "epsilon_closure 2" ec2 (Set.of_list [2]);
  expect_set "epsilon_closure 3" ec3 (Set.of_list [2;3]);
  expect_set "epsilon_closure 4" ec4 (Set.of_list [4]);

  expect_set "dfa_initial_state" (dfa_initial_state n) (Set.of_list [1;2;3]);

  let string_of_opt_tok ot =
    match ot with
      None -> "None"
    | Some t -> Printf.sprintf "Some (%s)" (string_of_symbol t)
  in

  let expect_token_option str to_got to_exp =
    if to_got = to_exp
    then Printf.printf "[OK] %s\n" str
    else Printf.printf "[KO] %s : got %s, expected %s\n" str (string_of_opt_tok to_got)
        (string_of_opt_tok to_exp)
  in

  expect_token_option "min_priority 1" (min_priority [SYM_EOF; SYM_IDENTIFIER "bla"; SYM_WHILE]) (Some SYM_WHILE);
  expect_token_option "min_priority 2" (min_priority [SYM_EOF; SYM_IDENTIFIER "bla"]) (Some (SYM_IDENTIFIER "bla"));
  expect_token_option "min_priority 3" (min_priority [SYM_EOF; SYM_WHILE]) (Some SYM_WHILE);

  expect_token_option "min_priority 4" (min_priority []) None;

  let set_incl s1 s2 =
    List.for_all (fun s -> List.exists (Set.equal s) s2) s1
  in

  let set_eq s1 s2 = set_incl s1 s2 && set_incl s2 s1 in

  let string_of_int_set_set s =
    List.map (fun s ->
        Printf.sprintf "{%s}"
          (String.concat "," (List.map string_of_int (Set.to_list s)))
      ) s
    |> String.concat ", "
    |> Printf.sprintf "{%s}"
  in

  let expect_set_set str (set_got : Set.t list) (set_exp : Set.t list) =
    if set_eq set_got set_exp
    then Printf.printf "[OK] %s\n" str
    else Printf.printf "[KO] %s : got %s, expected %s\n" str
        (string_of_int_set_set set_got)
        (string_of_int_set_set set_exp)
  in

  let table = Hashtbl.create 10 in
  build_dfa_table table n (dfa_initial_state n);
  expect_set_set "dfa states" (Hashtbl.to_seq_keys table |> List.of_seq)
    [Set.of_list [1;2;3]; Set.of_list [2;4]; Set.of_list [2]];

  let expect_nfa_accepts n s b =
    let r = nfa_accepts n (char_list_of_string s) in
    if r = b
    then Printf.printf "[OK] nfa_accepts %s = %b\n" s r
    else Printf.printf "[KO] nfa_accepts %s = %b\n" s r
  in

  Printf.printf "*** NFA n1 : 'hello'\n";
  let n1, f1 = nfa_of_regexp (keyword_regexp "hello") 1 (fun _ -> None) in
  expect_nfa_accepts n1 "hello" true;
  expect_nfa_accepts n1 "bonjour" false;

  Printf.printf "*** NFA n2 : 'bonjour'\n";
  let n2, f2 = nfa_of_regexp (keyword_regexp "bonjour") f1 (fun _ -> None) in
  expect_nfa_accepts n2 "hello" false;
  expect_nfa_accepts n2 "bonjour" true;

  Printf.printf "*** NFA n3 : n1 | n2\n";
  let n3 = alt_nfa n1 n2 in
  expect_nfa_accepts n3 "hello" true;
  expect_nfa_accepts n3 "bonjour" true;
  expect_nfa_accepts n2 "buongiorno" false;

  Printf.printf "*** NFA n4 : n1 . n2 \n";
  let n4 = cat_nfa n1 n2 in
  expect_nfa_accepts n4 "hello" false;
  expect_nfa_accepts n4 "bonjour" false;
  expect_nfa_accepts n4 "hellobonjour" true;
  expect_nfa_accepts n4 "bonjourhello" false;

  Printf.printf "*** NFA n5 : n1* \n";
  let n5 = star_nfa n1 (fun _ -> None) in
  expect_nfa_accepts n5 "" true;
  expect_nfa_accepts n5 "hello" true;
  expect_nfa_accepts n5 "hellohello" true;
  expect_nfa_accepts n5 "hellobonjour" false;

  Printf.printf "*** NFA n6 : n3* \n";
  let n6 = star_nfa n3 (fun _ -> None) in
  expect_nfa_accepts n6 "" true;
  expect_nfa_accepts n6 "hello" true;
  expect_nfa_accepts n6 "hellohello" true;
  expect_nfa_accepts n6 "hellobonjour" true;
  expect_nfa_accepts n6 "hellobonjourhello" true;
  expect_nfa_accepts n6 "bonjourbonjourbonjourhello" true;
  expect_nfa_accepts n6 "bonjlo" false;


  ignore f2
