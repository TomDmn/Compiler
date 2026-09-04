module CharSet = Set.Make (Char)
module IntSet = Set.Make (Int)
module StringSet = Set.Make (String)

module IntSetSet = Set.Make (struct
  type t = IntSet.t

  let compare = IntSet.compare
end)

module IntPairSet = Set.Make (struct
  type t = int * int

  let compare = compare
end)

let list_fold_lefti f init list =
  let _, result =
    List.fold_left
      (fun (index, acc) value -> index + 1, f acc index value)
      (0, init) list
  in
  result

let array_fold_lefti f init array =
  let result = ref init in
  Array.iteri (fun index value -> result := f !result index value) array;
  !result

let hashtbl_find_default table key default =
  match Hashtbl.find_opt table key with
  | Some value -> value
  | None -> default

let hashtbl_map f table =
  Hashtbl.fold
    (fun key value result -> Hashtbl.add result key (f key value); result)
    table (Hashtbl.create (Hashtbl.length table))

let hashtbl_filter_map f table =
  Hashtbl.fold
    (fun key value result ->
       match f key value with
       | None -> result
       | Some value -> Hashtbl.add result key value; result)
    table (Hashtbl.create (Hashtbl.length table))

let hashtbl_modify_default default key f table =
  Hashtbl.replace table key (f (hashtbl_find_default table key default))
