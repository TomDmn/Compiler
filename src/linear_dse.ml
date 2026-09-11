open Rtl
open Linear
open Linear_liveness
open Prog
open Utils
open Report
open Report
open Options
module Set = Collections.IntSet

let dse_instr (ins: rtl_instr) live =
   [ins]


let dse_fun live {linearfunargs; linearfunbody; linearfuninfo; } =
  let body =
    linearfunbody
    |> List.mapi (fun i ins -> dse_instr ins (Collections.hashtbl_find_default live i Set.empty))
    |> List.concat in
  { linearfunargs; linearfunbody = body; linearfuninfo; }


let dse_prog p live =
  if !Options.no_linear_dse
  then p
  else
  List.map (fun (fname,gdef) ->
      match gdef with
        Gfun f ->
        let live = Collections.hashtbl_find_default live fname (Hashtbl.create 17, Hashtbl.create 17) |> snd in
        let f = dse_fun live f in
        (fname, Gfun f)
      ) p

let pass_linear_dse linear lives =
  let linear = dse_prog linear lives in
  record_compile_result "DSE";
  OK linear
