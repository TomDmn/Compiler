open Elang
open Cfg
open Rtl
open Prog
open Utils
open Report
open Options

(* Part of generating RTL involves allocating variables into
   RTL pseudo-registers.

   These registers are unlimited in number so this problem is easy.

   Given:
   - [next_reg], the first available register number (not yet allocated to
   a variable)
   - [var2reg], a list of associations whose keys are variables and the
   register number values
   - [v] a variable name (of type [string]),

   [find_var (next_reg, var2reg) v] returns a triple [(r, next_reg, var2reg)]:

   - [r] is the RTL register associated with the variable [v]
   - [next_reg] is the new first register available
   - [var2reg] is the new variable name/register association.

*)
let find_var (next_reg, var2reg) v =
  match List.assoc_opt v var2reg with
    | Some r -> (r, next_reg, var2reg)
    | None -> (next_reg, next_reg + 1, assoc_set var2reg v next_reg)

(* [rtl_instrs_of_cfg_expr (next_reg, var2reg) e] constructs a list
   of RTL instructions corresponding to the evaluation of an expression E.

   The return of this function is a quadruplet [(r,l,next_reg,var2reg)], where:
   - [r] is the RTL register in which the result of the evaluation of [e] will have
     been stored
   - [l] is a list of RTL instructions.
   - [next_reg] is the new first register available
   - [var2reg] is the new variable name/register association.
*)
let rec rtl_instrs_of_cfg_expr (next_reg, var2reg) (e: expr) =
   (next_reg, [], next_reg, var2reg)

let is_cmp_op =
  function Eclt -> Some Rclt
         | Ecle -> Some Rcle
         | Ecgt -> Some Rcgt
         | Ecge -> Some Rcge
         | Eceq -> Some Rceq
         | Ecne -> Some Rcne
         | _ -> None

let rtl_cmp_of_cfg_expr (e: expr) =
  match e with
  | Ebinop (b, e1, e2) ->
    (match is_cmp_op b with
     | None -> (Rcne, e, Eint 0)
     | Some rop -> (rop, e1, e2))
  | _ -> (Rcne, e, Eint 0)


let rtl_instrs_of_cfg_node ((next_reg:int), (var2reg: (string*int) list)) (c: cfg_node) =
   (* TODO *)
   ([], next_reg, var2reg)

let rtl_instrs_of_cfg_fun cfgfunname ({ cfgfunargs; cfgfunbody; cfgentry }: cfg_fun) =
  let (rargs, next_reg, var2reg) =
    List.fold_left (fun (rargs, next_reg, var2reg) a ->
        let (r, next_reg, var2reg) = find_var (next_reg, var2reg) a in
        (rargs @ [r], next_reg, var2reg)
      )
      ([], 0, []) cfgfunargs
  in
  let rtlfunbody = Hashtbl.create 17 in
  let (next_reg, var2reg) = Hashtbl.fold (fun n node (next_reg, var2reg)->
      let (l, next_reg, var2reg) = rtl_instrs_of_cfg_node (next_reg, var2reg) node in
      Hashtbl.replace rtlfunbody n l;
      (next_reg, var2reg)
    ) cfgfunbody (next_reg, var2reg) in
  {
    rtlfunargs = rargs;
    rtlfunentry = cfgentry;
    rtlfunbody;
    rtlfuninfo = var2reg;
  }

let rtl_of_gdef funname = function
    Gfun f -> Gfun (rtl_instrs_of_cfg_fun funname f)

let rtl_of_cfg cp = List.map (fun (s, gd) -> (s, rtl_of_gdef s gd)) cp

let pass_rtl_gen cfg =
  let rtl = rtl_of_cfg cfg in
  record_compile_result "RTL";
  OK rtl
