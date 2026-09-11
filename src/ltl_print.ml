open Elang
open Ltl
open Prog
open Rtl_print
open Utils
open Regalloc

(* Printing LTL registers, locations, instructions. *)
let string_of_reg = function
  | 0 -> "zero"
  | 1 -> "ra"
  | 2 -> "sp"
  | 3 -> "gp"
  | 4 -> "tp"
  | 5 -> "t0"
  | 6 -> "t1"
  | 7 -> "t2"
  | 8 -> "s0"
  | 9 -> "s1"
  | 10 -> "a0"
  | 11 -> "a1"
  | 12 -> "a2"
  | 13 -> "a3"
  | 14 -> "a4"
  | 15 -> "a5"
  | 16 -> "a6"
  | 17 -> "a7"
  | 18 -> "s2"
  | 19 -> "s3"
  | 20 -> "s4"
  | 21 -> "s5"
  | 22 -> "s6"
  | 23 -> "s7"
  | 24 -> "s8"
  | 25 -> "s9"
  | 26 -> "s10"
  | 27 -> "s11"
  | 28 -> "t3"
  | 29 -> "t4"
  | 30 -> "t5"
  | 31 -> "t6"
  | _ -> "undefreg"

let print_reg r =
  string_of_reg r

let print_loc loc =
  match loc with
  | Stk o -> Format.sprintf "stk(%d)" o
  | Reg r -> print_reg r

let print_binop (b: binop) =
  match b with
  | Elang.Eadd -> "add"
  | Elang.Emul -> "mul"
  | Elang.Emod -> "mod"
  | Elang.Exor -> "xor"
  | Elang.Ediv -> "div"
  | Elang.Esub -> "sub"
  | Elang.Eclt -> "clt"
  | Elang.Ecle -> "cle"
  | Elang.Ecgt -> "cgt"
  | Elang.Ecge -> "cge"
  | Elang.Eceq -> "ceq"
  | Elang.Ecne -> "cne"


let print_unop  (u: unop) =
  match u with
  | Elang.Eneg -> "neg"

let dump_ltl_instr oc (i: ltl_instr) =
  match i with
  | LAddi(rd, rs, i) ->
    Format.fprintf oc "%s <- addi %s, %d" (print_reg rd) (print_reg rs) i
  | LSubi(rd, rs, i) ->
    Format.fprintf oc "%s <- subi %s, %d" (print_reg rd) (print_reg rs) i
  | LBinop(b, rd, rs1, rs2) ->
    Format.fprintf oc "%s <- %s %s, %s"
      (print_reg rd) (print_binop b) ( print_reg rs1) (print_reg rs2)
  | LUnop(u, rd, rs) ->
    Format.fprintf oc "%s <- %s %s"
      (print_reg rd) (print_unop u)  (print_reg rs)
  | LStore(rt, i, rs, sz) ->
    Format.fprintf oc "%s%s[%d] <- %s" (print_reg rt) (string_of_mem_access_size sz) i (print_reg rs)
  | LLoad(rd, rt, i, sz) ->
    Format.fprintf oc "%s <- %s%s[%d]" (print_reg rd) (print_reg rt) (string_of_mem_access_size sz) i
  | LMov(rd, rs) ->
    Format.fprintf oc "%s <- %s" (print_reg rd) (print_reg rs)
  | LLabel l ->
    Format.fprintf oc "%s" l
  | LJmp l -> Format.fprintf oc "j %s" l
  | LJmpr r -> Format.fprintf oc "jmpr %s" (print_reg r)
  | LConst (rd, i) -> Format.fprintf oc "%s <- %d" (print_reg rd) i
  | LComment l -> Format.fprintf oc "; %s" l
  | LGroupStart Prologue -> Format.fprintf oc "; Prologue"
  | LGroupStart (LinearSource source) ->
    Format.fprintf oc "; Linear source: %s" source
  | LGroupStart Epilogue -> Format.fprintf oc "; Epilogue"
  | LBranch(cmp, rs1, rs2, s) ->
    Format.fprintf oc "%s(%s,%s) ? j %s"
      (print_cmpop cmp) (print_reg rs1) (print_reg rs2) s
  | LCall fname ->
    Format.fprintf oc "call %s" fname
  | LHalt -> Format.fprintf oc "halt"

let dump_ltl_instr_list fname oc l =
  List.iteri (fun i ins ->
      Format.fprintf oc "%s:%d: " fname i;
      dump_ltl_instr oc ins;
      Format.fprintf oc "\n") l

let dump_allocation oc fname alloc =
  Format.fprintf oc "// In function %s\n" fname;
  List.iter (fun (linr,ltlloc) ->
      Format.fprintf oc "// LinReg %d allocated to %s\n" linr (print_loc ltlloc)
    ) alloc

let dump_ltl_fun oc fname lf =
  dump_allocation oc fname lf.ltlregalloc;
  Format.fprintf oc "%s:\n" fname;
  dump_ltl_instr_list fname oc lf.ltlfunbody

let dump_ltl_prog oc lp =
  dump_prog dump_ltl_fun oc lp

let html_escape s =
  let escaped = Buffer.create (String.length s) in
  String.iter (function
      | '&' -> Buffer.add_string escaped "&amp;"
      | '<' -> Buffer.add_string escaped "&lt;"
      | '>' -> Buffer.add_string escaped "&gt;"
      | '"' -> Buffer.add_string escaped "&quot;"
      | '\'' -> Buffer.add_string escaped "&#39;"
      | c -> Buffer.add_char escaped c
    ) s;
  Buffer.contents escaped

let html_span class_name contents =
  Format.sprintf "<span class=\"%s\">%s</span>"
    class_name (html_escape contents)

let html_register register =
  html_span "ltl-register" (print_reg register)

let html_opcode opcode =
  html_span "ltl-opcode" opcode

let html_number number =
  html_span "ltl-number" (string_of_int number)

let html_label label =
  html_span "ltl-label" label

let html_operator operator =
  html_span "ltl-operator" operator

let dump_ltl_instr_html oc = function
  | LAddi (rd, rs, immediate) ->
    Format.fprintf oc "%s %s %s %s, %s"
      (html_register rd) (html_operator "<-") (html_opcode "addi")
      (html_register rs) (html_number immediate)
  | LSubi (rd, rs, immediate) ->
    Format.fprintf oc "%s %s %s %s, %s"
      (html_register rd) (html_operator "<-") (html_opcode "subi")
      (html_register rs) (html_number immediate)
  | LBinop (opcode, rd, rs1, rs2) ->
    Format.fprintf oc "%s %s %s %s, %s"
      (html_register rd) (html_operator "<-")
      (html_opcode (print_binop opcode))
      (html_register rs1) (html_register rs2)
  | LUnop (opcode, rd, rs) ->
    Format.fprintf oc "%s %s %s %s"
      (html_register rd) (html_operator "<-")
      (html_opcode (print_unop opcode)) (html_register rs)
  | LStore (address, offset, source, size) ->
    Format.fprintf oc "%s%s[%s] %s %s"
      (html_register address)
      (html_span "ltl-memory-size" (string_of_mem_access_size size))
      (html_number offset) (html_operator "<-") (html_register source)
  | LLoad (destination, address, offset, size) ->
    Format.fprintf oc "%s %s %s%s[%s]"
      (html_register destination) (html_operator "<-")
      (html_register address)
      (html_span "ltl-memory-size" (string_of_mem_access_size size))
      (html_number offset)
  | LMov (destination, source) ->
    Format.fprintf oc "%s %s %s"
      (html_register destination) (html_operator "<-")
      (html_register source)
  | LLabel label ->
    Format.fprintf oc "%s:" (html_label label)
  | LJmp label ->
    Format.fprintf oc "%s %s" (html_opcode "j") (html_label label)
  | LJmpr register ->
    Format.fprintf oc "%s %s" (html_opcode "jmpr")
      (html_register register)
  | LConst (destination, value) ->
    Format.fprintf oc "%s %s %s"
      (html_register destination) (html_operator "<-") (html_number value)
  | LComment comment ->
    Format.fprintf oc "<span class=\"ltl-comment\">; %s</span>"
      (html_escape comment)
  | LGroupStart _ -> ()
  | LBranch (comparison, rs1, rs2, label) ->
    Format.fprintf oc "%s(%s,%s) %s %s %s"
      (html_opcode (print_cmpop comparison))
      (html_register rs1) (html_register rs2)
      (html_operator "?") (html_opcode "j") (html_label label)
  | LCall function_name ->
    Format.fprintf oc "%s %s"
      (html_opcode "call") (html_span "ltl-function" function_name)
  | LHalt ->
    Format.fprintf oc "%s" (html_opcode "halt")

let group_class_and_contents = function
  | Prologue -> "prologue", "Prologue", None
  | LinearSource source -> "linear", "Linear source", Some source
  | Epilogue -> "epilogue", "Epilogue", None

let dump_ltl_instr_list_html fname oc instructions =
  let group_is_open = ref false in
  let close_group () =
    if !group_is_open then begin
      Format.fprintf oc "</span></span>";
      group_is_open := false
    end
  in
  List.iteri (fun i instruction ->
      match instruction with
      | LGroupStart group ->
        close_group ();
        let group_class, label, contents = group_class_and_contents group in
        Format.fprintf oc
          "<span class=\"ltl-group ltl-group-%s\"><span class=\"ltl-group-header\"><span class=\"ltl-group-label\">%s</span>"
          group_class label;
        Option.iter (fun contents ->
            Format.fprintf oc
              "<span class=\"ltl-group-source\">%s</span>"
              (html_escape contents)
          ) contents;
        Format.fprintf oc "</span><span class=\"ltl-group-body\">";
        group_is_open := true
      | _ ->
        Format.fprintf oc
          "<span class=\"ltl-instruction-address\">%s:%d:</span> "
          (html_escape fname) i;
        dump_ltl_instr_html oc instruction;
        Format.fprintf oc "\n"
    ) instructions;
  close_group ()

let dump_allocation_html oc fname allocation =
  Format.fprintf oc
    "<span class=\"ltl-allocation\"><span class=\"ltl-allocation-header\"><span class=\"ltl-allocation-label\">Register allocation</span><span class=\"ltl-allocation-function\">Function %s</span></span><span class=\"ltl-allocation-entries\">"
    (html_escape fname);
  begin match allocation with
    | [] ->
      Format.fprintf oc
        "<span class=\"ltl-allocation-empty\">No virtual registers</span>"
    | _ ->
      allocation
      |> List.sort (fun (left, _) (right, _) -> Int.compare left right)
      |> List.iter (fun (linear_reg, ltl_loc) ->
          let location_class =
            match ltl_loc with
            | Reg _ -> "register"
            | Stk _ -> "stack"
          in
          Format.fprintf oc
            "<span class=\"ltl-allocation-entry\"><span class=\"ltl-allocation-register\">r%d</span><span class=\"ltl-allocation-arrow\">&rarr;</span><span class=\"ltl-allocation-location ltl-allocation-location-%s\">%s</span></span>"
            linear_reg location_class (html_escape (print_loc ltl_loc))
        )
  end;
  Format.fprintf oc "</span></span>"

let dump_ltl_fun_html oc fname ltl_fun =
  dump_allocation_html oc fname ltl_fun.ltlregalloc;
  Format.fprintf oc "<span class=\"ltl-function-label\">%s:</span>\n"
    (html_escape fname);
  dump_ltl_instr_list_html fname oc ltl_fun.ltlfunbody

let dump_ltl_prog_html oc lp =
  dump_prog dump_ltl_fun_html oc lp
