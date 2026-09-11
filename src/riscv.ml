open Elang
open Rtl
open Ltl
open Ltl_print
open Utils
open Prog
open Options
open Archi

(* This file performs the translation from LTL programs to RISC-V assembly
   programs. The languages are basically the same, so the only thing to do here
   is to print LTL instructions in RISC-V assembly syntax.

   Another thing to do is to deal with command-line arguments. These are given
   as strings and need to be converted into strings. That is the purpose of the
   [riscv_load_args] function in this file.

   Finally, [riscv_prelude] gives an entry point in the program : a global
   "_start" symbol is created, which points to startup code, whose job is the
   following:
   - initializes the global pointer (for "heap" allocation),
   - sets up the stack,
   - retrieves arguments,
   - calls the "main" function of our program,
   - prints the return value to the screen,
   - and finally exits the program by issuing the appropriate system call.

*)

let riscv_of_cmp (cmp: rtl_cmp) =
  match cmp with
  | Rtl.Rclt -> "blt"
  | Rtl.Rcle -> "ble"
  | Rtl.Rcgt -> "bgt"
  | Rtl.Rcge -> "bge"
  | Rtl.Rceq -> "beq"
  | Rtl.Rcne -> "bne"

let print_binop (b: binop) =
  match b with
  | Elang.Eadd -> "add"
  | Elang.Emul -> "mul"
  | Elang.Emod -> "remu"
  | Elang.Exor -> "xor"
  | Elang.Ediv -> "divu"
  | Elang.Esub -> "sub"
  | _ -> failwith "Unexpected binop"

let print_unop (u: unop) =
  match u with
  | Elang.Eneg -> "neg"

let instrsuffix_of_size sz =
  match sz with
  | MAS1 -> 'b'
  | MAS4 -> 'w'
  | MAS8 -> 'd'

let dump_riscv_instr oc (i: ltl_instr) =
  match i with
  | LAddi(rd, rs, i) ->
    Format.fprintf oc "  addi %s, %s, %d\n" (print_reg rd) (print_reg rs) i
  | LSubi(rd, rs, i) ->
    Format.fprintf oc "  addi %s, %s, %d\n" (print_reg rd) (print_reg rs) (-i)
  | LBinop(b, rd, rs1, rs2) ->
    begin match b with
     | Elang.Eclt ->
        Format.fprintf oc "  slt %s, %s, %s\n"
          (print_reg rd) (print_reg rs1) (print_reg rs2)
      | Elang.Ecgt ->
        Format.fprintf oc "  slt %s, %s, %s\n"
          (print_reg rd) (print_reg rs2) (print_reg rs1)
      | Elang.Ecle ->
        (* 'rd <- rs1 <= rs2' == 'rd <- rs2 < rs1; rd <- seqz rd' *)
        Format.fprintf oc "  slt %s, %s, %s\n"
          (print_reg rd) (print_reg rs2) (print_reg rs1);
        Format.fprintf oc "  seqz %s, %s\n"
          (print_reg rd) (print_reg rd)
      | Elang.Ecge ->
        Format.fprintf oc "  slt %s, %s, %s\n"
          (print_reg rd) (print_reg rs1) (print_reg rs2);
        Format.fprintf oc "  seqz %s, %s\n"
          (print_reg rd) (print_reg rd)
      | Elang.Eceq ->
        Format.fprintf oc "  sub %s, %s, %s\n"
          (print_reg rd) (print_reg rs1) (print_reg rs2);
        Format.fprintf oc "  seqz %s, %s\n"
          (print_reg rd) (print_reg rd)
      | Elang.Ecne ->
        Format.fprintf oc "  sub %s, %s, %s\n"
          (print_reg rd) (print_reg rs1) (print_reg rs2);
        Format.fprintf oc "  snez %s, %s\n"
          (print_reg rd) (print_reg rd)
      | _ -> Format.fprintf oc "  %s %s, %s, %s\n"
               (print_binop b) (print_reg rd) (print_reg rs1) (print_reg rs2)
    end
  | LUnop(u, rd, rs) ->
    Format.fprintf oc "  %s %s, %s\n"
          (print_unop u) (print_reg rd) (print_reg rs)
  | LStore(rt, i, rs, sz) ->
    let sz = instrsuffix_of_size sz in
    Format.fprintf oc "  s%c %s, %d(%s)\n"
          sz (print_reg rs) i (print_reg rt)
  | LLoad(rd, rt, i, sz) ->
    let sz = (instrsuffix_of_size sz) in
    Format.fprintf oc "  l%c %s, %d(%s)\n"
      sz (print_reg rd) i (print_reg rt)
  | LMov(rd, rs) ->
    Format.fprintf oc "  mv %s, %s\n" (print_reg rd) (print_reg rs)
  | LLabel l ->
    Format.fprintf oc "%s:\n" l
  | LJmp l -> Format.fprintf oc "  j %s\n" l
  | LJmpr r -> Format.fprintf oc "  jr %s\n" (print_reg r)
  | LConst (rd, i) -> Format.fprintf oc "  li %s, %d\n" (print_reg rd) i
  | LComment l -> Format.fprintf oc "  # %s\n" l
  | LGroupStart Prologue -> Format.fprintf oc "  # Prologue\n"
  | LGroupStart (LinearSource source) ->
    Format.fprintf oc "  # Linear source: %s\n" source
  | LGroupStart Epilogue -> Format.fprintf oc "  # Epilogue\n"
  | LBranch(cmp, rs1, rs2, s) ->
    Format.fprintf oc "  %s %s, %s, %s\n"
      (riscv_of_cmp cmp) (print_reg rs1) (print_reg rs2) s
  | LCall fname ->
    Format.fprintf oc "  jal ra, %s\n" fname
  | LHalt -> Format.fprintf oc "  halt\n"

let dump_riscv_fun oc (fname , lf) =
  Format.fprintf oc "\n%s:\n" fname;
  List.iter (dump_riscv_instr oc) lf.ltlfunbody

let riscv_load_args target oc : unit =
  (match target with
   | Linux -> LLoad(reg_s1, reg_sp, 0, archi_mas ()) :: (* s1 <- argc *)
              LAddi(reg_s2, reg_sp, (Archi.wordsize ())) :: []
   | Xv6 -> LMov(reg_s1, reg_a0) ::
            LMov(reg_s2, reg_a1) :: []) @
  LConst(reg_s3, 1) ::
  LSubi(reg_sp, reg_sp, 72) ::
  LLabel "Lloop" ::
  LBranch(Rceq, reg_s3, reg_s1, "Lendargs") ::
  LMov(reg_a0, reg_t4) ::
  LAddi(reg_s4, reg_s3, 0) ::
  LConst(reg_t1, (Archi.wordsize ())) ::
  LBinop(Emul, reg_s4, reg_s4, reg_t1) ::
  LBinop(Eadd, reg_t3, reg_s4, reg_s2) ::
  LLoad(reg_a0, reg_t3, 0, archi_mas ()) ::
  LCall "atoi" ::
  LBinop(Esub, reg_s4, reg_fp, reg_s4) ::
  LStore(reg_s4, 0, reg_a0, archi_mas ()) ::
  LAddi(reg_s3, reg_s3, 1) ::
  LJmp "Lloop" ::
  LLabel "Lendargs" ::
  LLoad(reg_a0, reg_fp, -8, archi_mas ()) ::
  LLoad(reg_a1, reg_fp, -16, archi_mas ()) ::
  LLoad(reg_a2, reg_fp, -24, archi_mas ()) ::
  LLoad(reg_a3, reg_fp, -32, archi_mas ()) ::
  LLoad(reg_a4, reg_fp, -40, archi_mas ()) ::
  LLoad(reg_a5, reg_fp, -48, archi_mas ()) ::
  LLoad(reg_a6, reg_fp, -56, archi_mas ()) ::
  LLoad(reg_a7, reg_fp, -64, archi_mas ()) ::
  [] |>
  List.iter (dump_riscv_instr oc)

let rv_store () =
  Format.sprintf "s%c" (Archi.instrsuffix ())
let rv_load () =
  Format.sprintf "l%c" (Archi.instrsuffix ())

let riscv_prelude target oc =
  Format.fprintf oc ".include \"syscall_numbers.s\"\n";
  Format.fprintf oc ".globl _start\n";
  Format.fprintf oc "_start:\n";
  Format.fprintf oc "  lui gp, %%hi(_heap_start)\n";
  Format.fprintf oc "  addi gp, gp, %%lo(_heap_start)\n";
  Format.fprintf oc "  addi t0, gp, 8\n";
  Format.fprintf oc "  %s t0, 0(gp)\n" (rv_store ());
  Format.fprintf oc "  mv s0, sp\n";
  riscv_load_args target oc ;
  Format.fprintf oc "  jal ra, main\n";
  Format.fprintf oc "  mv s0, a0\n";
  Format.fprintf oc "  jal ra, println\n";
  Format.fprintf oc "  mv a0, s0\n";
  Format.fprintf oc "  jal ra, print_int\n";
  Format.fprintf oc "  jal ra, println\n";
  Format.fprintf oc "  addi a7, zero, SYSCALL_EXIT\n";
  Format.fprintf oc "  ecall\n"

let dump_riscv_prog target oc lp : unit =
  (if !nostart then () else riscv_prelude target oc);
  Format.fprintf oc "\n.global main\n";
  List.iter (function
        (fname, Gfun f) -> dump_riscv_fun oc (fname,f)
    ) lp

let riscv_span class_name contents =
  Format.sprintf "<span class=\"%s\">%s</span>"
    class_name (html_escape contents)

let is_riscv_register name =
  let named_registers =
    ["zero"; "ra"; "sp"; "gp"; "tp"; "fp";
     "t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6";
     "s0"; "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7";
     "s8"; "s9"; "s10"; "s11";
     "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"]
  in
  List.mem name named_registers ||
  let length = String.length name in
  length > 1 && name.[0] = 'x' &&
  try
    let number = int_of_string (String.sub name 1 (length - 1)) in
    number >= 0 && number <= 31
  with Failure _ -> false

let is_riscv_identifier_char = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '.' | '$' -> true
  | _ -> false

let is_riscv_number_char = function
  | '0'..'9' | 'a'..'f' | 'A'..'F' | 'x' | 'X' -> true
  | _ -> false

let find_end predicate text start =
  let rec find index =
    if index < String.length text && predicate text.[index]
    then find (index + 1)
    else index
  in
  find start

let highlight_riscv_operands operands =
  let highlighted = Buffer.create (String.length operands * 2) in
  let add_span class_name start finish =
    Buffer.add_string highlighted
      (riscv_span class_name (String.sub operands start (finish - start)))
  in
  let rec highlight index =
    if index < String.length operands then
      match operands.[index] with
      | '"' ->
        let rec find_quote position =
          if position >= String.length operands then position
          else if operands.[position] = '"' then position + 1
          else find_quote (position + 1)
        in
        let finish = find_quote (index + 1) in
        add_span "riscv-string" index finish;
        highlight finish
      | '-' when index + 1 < String.length operands &&
                 operands.[index + 1] >= '0' && operands.[index + 1] <= '9' ->
        let finish = find_end is_riscv_number_char operands (index + 1) in
        add_span "riscv-number" index finish;
        highlight finish
      | '0'..'9' ->
        let finish = find_end is_riscv_number_char operands index in
        add_span "riscv-number" index finish;
        highlight finish
      | '%' ->
        let finish = find_end is_riscv_identifier_char operands (index + 1) in
        add_span "riscv-relocation" index finish;
        highlight finish
      | ('a'..'z' | 'A'..'Z' | '_' | '.' | '$') ->
        let finish = find_end is_riscv_identifier_char operands index in
        let word = String.sub operands index (finish - index) in
        add_span
          (if is_riscv_register word then "riscv-register" else "riscv-symbol")
          index finish;
        highlight finish
      | (',' | '(' | ')' | ':') as punctuation ->
        Buffer.add_string highlighted
          (riscv_span "riscv-punctuation" (String.make 1 punctuation));
        highlight (index + 1)
      | character ->
        Buffer.add_string highlighted (html_escape (String.make 1 character));
        highlight (index + 1)
  in
  highlight 0;
  Buffer.contents highlighted

let highlight_riscv_line line =
  let code, comment =
    match String.index_opt line '#' with
    | None -> line, None
    | Some index ->
      String.sub line 0 index,
      Some (String.sub line index (String.length line - index))
  in
  let first_non_space =
    find_end (function ' ' | '\t' -> true | _ -> false) code 0
  in
  let indentation = String.sub code 0 first_non_space in
  let body =
    String.sub code first_non_space (String.length code - first_non_space)
  in
  let first_word_end =
    find_end (function ' ' | '\t' -> false | _ -> true) body 0
  in
  let first_word = String.sub body 0 first_word_end in
  let remainder =
    String.sub body first_word_end (String.length body - first_word_end)
  in
  let highlighted_code =
    if first_word = "" then html_escape indentation
    else if first_word.[0] = '.' then
      html_escape indentation ^ riscv_span "riscv-directive" first_word ^
      highlight_riscv_operands remainder
    else if first_word.[String.length first_word - 1] = ':' then
      html_escape indentation ^ riscv_span "riscv-label" first_word ^
      highlight_riscv_operands remainder
    else
      html_escape indentation ^ riscv_span "riscv-opcode" first_word ^
      highlight_riscv_operands remainder
  in
  match comment with
  | None -> highlighted_code
  | Some comment -> highlighted_code ^ riscv_span "riscv-comment" comment

let dump_riscv_prog_html target oc program =
  let assembly = Format.asprintf "%a" (dump_riscv_prog target) program in
  String.split_on_char '\n' assembly
  |> List.iter (fun line ->
      Format.fprintf oc "%s\n" (highlight_riscv_line line))
