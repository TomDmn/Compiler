(* HTML presentation for LTL.  Plain text dumping remains in [Ltl_print]. *)
open Ltl
open Rtl
open Regalloc
open Prog
open Ltl_print

let escape = Report.escape_html_text
let span class_name text = Printf.sprintf "<span class=\"%s\">%s</span>" class_name (escape text)
let register value = span "ltl-register" (print_reg value)
let opcode value = span "ltl-opcode" value
let number value = span "ltl-number" (string_of_int value)
let label value = span "ltl-label" value
let operator value = span "ltl-operator" value
let comparison = function
  | Rclt -> "<" | Rcle -> "<=" | Rcgt -> ">" | Rcge -> ">=" | Rceq -> "==" | Rcne -> "!="

let dump_instruction formatter = function
  | LAddi (dst, source, immediate) -> Format.fprintf formatter "%s %s %s %s, %s" (register dst) (operator "<-") (opcode "addi") (register source) (number immediate)
  | LSubi (dst, source, immediate) -> Format.fprintf formatter "%s %s %s %s, %s" (register dst) (operator "<-") (opcode "subi") (register source) (number immediate)
  | LBinop (op, dst, left, right) -> Format.fprintf formatter "%s %s %s %s, %s" (register dst) (operator "<-") (opcode (print_binop op)) (register left) (register right)
  | LUnop (op, dst, source) -> Format.fprintf formatter "%s %s %s %s" (register dst) (operator "<-") (opcode (print_unop op)) (register source)
  | LStore (address, offset, source, size) -> Format.fprintf formatter "%s%s[%s] %s %s" (register address) (span "ltl-memory-size" (string_of_mem_access_size size)) (number offset) (operator "<-") (register source)
  | LLoad (dst, address, offset, size) -> Format.fprintf formatter "%s %s %s%s[%s]" (register dst) (operator "<-") (register address) (span "ltl-memory-size" (string_of_mem_access_size size)) (number offset)
  | LMov (dst, source) -> Format.fprintf formatter "%s %s %s" (register dst) (operator "<-") (register source)
  | LLabel value -> Format.fprintf formatter "%s" (label value)
  | LJmp value -> Format.fprintf formatter "%s %s" (opcode "j") (label value)
  | LJmpr value -> Format.fprintf formatter "%s %s" (opcode "jmpr") (register value)
  | LConst (dst, value) -> Format.fprintf formatter "%s %s %s" (register dst) (operator "<-") (number value)
  | LComment value -> Format.fprintf formatter "%s" (span "ltl-comment" ("; " ^ value))
  | LGroupStart _ -> ()
  | LBranch (cmp, left, right, successor) -> Format.fprintf formatter "%s(%s, %s) %s %s %s" (opcode (comparison cmp)) (register left) (register right) (operator "?") (opcode "j") (label successor)
  | LCall value -> Format.fprintf formatter "%s %s" (opcode "call") (label value)
  | LHalt -> Format.fprintf formatter "%s" (opcode "halt")

let group = function
  | Prologue -> "prologue", "Prologue", None
  | Epilogue -> "epilogue", "Epilogue", None
  | LinearSource source -> "linear-source", "Linear source", Some source

let dump_debug_instruction formatter = function
  | LGroupStart value ->
    let _, title, source = group value in
    let description = match source with
      | None -> title
      | Some text -> title ^ ": " ^ text in
    Format.fprintf formatter "%s" (span "ltl-comment" ("; " ^ description))
  | instruction -> dump_instruction formatter instruction

let dump_ltl_prog_html formatter program =
  dump_prog (fun formatter name function_ ->
      Format.fprintf formatter "<span class=\"ltl-allocation\"><span class=\"ltl-allocation-header\"><span class=\"ltl-allocation-label\">Register allocation</span><span class=\"ltl-allocation-function\">Function %s</span></span><span class=\"ltl-allocation-entries\">" (escape name);
      List.iter (fun (virtual_register, location) ->
          let location_class = match location with Reg _ -> "register" | Stk _ -> "stack" in
          Format.fprintf formatter "<span class=\"ltl-allocation-entry\"><span class=\"ltl-allocation-register\">r%d</span><span class=\"ltl-allocation-arrow\">&rarr;</span><span class=\"ltl-allocation-location ltl-allocation-location-%s\">%s</span></span>" virtual_register location_class (escape (print_loc location))) function_.ltlregalloc;
      Format.fprintf formatter "</span></span><span class=\"ltl-function-label\">%s:</span>\n" (escape name);
      let open_group = ref false in
      List.iteri (fun index instruction -> match instruction with
          | LGroupStart value ->
            if !open_group then Format.fprintf formatter "</span></span>";
            let class_name, title, source = group value in
            Format.fprintf formatter "<span class=\"ltl-group ltl-group-%s\"><span class=\"ltl-group-header\"><span class=\"ltl-group-label\">%s</span>" class_name title;
            Option.iter (fun text -> Format.fprintf formatter "<span class=\"ltl-group-source\">%s</span>" (escape text)) source;
            Format.fprintf formatter "</span><span class=\"ltl-group-body\">"; open_group := true
          | _ -> Format.fprintf formatter "<span class=\"ltl-instruction-address\">%s:%d:</span> " (escape name) index; dump_instruction formatter instruction; Format.fprintf formatter "\n") function_.ltlfunbody;
      if !open_group then Format.fprintf formatter "</span></span>") formatter program
