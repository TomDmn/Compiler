(* HTML presentation for RTL.  Plain text dumping remains in [Rtl_print]. *)
open Rtl
open Prog
open Rtl_print
module Set = Collections.IntSet

let escape text = Report.escape_html_text text
let span class_name text = Printf.sprintf "<span class=\"%s\">%s</span>" class_name (escape text)
let reg register = span "rtl-register" (print_reg register)
let opcode text = span "rtl-opcode" text
let number value = span "rtl-number" (string_of_int value)
let label text = span "rtl-label" text
let operator text = span "rtl-operator" text

let dump_instruction name formatter = function
  | Rbinop (op, dst, left, right) -> Format.fprintf formatter "%s %s %s(%s, %s)" (reg dst) (operator "<-") (opcode (Elang_print.dump_binop op)) (reg left) (reg right)
  | Runop (op, dst, source) -> Format.fprintf formatter "%s %s %s(%s)" (reg dst) (operator "<-") (opcode (Elang_print.dump_unop op)) (reg source)
  | Rconst (dst, value) -> Format.fprintf formatter "%s %s %s" (reg dst) (operator "<-") (number value)
  | Rbranch (cmp, left, right, successor) -> Format.fprintf formatter "%s %s %s %s %s %s" (reg left) (operator (print_cmpop cmp)) (reg right) (operator "?") (opcode "jmp") (label (Format.sprintf "%s_%d" name successor))
  | Rjmp successor -> Format.fprintf formatter "%s %s" (opcode "jmp") (label (Format.sprintf "%s_%d" name successor))
  | Rmov (dst, source) -> Format.fprintf formatter "%s %s %s" (reg dst) (operator "<-") (reg source)
  | Rret source -> Format.fprintf formatter "%s %s" (opcode "ret") (reg source)
  | Rprint source -> Format.fprintf formatter "%s %s" (opcode "print") (reg source)
  | Rlabel value -> Format.fprintf formatter "%s" (label (Format.sprintf "%s_%d:" name value))

let dump_liveness formatter title = function
  | None -> ()
  | Some live ->
    Format.fprintf formatter "<span class=\"rtl-liveness rtl-liveness-%s\"><span class=\"rtl-liveness-label\">Live %s</span><span class=\"rtl-liveness-registers\">" title title;
    if Set.is_empty live then Format.fprintf formatter "<span class=\"rtl-liveness-empty\">empty</span>"
    else Set.iter (fun value -> Format.fprintf formatter "<span class=\"rtl-live-register\">%s</span>" (reg value)) live;
    Format.fprintf formatter "</span></span>"

let dump_row name lives index formatter instruction =
  let before, after = match lives with
    | None -> None, None
    | Some (before, after) -> Hashtbl.find_opt before index, Hashtbl.find_opt after index in
  Format.fprintf formatter "<span class=\"rtl-instruction\"><span class=\"rtl-instruction-code\">";
  dump_instruction name formatter instruction;
  Format.fprintf formatter "</span><span class=\"rtl-liveness-row\">";
  dump_liveness formatter "before" before; dump_liveness formatter "after" after;
  Format.fprintf formatter "</span></span>"

let dump_rtl_prog_html formatter program =
  dump_prog (fun formatter name function_ ->
      Format.fprintf formatter "<span class=\"rtl-function\"><span class=\"rtl-function-signature\"><span class=\"rtl-function-name\">%s</span>(%s):</span>"
        (escape name) (String.concat ", " (List.map reg function_.rtlfunargs));
      Hashtbl.iter (fun node instructions ->
          Format.fprintf formatter "<span class=\"rtl-node\"><span class=\"rtl-node-label\">%s</span><span class=\"rtl-node-body\">"
            (label (Format.sprintf "%s_%d:" name node));
          List.iteri (fun index instruction -> dump_row name None index formatter instruction) instructions;
          Format.fprintf formatter "</span></span>") function_.rtlfunbody;
      Format.fprintf formatter "</span>") formatter program
