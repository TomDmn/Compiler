open Symbols
open Utils
module Set = Collections.CharSet
(* Regular Expressions*)

(* We model regular expressions with the following type.

   A regular expression is either:
   - [Eps] denotes the empty word.
   - [Charset cs] denotes the regular expression matching any character in
     the set [cs].
   - [Cat(r1,r2)] denotes the concatenation of [r1] and [r2]: recognizes the words
     [uv] such that [u] belongs to [r1] and [v] belongs to [r2].
   - [Alt(r1,r2)] denotes a choice between [r1] and [r2]: it recognizes words
     recognized by either [r1] or [r2].
   - [Star r] denotes the repetition 0, 1 or more times of the expression [r].
*)

type regexp =
  | Eps
  | Charset of Set.t
  | Cat of regexp * regexp
  | Alt of regexp * regexp
  | Star of regexp

(* [char_regexp c] recognizes the character [c] only.*)
let char_regexp c = Charset (Set.singleton c)

(* [char_range l] recognizes all the characters of [l].*)
let char_range (l: char list) =
  Charset (Set.of_list l)

(* [str_regexp s] recognizes the character string [s].*)
let str_regexp (s: char list) =
  List.fold_right (fun c reg -> Cat(Charset (Set.singleton c), reg)) s Eps

(* if s = [a,b,c] it does Eps ->  c . Eps - > b . (c . Eps) -> a . (b . (c . Eps))
i.e : Cat(
  Charset {'a'},
  Cat(
    Charset {'b'},
    Cat(
      Charset {'c'},
      Eps
    )
  )
)
*)

(* [plus r] recognizes the expression [r] 1 or more times.*)
let plus r = Cat(r,Star r)

(* Display function. May be useful for debugging.*)
let rec string_of_regexp r =
  match r with
    Eps -> "Eps"
  | Charset c -> Printf.sprintf "[%s]" (string_of_char_list (Set.to_list c))
  | Alt (r1,r2) -> Printf.sprintf "(%s)|(%s)"
                     (string_of_regexp r1) (string_of_regexp r2)
  | Cat (r1,r2) -> Printf.sprintf "(%s).(%s)"
                     (string_of_regexp r1) (string_of_regexp r2)
  | Star r -> Printf.sprintf "(%s)*" (string_of_regexp r)


let lowercase_letters = "abcdefghijklmnopqrstuvwxyz"
let uppercase_letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let digits = "0123456789"
let other_characters = "?!=<>_ :;,{}()[]^`-+*/%@\n\t\x00.\"\'\\|~#$&"

(* The ^ operator denotes the concatenation of character strings.*)
let alphabet = char_list_of_string (lowercase_letters ^ uppercase_letters ^ digits ^ other_characters)
let letter_regexp = char_range (char_list_of_string (uppercase_letters ^ lowercase_letters))
let digit_regexp = char_range (char_list_of_string digits)
let identifier_material = char_range (char_list_of_string (uppercase_letters ^ lowercase_letters ^ digits ^ "_"))
let keyword_regexp s = str_regexp (char_list_of_string s)

(* The list of regular expressions used to identify E language tokens*)
let list_regexp : (regexp * (string -> token option)) list =
  [
    (keyword_regexp "while",    fun _ -> Some (SYM_WHILE));
    (keyword_regexp "int", fun _ -> Some (SYM_INT));
    (* begin TODO *)
    (keyword_regexp "void",   fun _ -> Some (SYM_VOID));
    (keyword_regexp "char",   fun _ -> Some (SYM_CHAR));
    (keyword_regexp "if",     fun _ -> Some (SYM_IF));
    (keyword_regexp "else",   fun _ -> Some (SYM_ELSE));
    (keyword_regexp "return", fun _ -> Some (SYM_RETURN));
    (keyword_regexp "print",  fun _ -> Some (SYM_PRINT));
    (keyword_regexp "struct", fun _ -> Some (SYM_STRUCT));
    (char_regexp '.', fun _ -> Some (SYM_POINT));
    (char_regexp '+', fun _ -> Some (SYM_PLUS));
    (char_regexp '-', fun _ -> Some (SYM_MINUS));
    (char_regexp '*', fun _ -> Some (SYM_ASTERISK));
    (char_regexp '/', fun _ -> Some (SYM_DIV));
    (char_regexp '%', fun _ -> Some (SYM_MOD));
    (char_regexp '{', fun _ -> Some (SYM_LBRACE));
    (char_regexp '}', fun _ -> Some (SYM_RBRACE));
    (char_regexp '[', fun _ -> Some (SYM_LBRACKET));
    (char_regexp ']', fun _ -> Some (SYM_RBRACKET));
    (char_regexp '(', fun _ -> Some (SYM_LPARENTHESIS));
    (char_regexp ')', fun _ -> Some (SYM_RPARENTHESIS));
    (char_regexp ';', fun _ -> Some (SYM_SEMICOLON));
    (char_regexp ',', fun _ -> Some (SYM_COMMA));
    (char_regexp '=', fun _ -> Some (SYM_ASSIGN));
    (keyword_regexp "==", fun _ -> Some (SYM_EQUALITY));
    (keyword_regexp "!=", fun _ -> Some (SYM_NOTEQ));
    (char_regexp '<', fun _ -> Some (SYM_LT));
    (char_regexp '>', fun _ -> Some (SYM_GT));
    (keyword_regexp "<=", fun _ -> Some (SYM_LEQ));
    (keyword_regexp ">=", fun _ -> Some (SYM_GEQ));
    (Cat(letter_regexp, Star identifier_material), fun s -> Some (SYM_IDENTIFIER s));
    (* end TODO *)
    (Cat(keyword_regexp "//",
         Cat(Star (char_range (List.filter (fun c -> c <> '\n') alphabet)),
             Alt (char_regexp '\n', Eps))),
     fun _ -> None);
    (Cat(keyword_regexp "/*",
         Cat(
           Cat (Star (Alt (
               char_range (List.filter (fun c -> c <> '*') alphabet),
               Cat (Star(char_regexp '*'),
                    plus(char_range (List.filter (fun c -> c <> '/' && c <> '*') alphabet)))
             )), Star (char_range ['*'])),
           keyword_regexp "*/")),
     fun _ -> None);
    (Cat (char_regexp '\'',
          Cat (char_range (List.filter (fun c -> c <> '\'' && c <> '\\') alphabet),
               char_regexp '\'')),
     fun s ->
       match String.get s 1 with
       | a -> Some (SYM_CHARACTER a)
       | exception Invalid_argument _ -> Some (SYM_CHARACTER 'a')
    );
    (Cat (char_regexp '\'', Cat (char_regexp '\\',
          Cat (char_range (char_list_of_string "\\tn0'"),
               char_regexp '\''))),
     fun s -> match String.get s 2 with
         | '\\' -> Some (SYM_CHARACTER '\\')
         | 'n' -> Some (SYM_CHARACTER '\n')
         | 't' -> Some (SYM_CHARACTER '\t')
         | '\'' -> Some (SYM_CHARACTER '\'')
         | '0' -> Some (SYM_CHARACTER 'a')
         | _ -> None
         | exception _ -> Some (SYM_CHARACTER 'a')
    );
    (Cat (char_regexp '"',
          Cat (Star (
              Alt (
                char_range (List.filter (fun c -> c <> '"' && c <> '\\') alphabet),
                Cat (char_regexp '\\', char_range (char_list_of_string "tn0\\\""))
              )
            ),
               char_regexp '"')),
     fun s ->
       Some
         (SYM_STRING
            (Stdlib.Scanf.unescaped (String.sub s 1 (String.length s - 2)))));
    (char_range (char_list_of_string " \t\n"), fun _ -> None);
    (plus digit_regexp, fun s -> Some (SYM_INTEGER (int_of_string s)));
    (Eps, fun _ -> Some (SYM_EOF))
  ]
