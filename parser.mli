(* Parser *)

exception Error of string
exception At_line of int * exn
val load :
  string ->
  (string * (Target.source * (string * Pcre.regexp) Bool.boolean) list * (int * Target.action list)) list
