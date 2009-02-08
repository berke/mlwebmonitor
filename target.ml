(* Target *)

open Bool;;

type action =
| Mail of string * string list
| Echo of string
| Exec of string
;;

type source =
| Url of string
| Cmd of string
;;

type regexp = string * Pcre.regexp;;

type entry = {
  name : string;
  delay : float;
  bad_max : int;
  test : (source * regexp boolean) boolean;
  action : action list;
  timeout : int;
  mutable bad_count : int;
  mutable last_test : float;
  mutable is_good : bool;
};;

type tests = entry list;;
