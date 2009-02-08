(* Bool *)

type 'a boolean =
    And of 'a boolean list
  | Or of 'a boolean list
  | Not of 'a boolean
  | Atom of 'a
  | True
  | False
val eval : ('a -> bool) -> 'a boolean -> bool
val iter : ('a -> unit) -> 'a boolean -> unit
val map : ('a -> 'b) -> 'a boolean -> 'b boolean
