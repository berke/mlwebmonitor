(* Parser *)
(* Copyright (C)2005-2006 Berke Durak *)
(* Released under the GNU General Public License, version 2 or later. *)
(* Hand-crafted on 2006-05-24 *)

open Bool;;
open Target;;
open Util;;

exception Error of string;;

(*** comment_killer_line_counter *)
let comment_killer_line_counter lc s =
  let rec state0 = parser
  | [< 'c; s >] ->
    (match c with
    | '#' -> state1 s
    | '\n' -> incr lc; [< 'c; state0 s >]
    | _ -> [< 'c; state0 s >])
  | [< >] -> [< >]
  and state1 = parser
  | [< 'c; s >] ->
    if c = '\n' then
      begin
      incr lc;
      [< '' '; state0 s >]
      end
    else
      state1 s
  | [< >] -> [< >]
  in
  state0 s
;;
(* ***)

type token =
  | AND
  | CHECK
  | COMMA
  | COMMAND
  | CONSECUTIVE
  | ECHO
  | EXECUTE
  | FAILURE
  | FAILURES
  | FALSE
  | HAS
  | INT of int
  | MAIL
  | MONITOR
  | MORE
  | NOT
  | ON
  | OR
  | OUTPUTS
  | REGEXP of string * char list
  | STRING of string
  | SUBJECT
  | THAN
  | THANKS
  | THAT
  | THEN
  | TO
  | TRUE
  | URL
  | WITH
  | LPAREN
  | RPAREN
;;

let string_of_token = function
  | AND -> "AND"
  | CHECK -> "CHECK"
  | COMMA -> "COMMA"
  | COMMAND -> "COMMAND"
  | CONSECUTIVE -> "CONSECUTIVE"
  | ECHO -> "ECHO"
  | EXECUTE -> "EXECUTE"
  | FAILURE -> "FAILURE"
  | FAILURES -> "FAILURES"
  | FALSE -> "FALSE"
  | HAS -> "HAS"
  | INT i -> "INT("^(string_of_int i)^")"
  | MAIL -> "MAIL"
  | MONITOR -> "MONITOR"
  | MORE -> "MORE"
  | NOT -> "NOT"
  | ON -> "ON"
  | OR -> "OR"
  | OUTPUTS -> "OUTPUTS"
  | REGEXP(r,o) -> "REGEXP("^r^",["^(String.concat ";" (List.map (String.make 1) o))^"])"
  | STRING s -> "STRING("^s^")"
  | SUBJECT -> "SUBJECT"
  | THAN -> "THAN"
  | THANKS -> "THANKS"
  | THAT -> "THAT"
  | THEN -> "THEN"
  | TO -> "TO"
  | TRUE -> "TRUE"
  | URL -> "URL"
  | WITH -> "WITH"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
;;

let keywords_list =
[
  "and", AND;
  "check", CHECK;
  "comma", COMMA;
  "command", COMMAND;
  "consecutive", CONSECUTIVE;
  "echo", ECHO;
  "execute", EXECUTE;
  "failure", FAILURE;
  "failures", FAILURES;
  "false", FALSE;
  "has", HAS;
  "mail", MAIL;
  "monitor", MONITOR;
  "more", MORE;
  "not", NOT;
  "on", ON;
  "or", OR;
  "outputs", OUTPUTS;
  "subject", SUBJECT;
  "than", THAN;
  "thanks", THANKS;
  "that", THAT;
  "then", THEN;
  "to", TO;
  "true", TRUE;
  "url", URL;
  "with", WITH;
  ]
;;

(*** lex *)
let lex s =
  let b = Buffer.create 16 in
  let rec lex0 = parser
  | [< ''/'; s >] -> lex_regexp0 s
  | [< ''('; s >] -> [< 'LPAREN; lex0 s >]
  | [< '')'; s >] -> [< 'RPAREN; lex0 s >]
  | [< '','; s >] -> [< 'COMMA; lex0 s >]
  | [< ''"'; s >] -> lex_string0 s
  | [< '( '0'..'9' as c); s >] -> lex_int0 ((Char.code c) land 15) s
  | [< ' ('a'..'z' as c); s >] ->
      Buffer.add_char b c;
      lex_keyword0 s
  | [< ' (' '|'\t'|'\n'|'\r'); s >] -> lex0 s
  | [< s >] -> Stream.empty s; [< >]
  and lex_escapable = parser
  | [< ''\\'; s >] ->
      begin
        match s with parser
        | [< ''t' >] -> '\t'
        | [< ''n' >] -> '\n'
        | [< ''r' >] -> '\r'
        | [< ''\\' >] -> '\\'
        | [< ''/' >] -> '/'
        | [< ''"' >] -> '"'
      end
  | [< 'c; s >] -> c
  and lex_string0 = parser
  | [< ''"'; s >] ->
      let w = Buffer.contents b in
      Buffer.clear b;
      [< '(STRING w); lex0 s >]
  | [< c = lex_escapable; s >] ->
      Buffer.add_char b c;
      lex_string0 s
  and lex_regexp0 = parser
  | [< ''/'; s >] ->
      let w = Buffer.contents b in
      Buffer.clear b;
      lex_regexp1 w [] s
  | [< c = lex_escapable; s >] ->
      Buffer.add_char b c;
      lex_regexp0 s
  and lex_regexp1 w o = parser
  | [< ' ('a'..'z' as c); s >] -> lex_regexp1 w (c :: o) s
  | [< s >] -> [< '(REGEXP(w,o)); lex0 s >]
  and lex_keyword0 = parser
  | [< ' ('a'..'z' as c); s >] ->
      Buffer.add_char b c;
      lex_keyword0 s
  | [< s >] ->
      let w = Buffer.contents b in
      Buffer.clear b;
      match
        try
          Some(List.assoc w keywords_list)
        with
        | Not_found -> None
      with
      | Some k -> [< 'k; lex0 s >]
      | None -> raise (Error(sf "Unknown token %S" w))
  and lex_int0 q = parser
  | [< '( '0'..'9' as c); s >] -> lex_int0 (10 * q + ((Char.code c) land 15)) s
  | [< s >] -> [< '(INT q); lex0 s >]
  in
  lex0 s
;;
(* ***)

let parse s =
  let rec do_program = parser
  | [< e = do_entry; s >] -> e :: do_program s
  | [< >] -> []
  and do_entry = parser
  | [< 'TO; 'MONITOR; '(STRING w); cl = do_checklist; a = do_actions; 'THANKS >] -> (w, cl, a)
  and do_checklist = parser
  | [< ck = do_check; s >] ->
      match s with parser
      | [< 'THEN; s >] -> ck :: (do_checklist s)
      | [< >] -> [ck]
  and do_check = parser
  | [< 'CHECK; 'THAT; src = do_source; '(HAS|OUTPUTS); x = do_expression >] -> (src, x)
  and do_source = parser
  | [< 'URL; '(STRING w) >] -> Url w
  | [< 'COMMAND; '(STRING w) >] -> Cmd w
  and do_actions = parser
  | [< fc = do_failure_condition; al = do_action_list >] -> (fc, al)
  and do_failure_condition = parser
  | [< 'ON; s >] ->
      match s with parser
      | [< 'MORE; 'THAN; '(INT i); 'CONSECUTIVE; '(FAILURE|FAILURES) >] -> i
      | [< 'FAILURE >] -> 0
  and do_action_list = parser
  | [< a = do_action; s >] ->
      match s with parser
      | [< 'THEN; s >] -> a :: (do_action_list s)
      | [< >] -> [a]
  and do_action ?(subject="No subject") = parser
  | [< 'WITH; 'SUBJECT; '(STRING subject); s >] -> do_action ~subject s
  | [< 'MAIL; rl = do_recipient_list >] -> Mail(subject, rl)
  | [< 'ECHO; '(STRING w) >] -> Echo w
  | [< 'EXECUTE; '(STRING w) >] -> Exec w
  and do_recipient_list = parser
  | [< '(STRING u); s >] ->
      match s with parser
      | [< 'AND; '(STRING v) >] -> [u;v]
      | [< 'COMMA; rl = do_recipient_list >] -> u :: rl
      | [< >] -> [u]
  and do_expression x = parse_s x
  and atomizer continuation = parser
  | [< 'NOT; rest >] ->
    atomizer
      begin fun x rest ->
        continuation (Not x) rest
      end
      rest
  | [< 'REGEXP(r,o); rest >] ->
      let flags =
        List.map
          begin function
            | 'i' -> `CASELESS
            | c -> raise (Error(sf "Bad regexp option char %C" c))
          end
          o
      in
      let re = Pcre.regexp ~study:true ~flags r in
      let a = Atom(r,re) in
      continuation a rest
  | [< 'LPAREN; y = parse_s; 'RPAREN; rest >] -> continuation y rest
  and parse_s1 x = parser
    | [< 'OR; y = parse_s >] -> Or[x;y]
    | [< 'AND; rest >] -> parse_t x rest
    | [< >] -> x
  and parse_t1 x y = parser
    | [< 'OR; z = parse_s >] -> Or[And[x;y]; z]
    | [< 'AND; rest >] -> parse_t (And[x;y]) rest
    | [< >] -> And[x;y]
  and parse_s x = atomizer parse_s1 x
  and parse_t x y = atomizer (parse_t1 x) y
  in
  do_program s
;;

exception At_line of int * exn;;

let dump line x =
  Stream.iter (fun a -> Printf.printf "%d %s\n" (1 + !line) (string_of_token a)) x
;;

let dump fn =
  let ic = open_in fn in
  let s0 = Stream.of_channel ic in
  let line = ref 0 in
  let s1 = comment_killer_line_counter line s0 in
  try
    let s2 = lex s1 in
    dump line s2;
    close_in ic
  with
  | x ->
      close_in ic;
      raise (At_line(!line, x))
;;

let load fn =
  let ic = open_in fn in
  let s0 = Stream.of_channel ic in
  let line = ref 0 in
  let s1 = comment_killer_line_counter line s0 in
  try
    let s2 = lex s1 in
    let p = parse s2 in
    close_in ic;
    p
  with
  | x ->
      close_in ic;
      raise (At_line(!line, x))
;;

let parse x = parse (lex (Stream.of_string x));;
