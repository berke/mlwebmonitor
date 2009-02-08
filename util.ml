(* Util *)

let info x = output_string stderr x; output_char stderr '\n'; flush stderr;;
let fp = Printf.fprintf;;
let sf = Printf.sprintf;;
let se = String.escaped;;
let unwrap_option = function None -> raise Not_found | Some(x) -> x ;;
(*** with_output_to_file *)
let with_output_to_file fn f =
  let oc = open_out fn in
  try
    let y = f oc in
    close_out oc;
    y
  with
  | x ->
    close_out oc;
    raise x
;;
(* ***)
(*** split_at *)
let split_at c s =
  let m = String.length s in
  let i = String.index s c in
  (String.sub s 0 i,
  String.sub s (i + 1) (m - i - 1))
;;
(* ***)
(*** print_timestamp *)
let print_timestamp oc () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.fprintf oc "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec
;;
(* ***)
(*** with_timeout *)
let with_timeout timeout f x =
  try
    ignore (Unix.alarm timeout);
    let z = f x in
    ignore (Unix.alarm 0);
    z
  with
  | y ->
      ignore (Unix.alarm 0);
      raise y
;;
(* ***)
(*** reorder_assoc_list *)
let reorder_assoc_list order assoc =
  List.rev (List.fold_left (fun r k -> try (k,List.assoc k assoc)::r with Not_found -> r) [] order)
;;
(* ***)
let msg x = Printf.printf "%a %s\n%!" print_timestamp () x;;
