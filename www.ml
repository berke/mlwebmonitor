(* Www *)

open Neturl;;
open Nethtml;;
module HC = Http_client;;
open Util;;

exception Found of string;;

let http_url = Hashtbl.find common_url_syntax "http";;

(*** concat_urls *)
let concat_urls u v =
  (* info (sf "Concat [%s] with [%s]" u v); *)
  string_of_url
  (apply_relative_url
  (url_of_string http_url u)
  (url_of_string (partial_url_syntax http_url) v))
;;
(* ***)

let document_of_document_list x = Element("document-list",[],x);;

let rec iter_over_data f = function
  | Data(x) -> f x
  | Element(what,attrs,cont) -> List.iter (iter_over_data f) cont
;;

let rec iter_over_data_list f l = List.iter (iter_over_data f) l;;

(*** scan_elements *)
let rec scan_elements elt specs f = function
  | Data(_) -> ()
  | Element(what,attrs,cont) ->
      if what = elt &&
        List.for_all (fun (x,y) -> try y (List.assoc x attrs) with Not_found -> false) specs
      then
        f what attrs cont
      else
        List.iter (scan_elements elt specs f) cont
;;
(* ***)

let scan_elements_list elt specs f l = List.iter (scan_elements elt specs f) l;;

let select_elements elt = List.filter (function Data(_) -> false | Element(x,y,z) -> x = elt)

(*** dump_document *)
let dump_document ch doc =
  let o = Format.formatter_of_out_channel ch in
  let g = Format.fprintf in
  let rec h f sep flag = function
    | x::r ->
        if flag then g o "%s@ " sep;
        f x;
        h f sep true r
    | [] -> ()
  in let rec f = function
    Data(x) -> g o "\027[34mData\027[0m(\"\027[36m%s\027[0m\")" (se x)
  | Element(x,y,z) ->
      g o "@[<hov 2>\027[31mElement\027[0m(\"\027[31m%s\027[0m\",[" x;
      h (fun (x,y) -> g o "\027[34m%s\027[0m=\"%s\"" x (se y)) ";" false y;
      g o "],@,[";
      h f ";" false z;
      g o "])@]";
  in
  f doc; g o "@?"
;;
(* ***)
(*** add_default_headers *)
let add_default_headers m re co =
  m # set_req_header "User-Agent" "Googlebot/2.1 (+http://www.google.com/bot.html)";
  m # set_req_header "Accept" "image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, image/png, */*";
  m # set_req_header "Accept-Language" "en";
  m # set_req_header "Accept-Charset" "iso-8859-1";
  (match re with None -> () | Some(x) -> m # set_req_header "Referer" x);
  List.iter (fun (a,b) -> m # set_req_header "Cookie" (a^"="^b)) co
;;
(* ***)
(*** obtain_document *)
let obtain_document cookies p re url = 
  (*info (sf "Retrieving %s..." url);*)
  let g = new HC.get url in
  (* (match re with None -> () | Some(x) -> g # set_req_header "Referer" x); *)
  g # set_req_header "User-Agent" "monitor/3.14159265358";
  (* List.iter (fun (a,b) -> g # set_req_header "Cookie" (a^"="^b)) cookies; *)
  add_default_headers g re cookies;
  p # add g; p # run ();
  let (x,code,y) = g # dest_status () in
  match code with
  200 ->
    (*info (sf "Retrieval ok %s %s." x y);*)
    let body = g # get_resp_body () in
    let nioc = new Netchannels.input_string body in
    let doc_list = 
      Nethtml.parse
        ~dtd:html40_dtd
        ~return_declarations:false
        ~return_pis:false
        ~return_comments:false
        nioc
    in
    document_of_document_list
      (*(decode ~subst:(fun p -> Printf.sprintf "&#%d;" p))*)
      doc_list
  | x -> failwith (sf "obtain_document: got code %d" x)
(* ***)
(*** add_to_pipe_and_parse *)
let add_to_pipe_and_parse p t url =
  p # add t; p # run ();
  let (x,code,y) = t # dest_status () in
  match code with
  | 200 ->
    (*info (sf "Retrieval ok %s %s." x y);*)
    let body = t # get_resp_body () in
    ignore body
  | x -> failwith (sf "add_to_pipe_and_parse: got code %d" x)
;;
(* ***)
(*** parse_cookie *)
let parse_cookie a =
  let (b,_) = split_at ';' a in
  split_at '=' b 
;;
(* ***)
