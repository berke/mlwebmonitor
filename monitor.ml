(* Monitor *)
(* Copyright (C)2005-2006 Berke Durak *)
(* Released under the GNU General Public License, version 2 or later. *)

open Util;;
open Bool;;
open Target;;

exception Alarm;;

let continue = ref false;;

let default_entry = {
  name = "unnamed";
  delay = 300.0;
  bad_max = 1;
  test = False;
  action = [];
  bad_count = 0;
  last_test = 0.0;
  is_good = true;
  timeout = 30
};;

module Do =
  struct
    (*** grep_document *)
    let grep_document url regex =
      let p = new Www.HC.pipeline in
      let rex = Pcre.regexp ~study:true regex in
      let doc = Www.obtain_document [] p None url in
      (*with_output_to_file "unfiltered.txt" (fun f -> dump_document f doc); *)
      Www.iter_over_data
        begin fun w ->
          if Pcre.pmatch ~rex w then
            Printf.printf "Matched %S.\n" w
          else
            ()
        end
        doc
    ;;
    (* ***)
    (*** bool_command *)
    let bool_command cmd formula =
      try
        let ic = Unix.open_process_in cmd in
        let formula = Bool.map (fun (_,rex) -> (rex, ref false)) formula in
        (*with_output_to_file "unfiltered.txt" (fun f -> dump_document f doc); *)
        try
          while true do
            let l = input_line ic in
            ignore (Bool.iter (fun (rex, result) ->
              if Pcre.pmatch ~rex l then
                begin
                  result := true
                end) formula)
          done;
          assert false
        with
        | End_of_file ->
            ignore (Unix.close_process_in ic);
            Bool.eval (fun (rex, result) -> !result) formula;
      with
      | x ->
          msg (sf "While executing %S, exception %s" cmd (Printexc.to_string x));
          false
    ;;
    (* ***)
    (*** bool_document *)
    let bool_document url formula =
      if !Opt.debug then
        Printf.eprintf "Checking URL %S\n%!" url;
      let p = new Www.HC.pipeline in
      try
        (*let formula = Bool.map (Pcre.regexp ~study:true) formula in*)
        let doc = Www.obtain_document [] p None url in
        (*with_output_to_file "unfiltered.txt" (fun f -> dump_document f doc); *)
        let res =
          Bool.eval
            (fun (rs,rex) ->
              let result = ref false in
              Www.iter_over_data
                begin fun w ->
                  let outcome = Pcre.pmatch ~rex w in
                  if !Opt.debug then
                    Printf.eprintf "Match(%S,%S)=%b\n%!" rs w outcome;
                  if outcome then
                    begin
                      result := true;
                    end
                end
                doc;
              !result) formula
        in
        p#reset ();
        res
      with
      | x ->
          p#reset ();
          msg (sf "While checking %S, exception %s" url (Printexc.to_string x));
          false
    ;;
    (* ***)
    (*** perform_test *)
    let perform_test e =
       try
         with_timeout e.timeout
           begin
             Bool.eval
               begin function
                 | Url url,formula -> bool_document url formula
                 | Cmd cmd,formula -> bool_command cmd formula
               end
           end
           e.test
       with
       | Alarm -> false
    ;;
    (* ***)
    (*** check *)
    let check tests =
      List.iter
        begin fun e ->
          let t = Unix.gettimeofday () in
          if e.last_test = 0.0 or t -. e.last_test > e.delay then
            begin 
              msg (sf "Testing for %S" e.name);
              e.last_test <- t;
              let result = perform_test e in
              if e.is_good then
                begin
                  if result then
                    begin
                      e.bad_count <- 0;
                      msg (sf "%S is okay" e.name)
                    end
                  else
                    begin
                      e.bad_count <- e.bad_count + 1;
                      if e.bad_count >= e.bad_max then
                        begin
                          msg (sf "Houston, we have a problem on %S" e.name);
                          e.is_good <- false;
                          if not !continue then Action.perform_action e
                        end
                      else
                        msg (sf "There seems to be a problem on %S, count = %d" e.name e.bad_count);
                    end
                end
              else
                begin
                  if result then
                    begin
                      msg (sf "%S is good again" e.name);
                      e.bad_count <- 0;
                      e.is_good <- true;
                      if not !continue then Action.perform_action e
                    end
                  else
                    begin
                      e.bad_count <- e.bad_count + 1;
                      if e.bad_count >= e.bad_max then
                        begin
                          msg (sf "Houston, we still have a problem on %S" e.name);
                          e.is_good <- false;
                        end
                      else
                        msg (sf "There seems to be a problem on %S, count = %d" e.name e.bad_count);
                    end
                end
            end
          else
            ()
        end
        tests
    ;;
    (* ***)
    (*** forever *)
    let forever tests =
      Sys.set_signal
        Sys.sigalrm
        (Sys.Signal_handle(fun _ -> raise Alarm));
      while true do
        check tests;
        continue := false;
        Unix.sleep 5
      done
    ;;
    (* ***)
    (*** dump_document *)
    let dump_document url =
      let p = new Www.HC.pipeline in
      let doc = Www.obtain_document [] p None url in
      (*with_output_to_file "unfiltered.txt" (fun f -> dump_document f doc); *)
      Www.dump_document stdout doc
    ;;
    (* ***)
  (*** testify *)
  let testify 
    (l : (string *
          (Target.source * (string * Pcre.regexp) Bool.boolean) list *
          (int * Target.action list))
            list)
    =
    List.map
      begin fun (name, tests, (bad_max, actions)) ->
        let t = And(List.map (fun x -> Atom x) tests) in
        { default_entry with
          delay = !Opt.delay;
          bad_max = bad_max;
          action = actions;
          name = name;
          test = t }
      end
      l
  ;;
  (* ***)
  end
;;

(*** spec *)
let spec = [
  "-continue", Arg.Set continue, " continue processing ; don't re-send alerts";
  "-dump", Arg.String(fun url -> Do.dump_document url), "<url> Retrieve HTML document and dump its syntax tree.";
  "-config-file", Arg.Set_string Opt.config_file, sf "<path> Set configuration file (%s)" !Opt.config_file;
  "-debug", Arg.Set Opt.debug, " Enable debugging";
  "-grep",
    Arg.Tuple(
      let url = ref "" in
      [Arg.Set_string url;
       Arg.String(fun regex -> Do.grep_document !url regex)]),
    "<url> <regex>  Download and grep document.";
  "-delay", Arg.Set_float Opt.delay, "<delay> Delay between tests in seconds.";
  "-sendmail", Arg.Set_string Opt.sendmail, sf "<sendmail> Set sendmail command (%S)" !Opt.sendmail
]
(* ***)

let _ =
  Arg.parse spec (fun _ -> ()) (sf "Usage: %s [options]" (Filename.basename Sys.argv.(0)));
  let cfg = Parser.load !Opt.config_file in
  let tests = Do.testify cfg in
  Do.forever tests
;;
