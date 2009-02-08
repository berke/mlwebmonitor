(* Action *)

open Util;;
open Target;;

let perform_action e =
  List.iter
    begin function
      | Mail(what, whom) ->
          List.iter
            begin fun who ->
              let subject = sf "[Monitor] %s is %s" what (if e.is_good then "up" else "down") in
              let contact = !Opt.contact in
              let cmd =
                let b = Buffer.create 80 in
                Buffer.add_substitute b
                  begin function
                    | "WHO" -> Filename.quote who
                    | "SUBJECT" -> Filename.quote subject
                    | "CONTACT" -> Filename.quote !Opt.contact
                    | x -> msg (sf "Ignoring invalid substitution variable %S" x);
                           ""
                  end
                  !Opt.sendmail;
                Buffer.contents b
              in
              let oc = Unix.open_process_out cmd in
              fp oc "To: %s\n" who;
              fp oc "Subject: %s\n" subject;
              fp oc "\n";
              fp oc "This is an automatic monitoring script.\n";
              fp oc "Contact %s if there are any problems with it.\n" contact;
              fp oc "\n";
              fp oc "--\n";
              fp oc "Monitor\n";
              fp oc ".\n";
              match Unix.close_process_out oc with
              | Unix.WEXITED 0 -> ()
              | _ -> msg (sf "Problem mailing to %s." who)
            end
            whom
      | Echo x -> msg (sf "Echo: %S" x)
      | Exec cmd ->
          msg (sf "Executing %S" cmd);
          let r = Sys.command cmd in
          msg (sf "Result is %d" r)
    end
    e.action
;;
