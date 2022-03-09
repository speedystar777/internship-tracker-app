open Tracker
open ANSITerminal
open Commands

(* let malformed = "I don't understand your command. Please enter a\n\n
   valid command"

   let get_score st = "Your score is " ^ string_of_int (current_score
   st)

   let not_finished adv st = match exits adv (current_room_id st) with |
   [] -> false | _ :: t -> true

   let rec play msg adv st = print_endline msg; if not_finished adv st
   then ( print_string "> "; match read_line () with | cmd -> ( match
   parse cmd with | Go lst -> ( let ex = cmd_string lst in match go ex
   adv st with | Legal s -> play (get_description adv s) adv s | Illegal
   -> play illegal adv st) | Score -> play (get_score st) adv st | Quit
   -> print_endline "Bye!"; exit 0 | exception Malformed -> play
   malformed adv st | exception Empty -> play "Please enter an exit" adv
   st)) else print_endline (get_score st ^ ". Thanks for playing!");
   exit 0

   (** [play_game f] starts the adventure in file [f]. *) let
   make_tracker f = match get_tracker f with | exception Sys_error x ->
   print_endline (f ^ " is not a valid file. Please restart the\n\ \
   tracker\n\ \ with a valid file") | adv -> let st = init_state adv in
   play (get_description adv st) adv st*)

(** [main ()] prompts for the game to play, then starts it. *)

let field_list = [ "name"; "date"; "status" ]

let rec build_entry id str acc =
  match id with
  | "name" ->
      print_endline "Please enter the deadline of the internship";
      let st = read_line () in
      build_entry "date" st (str :: acc)
  | "date" ->
      print_endline
        "Please enter the current status fo your application";
      let st = read_line () in
      build_entry "status" st (str :: acc)
  | "status" -> str :: acc
  | _ -> []

let rec make_tracker msg =
  match parse msg with
  | Add -> ()
  | Delete s -> ()
  | View -> ()

let malformed =
  "I don't understand your command. Please enter a valid command"

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome\n to the Internship Application Tracker interface.\n";
  print_endline
    "Please\n enter the action you would like to\n\n   complete.\n";
  print_endline
    "\n\nThe options include 'add', 'delete [entry name]', or 'view'.\n";
  print_endline "> ";
  match read_line () with
  | exception End_of_file -> ()
  | command -> make_tracker command

(* Execute the game engine. *)
let () = main ()
