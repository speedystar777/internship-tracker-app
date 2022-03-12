open Tracker
open ANSITerminal
open Commands
open Record

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
let string_of_list =
  let rec helper acc = function
    | [] -> acc ^ "]"
    | [ h ] -> helper (acc ^ h) []
    | h :: t -> helper (acc ^ h ^ ", ") t
in helper "["

type id = | Name of string | Date of string | Status of string

let grab_id_phrase = function 
| Name n -> n
| Date d -> d
| Status s -> s

let process s = s |> String.split_on_char ' ' |> List.filter (fun x -> x <> "") |> List.map String.lowercase_ascii |> String.concat " "

let valid_s s = s = "new" || s = "applied" || s = "interviewed" || s = "accepted" || s = "rejected"

let valid_d d = true

let date_error = "The date you have entered is invalid. The date must be in the form mm/dd/yy"
let status_error = "Please enter a valid status from: new, applied, interviewed, accepted, rejected"

let rec build_entry (id : id) acc valid error : string list = 
  if not valid then (print_endline error);
  let s = grab_id_phrase id in print_endline ("Please enter the " ^ s ^ " of the internship");
match id with
| Name n -> build_entry (Date "date") ((process (read_line ())) :: acc) true ""
| Date d -> if valid_d d then build_entry (Status "status") ((process (read_line ())) :: acc) true "" else build_entry (Date "date") acc false date_error
| Status s -> let s = (process (read_line ())) in if valid_s s then s :: acc else build_entry (Status "status") acc false status_error

let process_lst_to_entry = function 
| [s; d; n] -> create_entry n d s 
| _ -> raise (Invalid_argument "invalid")

let rec make_tracker msg =
  match parse msg with
  | Add -> build_entry (Name "name") [] true "" |> string_of_list |> print_endline;
  | Delete s -> print_endline ""; ()
  | View -> print_endline ""; ()

(* let malformed =
  "I don't understand your command. Please enter a valid command" *)

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
