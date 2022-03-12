open Tracker
open ANSITerminal
open Commands
open Record

let string_of_list =
  let rec helper acc = function
    | [] -> acc
    | h :: t -> helper (acc ^ "[" ^ h ^ "] \n") t
in helper ""

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

let command_message =  "\n\nPlease enter the action you would like to complete." ^ "\nThe options include 'add', 'delete [entry name]', 'view', or 'quit'.\n"

let dup_message = "\nWARNING: You cannot enter duplicate entry names."

let rec make_tracker msg st =
  print_endline msg;
  Printf.printf "> ";
  match parse (read_line ()) with
  | Add -> (try add (build_entry (Name "name") [] true "" |> process_lst_to_entry) st |> make_tracker command_message with Duplicate -> make_tracker (dup_message ^ command_message) st)
  | Delete s -> print_endline ""; ()
  | View -> print_endline (string_of_list (print_t st)); make_tracker command_message st
  | Quit -> exit 0
  | exception Malformed -> print_endline "WARNING: Could not recognize your command"; make_tracker command_message st

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "Welcome to the Internship Application Tracker Interface!"; make_tracker command_message empty 

(* Execute the game engine. *)
let () = main ()
