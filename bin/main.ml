open Tracker
open ANSITerminal
open Commands
open Entry
open Entrylist

let string_of_list =
  let rec helper acc = function
    | [] -> acc
    | h :: t -> helper (acc ^ "[" ^ h ^ "] \n") t
  in
  helper ""

let process s =
  s
  |> String.split_on_char ' '
  |> List.filter (fun x -> x <> "")
  |> List.map String.lowercase_ascii
  |> String.concat " "

let date_error =
  "The date you have entered is invalid. The date must be in the form \
   mm/dd/yy"

let status_error =
  "Please enter a valid status from: new, applied, interviewed, \
   accepted, rejected"

let dup_message = "\nWARNING: You cannot enter duplicate entry names."

let command_message =
  "\n\nPlease enter the action you would like to complete."
  ^ "\n\
     The options include 'add', 'delete [name of internship]', 'view', \
     'update [name of internship]', 'quit'.\n"

let notfound_message =
  "\nWARNING: The entry you are trying to query does not exist."

let deleted_msg name = "The deletion of " ^ name ^ " was successful!"
let cmd_string lst = String.concat " " lst

let rec build_entry id acc valid error : string list =
  if not valid then print_endline error;
  let s = grab_id_phrase id in
  print_endline ("Please enter the " ^ s ^ " of the internship");
  match id with
  | Name n ->
      build_entry (Date "date") (process (read_line ()) :: acc) true ""
  | Date d ->
      if valid_d d then
        build_entry (Status "status")
          (process (read_line ()) :: acc)
          true ""
      else build_entry (Date "date") acc false date_error
  | Status s ->
      let s = process (read_line ()) in
      if valid_s s then s :: acc
      else build_entry (Status "status") acc false status_error

let process_lst_to_entry = function
  | [ s; d; n ] -> create_entry n d s
  | _ -> raise (Invalid_argument "invalid")

let rec process_update_acc st entry acc =
  match acc with
  | ("name", n) :: t ->
      let new_entry = create_entry n (date entry) (status entry) in
      process_update_acc (change_name entry st new_entry) new_entry t
  | ("date", d) :: t ->
      let new_entry = create_entry (name entry) d (status entry) in
      process_update_acc (change_date entry st new_entry) new_entry t
  | ("status", s) :: t ->
      let new_entry = create_entry (name entry) (date entry) s in
      process_update_acc (change_status entry st new_entry) new_entry t
  | [] -> st
  | _ -> raise (Invalid_argument "invalid")

let rec update_entry acc valid error st entry_name =
  if not valid then print_endline error;
  print_endline
    "What would you like to change? Please enter: name, date, status, \
     cancel to cancel all changes, or done to finish changes.";
  let s = read_line () in
  match s with
  | "name" ->
      print_endline "Please enter the new name of the selected entry:";
      let n = process (read_line ()) in
      if List.exists (fun x -> String.compare n x = 0) (entry_names st)
      then update_entry acc false dup_message st n
      else update_entry (acc @ [ ("name", n) ]) true "" st entry_name
  | "date" ->
      print_endline "Please enter the new date of the selected entry:";
      let s = process (read_line ()) in
      if valid_d s then
        update_entry (acc @ [ ("date", s) ]) true "" st entry_name
      else update_entry acc false date_error st entry_name
  | "status" ->
      print_endline
        "Please enter the new status of the selected entry from: new, \
         applied, interviewed, accepted, rejected";
      let s = process (read_line ()) in
      if valid_s s then
        update_entry (acc @ [ ("status", s) ]) true "" st entry_name
      else update_entry acc false status_error st entry_name
  | "done" -> acc
  | "cancel" -> []
  | _ ->
      update_entry acc false
        "The field name was not recognized. Please enter a valid field \
         name."
        st entry_name

let rec sort_by st valid error : string =
  if not valid then print_endline error;
  print_endline
    "What would you like to sort by? Please enter either 'name', \
     'status', or 'none'.";
  match read_line () with
  | "name" -> string_of_list (print_t (sort_by_name st))
  | "status" -> string_of_list (print_t (sort_by_status st))
  | "none" -> string_of_list (print_t st)
  | _ -> sort_by st false "Could not recognize your command."

let rec make_tracker msg st =
  print_endline msg;
  Printf.printf "> ";
  match parse (read_line ()) with
  | Add -> (
      try
        add
          (build_entry (Name "name") [] true "" |> process_lst_to_entry)
          st
        |> make_tracker command_message
      with Duplicate ->
        make_tracker (dup_message ^ command_message) st)
  | Delete s -> (
      try
        let str = cmd_string s in
        delete str st |> make_tracker (deleted_msg str ^ command_message)
      with NotFound ->
        make_tracker (notfound_message ^ command_message) st)
  | View ->
      print_endline (sort_by st true "");
      make_tracker command_message st
  | Update s -> (
      try
        let entry_name = cmd_string s in
        let entry = find_entry entry_name st in
        update_entry [] true "" st entry_name
        |> process_update_acc st entry
        |> make_tracker command_message
      with NotFound ->
        make_tracker (notfound_message ^ command_message) st)
  | Quit -> exit 0
  | exception Malformed ->
      print_endline "WARNING: Could not recognize your command";
      make_tracker command_message st

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "Welcome to the Internship Application Tracker Interface!";
  make_tracker command_message []

let () = main ()
