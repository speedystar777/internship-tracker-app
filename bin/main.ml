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
     'update [name of internship]', \n\
    \ 'notes for [name of internship]', 'quit'.\n"

let notfound_message =
  "\nWARNING: The entry you are trying to query does not exist."

let deleted_msg name = "\nThe deletion of " ^ name ^ " was successful!"
let cmd_string lst = String.concat " " lst
let notes_error_msg = "\nCould not recognize your command."

let name_error =
  "\nCannot enter an empty string or just spaces for internship name"

let rec build_entry id acc valid error : string list =
  if not valid then
    ANSITerminal.print_string [ ANSITerminal.red ] (error ^ "\n");
  let s = grab_id_phrase id in
  print_endline ("Please enter the " ^ s ^ " of the internship");
  match id with
  | Name n ->
      if valid_n n then
        build_entry (Date "date")
          (process (read_line ()) :: acc)
          true ""
      else build_entry (Name "name") acc true name_error
  | Date d ->
      if valid_d d then
        build_entry (Status "status")
          (process (read_line ()) :: acc)
          true ""
      else build_entry (Date "date") acc false date_error
  | Status s ->
      let s = process (read_line ()) in
      if valid_s s then (
        print_endline
          "Please enter if you would like to include notes for your \
           internship: type 'yes' or 'no'";
        let r = process (read_line ()) in
        match r with
        | "yes" -> build_entry (Notes "notes") (s :: acc) true ""
        | _ -> "no" :: s :: acc)
      else build_entry (Status "status") acc false status_error
  | Notes n -> process (read_line ()) :: acc

let process_lst_to_entry = function
  | [ nt; s; d; n ] ->
      create_entry n d s
        (match nt with
        | "no" -> No
        | s -> Yes s)
  | _ -> raise (Invalid_argument "invalid")

let rec process_update_acc st entry acc =
  match acc with
  | ("name", n) :: t ->
      let new_entry =
        create_entry n (date entry) (status entry) (notes entry)
      in
      process_update_acc (change entry st new_entry) new_entry t
  | ("date", d) :: t ->
      let new_entry =
        create_entry (name entry) d (status entry) (notes entry)
      in
      process_update_acc (change entry st new_entry) new_entry t
  | ("status", s) :: t ->
      let new_entry =
        create_entry (name entry) (date entry) s (notes entry)
      in
      process_update_acc (change entry st new_entry) new_entry t
  | ("notes", "cancel") :: t -> process_update_acc st entry t
  | ("notes", nts) :: t ->
      let new_entry =
        create_entry (name entry) (date entry) (status entry) (Yes nts)
      in
      process_update_acc (change entry st new_entry) new_entry t
  | [] -> st
  | _ -> raise (Invalid_argument "invalid")

let rec update_entry acc valid error st entry_name =
  if not valid then
    ANSITerminal.print_string [ ANSITerminal.red ] (error ^ "\n");
  print_endline
    "What would you like to change? Please enter: name, date, status, \
     notes, cancel to cancel all changes, or done to finish changes.";
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
  | "notes" ->
      print_endline
        "Please enter the new notes for the selected entry or type \
         cancel";
      let s = process (read_line ()) in
      update_entry (acc @ [ ("notes", s) ]) true "" st entry_name
  | "done" -> acc
  | "cancel" -> []
  | _ ->
      update_entry acc false
        "The field name was not recognized. Please enter a valid field \
         name."
        st entry_name

let rec sort_by st valid error : string =
  if not valid then ANSITerminal.print_string [ ANSITerminal.red ] error;
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
        ANSITerminal.print_string [ ANSITerminal.red ]
          (dup_message ^ "\n");
        make_tracker command_message st)
  | Delete s -> (
      try
        let str = cmd_string s in
        delete str st |> make_tracker (deleted_msg str ^ command_message)
      with NotFound ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          (notfound_message ^ "\n");
        make_tracker command_message st)
  | View ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        (sort_by st true "" ^ "\n");
      make_tracker command_message st
  | Update s -> (
      try
        let entry_name = cmd_string s in
        let entry = find_entry entry_name st in
        update_entry [] true "" st entry_name
        |> process_update_acc st entry
        |> make_tracker command_message
      with NotFound ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          (notfound_message ^ "\n");
        make_tracker command_message st)
  | Quit -> exit 0
  | Notes n -> (
      try
        let entry_name = cmd_string n in
        let entry = find_entry entry_name st in
        print_endline ("Your notes for entry " ^ entry_name ^ " are: ");
        print_endline (notes_string entry);
        make_tracker command_message st
      with NotFound ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          (notfound_message ^ "\n");
        make_tracker command_message st)
  | exception Malformed ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "WARNING: Could not recognize your command\n";
      make_tracker command_message st

let main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Welcome to the Internship Application Tracker Interface!";
  make_tracker command_message []

let () = main ()
