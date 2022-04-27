open Tracker
open ANSITerminal
open Commands
open Entry
open Entrylist
open Contact
open Network
open Message_strings
open Network_main
open Date
open Calendar_main
open Yojson.Basic
open Create
open Read
open String

let clean r =
  List.filter (fun x -> x <> "") (String.split_on_char ' ' r)

let rec build_entry (id : Entry.id) acc valid error extra_msg :
    string list =
  if not valid then
    ANSITerminal.print_string [ ANSITerminal.red ] (error ^ "\n");
  let s = Entry.grab_id_phrase id in
  print_endline
    (("Please enter the " ^ s ^ " of the internship. ") ^ extra_msg);
  match id with
  | Name n ->
      if valid_n n then
        build_entry (Date "date")
          (process (read_line ()) :: acc)
          true "" "Please use the format dd/mm/yyyy."
      else build_entry (Name "name") acc true name_error ""
  | Date d -> begin
      try
        let d = date_string (create_date (process (read_line ()))) in
        build_entry (Status "status") (d :: acc) true "" status_msg
      with InvalidDate ->
        build_entry (Date "date") acc false date_error
          "Please use the format dd/mm/yyyy."
    end
  | Status s ->
      let s = process (read_line ()) in
      if valid_s s then (
        print_endline
          "Please enter if you would like to include notes for your \
           internship: type 'yes' or 'no'";
        let r = process (read_line ()) in
        match r with
        | "yes" -> build_entry (Notes "notes") (s :: acc) true "" ""
        | _ -> "no" :: s :: acc)
      else
        build_entry (Status "status") acc false status_error status_msg
  | Notes n -> process (read_line ()) :: acc

let process_lst_to_entry = function
  | [ nt; s; d; n ] ->
      create_entry n (create_date d) s
        (match nt with
        | "no" -> No
        | s -> Yes s)
  | _ -> raise (Invalid_argument "invalid")

let rec process_update_acc st entry acc =
  match acc with
  | ("name", n) :: t ->
      let new_entry =
        create_entry n (date entry) (status entry) (Entry.notes entry)
      in
      process_update_acc
        (Entrylist.change entry st new_entry)
        new_entry t
  | ("date", d) :: t ->
      let new_entry =
        create_entry (Entry.name entry) (create_date d) (status entry)
          (Entry.notes entry)
      in
      process_update_acc
        (Entrylist.change entry st new_entry)
        new_entry t
  | ("status", s) :: t ->
      let new_entry =
        create_entry (Entry.name entry) (date entry) s
          (Entry.notes entry)
      in
      process_update_acc
        (Entrylist.change entry st new_entry)
        new_entry t
  | ("notes", "cancel") :: t -> process_update_acc st entry t
  | ("notes", nts) :: t ->
      let new_entry =
        create_entry (Entry.name entry) (date entry) (status entry)
          (Yes nts)
      in
      process_update_acc
        (Entrylist.change entry st new_entry)
        new_entry t
  | [] -> st
  | _ -> raise (Invalid_argument "invalid")

let rec update_entry acc valid error st entry_name =
  if not valid then
    ANSITerminal.print_string [ ANSITerminal.red ] (error ^ "\n");
  print_endline
    "What would you like to change? Please enter: name, date, status, \
     notes, cancel to cancel all changes, or done to finish changes.";
  let r = clean (read_line ()) in
  match r with
  | [ "name" ] ->
      print_endline "Please enter the new name of the selected entry:";
      let n = process (read_line ()) in
      if List.exists (fun x -> String.compare n x = 0) (entry_names st)
      then update_entry acc false dup_message st n
      else update_entry (acc @ [ ("name", n) ]) true "" st entry_name
  | [ "date" ] -> (
      print_endline "Please enter the new date of the selected entry:";
      try
        let s = process (read_line ()) in
        let d = date_string (create_date s) in
        update_entry (acc @ [ ("date", d) ]) true "" st entry_name
      with InvalidDate ->
        update_entry acc false date_error st entry_name)
  | [ "status" ] ->
      print_endline
        "Please enter the new status of the selected entry from: new, \
         applied, interviewed, accepted, rejected";
      let s = process (read_line ()) in
      if valid_s s then
        update_entry (acc @ [ ("status", s) ]) true "" st entry_name
      else update_entry acc false status_error st entry_name
  | [ "notes" ] ->
      print_endline
        "Please enter the new notes for the selected entry or type \
         cancel";
      let s = process (read_line ()) in
      update_entry (acc @ [ ("notes", s) ]) true "" st entry_name
  | [ "done" ] -> acc
  | [ "cancel" ] -> []
  | _ ->
      update_entry acc false
        "The field name was not recognized. Please enter a valid field \
         name."
        st entry_name

let rec sort_by_entry (st : Entrylist.t) valid error : string =
  if not valid then ANSITerminal.print_string [ ANSITerminal.red ] error;
  print_endline
    "What would you like to sort by? Please enter either 'name', \
     'status', 'date', or 'none'.";
  match clean (read_line ()) with
  | [ "name" ] ->
      string_of_list (Entrylist.print_t (Entrylist.sort_by_name st))
  | [ "status" ] ->
      string_of_list (Entrylist.print_t (sort_by_status st))
  | [ "date" ] -> string_of_list (Entrylist.print_t (sort_by_date st))
  | [ "none" ] -> string_of_list (Entrylist.print_t st)
  | _ -> sort_by_entry st false "Could not recognize your command.\n"

let rec make_tracker
    user
    password
    msg
    cal
    network
    (internships : Entrylist.t) =
  print_endline msg;
  Printf.printf "> ";
  match parse (read_line ()) with
  | Add -> (
      try
        Entrylist.add
          (build_entry (Name "name") [] true "" ""
          |> process_lst_to_entry)
          internships
        |> make_tracker user password command_message cal network
      with Duplicate ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          (dup_message ^ "\n");
        make_tracker user password command_message cal network
          internships)
  | Delete s -> (
      try
        let str = cmd_string s in
        Entrylist.delete str internships
        |> make_tracker user password
             (deleted_msg str ^ command_message)
             cal network
      with NotMem ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          (notfound_message ^ "\n");
        make_tracker user password command_message cal network
          internships)
  | View ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        (sort_by_entry internships true "" ^ "\n");
      make_tracker user password command_message cal network internships
  | Update s -> (
      try
        let entry_name = cmd_string s in
        let entry = find_entry entry_name internships in
        update_entry [] true "" internships entry_name
        |> process_update_acc internships entry
        |> make_tracker user password command_message cal network
      with NotMem ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          (notfound_message ^ "\n");
        make_tracker user password command_message cal network
          internships)
  | Quit ->
      write_file password internships network user;
      exit 0
  | Notes n -> (
      try
        let entry_name = cmd_string n in
        let entry = find_entry entry_name internships in
        print_endline ("Your notes for entry " ^ entry_name ^ " are: ");
        print_endline (notes_string entry);
        make_tracker user password command_message cal network
          internships
      with NotMem ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          (notfound_message ^ "\n");
        make_tracker user password command_message cal network
          internships)
  | Network ->
      make_tracker user password command_message cal
        (make_network network_msg network)
        internships
  | Calendar ->
      make_tracker user password command_message
        (make_calendar internships cal_start_msg)
        network internships
  | exception Malformed ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "WARNING: Could not recognize your command\n";
      make_tracker user password command_message cal network internships

let is_alpha_numeric = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false

let explode s = List.init (String.length s) (String.get s)
let valid_login s = List.for_all is_alpha_numeric (explode s)

let does_exist u =
  try Sys.file_exists ("data/" ^ u ^ ".json")
  with Sys_error s -> false

let rec create_password msg =
  print_endline msg;
  let p = read_line () in
  if valid_login p then p
  else
    create_password ("The password you have entered is invalid.\n" ^ msg)

let rec new_user m =
  print_endline m;
  let u = read_line () in
  if valid_login u && not (does_exist u) then (
    let p = create_password new_password_msg in
    write_file p [] [] u;
    (u, p))
  else if not (valid_login u) then new_user invalid_username_msg
  else new_user username_exists_msg

let rec get_password user =
  print_endline "Please enter your password:";
  let pass = from_json_password user in
  let entered = read_line () in
  if entered = pass then (user, entered)
  else (
    ANSITerminal.print_string [ ANSITerminal.red ]
      "Incorrect password.\n";
    get_password user)

let rec returning_user msg =
  print_endline msg;
  let user = read_line () in
  if does_exist user then get_password user
  else
    returning_user
      ("The username you have entered does not exist.\n" ^ msg)

let rec get_data () =
  print_endline (new_or_return_msg ^ "To quit, enter 'quit'.");
  match clean (read_line ()) with
  | [ "n" ] -> new_user username_msg
  | [ "r" ] -> returning_user "Please enter your username:"
  | [ "quit" ] -> exit 0
  | _ ->
      print_endline "The input was invalid.";
      get_data ()

let main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Welcome to the Internship Application Tracker Interface!\n";
  let user, password = get_data () in
  make_tracker user password command_message ""
    (from_json_network user)
    (from_json_internships user)

let () = main ()
