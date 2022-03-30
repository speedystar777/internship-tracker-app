open Tracker
open ANSITerminal
open Entry
open Commands
open Contact
open Network
open Message_strings

let rec build_contact (id : Contact.id) acc valid error : string list =
  if not valid then
    ANSITerminal.print_string [ ANSITerminal.red ] (error ^ "\n");
  let s = Contact.grab_id_phrase id in
  print_endline ("Please enter the " ^ s ^ " of the contact");
  match id with
  | Name n ->
      if valid_n n then
        build_contact (Email "email")
          (process (read_line ()) :: acc)
          true ""
      else build_contact (Name "name") acc true name_error
  | Email e ->
      build_contact (Phone "phone")
        (process (read_line ()) :: acc)
        true ""
  | Phone p -> (
      let s = process (read_line ()) in
      print_endline
        "Please enter if you would like to include notes for your \
         contact : type 'yes' or 'no'";
      let r = process (read_line ()) in
      match r with
      | "yes" -> build_contact (Notes "notes") (s :: acc) true ""
      | _ -> "no" :: s :: acc)
  | Notes n -> process (read_line ()) :: acc

let process_lst_to_contact = function
  | [ nt; p; e; n ] ->
      Contact.create_contact n (Some e) (Some p)
        (match nt with
        | "no" -> No
        | s -> Yes s)
  | _ -> raise (Invalid_argument "invalid")

let rec process_update_acc_network st (contact : contact) acc =
  match acc with
  | ("name", n) :: t ->
      let new_contact =
        create_contact n (email contact) (phone_number contact)
          (Contact.notes contact)
      in
      process_update_acc_network
        (Network.change contact st new_contact)
        new_contact t
  | ("email", e) :: t ->
      let em = if e = "" then None else Some e in
      let new_contact =
        create_contact (Contact.name contact) em (phone_number contact)
          (Contact.notes contact)
      in
      process_update_acc_network
        (Network.change contact st new_contact)
        new_contact t
  | ("phone", p) :: t ->
      let ph = if p = "" then None else Some p in
      let new_contact =
        create_contact (Contact.name contact) (Contact.email contact) ph
          (Contact.notes contact)
      in
      process_update_acc_network
        (change contact st new_contact)
        new_contact t
  | ("notes", "cancel") :: t -> process_update_acc_network st contact t
  | ("notes", nts) :: t ->
      let new_contact =
        create_contact (Contact.name contact) (Contact.email contact)
          (Contact.phone_number contact)
          (Yes nts)
      in
      process_update_acc_network
        (change contact st new_contact)
        new_contact t
  | [] -> st
  | _ -> raise (Invalid_argument "invalid")

let rec update_contact acc valid error st contact_name =
  if not valid then
    ANSITerminal.print_string [ ANSITerminal.red ] (error ^ "\n");
  print_endline
    "What would you like to change? Please enter: name, email, phone, \
     notes, cancel to cancel all changes, or done to finish changes.";
  let s = read_line () in
  match s with
  | "name" ->
      print_endline "Please enter the new name of the selected contact:";
      let n = process (read_line ()) in
      update_contact (acc @ [ ("name", n) ]) true "" st contact_name
  | "email" ->
      print_endline
        "Please enter the new email of the selected contact:";
      let s = process (read_line ()) in
      update_contact (acc @ [ ("email", s) ]) true "" st contact_name
  | "phone" ->
      print_endline "Please enter the new phone number.";
      let s = process (read_line ()) in
      update_contact (acc @ [ ("phone", s) ]) true "" st contact_name
  | "notes" ->
      print_endline
        "Please enter the new notes for the selected contact or type \
         cancel";
      let s = process (read_line ()) in
      update_contact (acc @ [ ("notes", s) ]) true "" st contact_name
  | "done" -> acc
  | "cancel" -> []
  | _ ->
      update_contact acc false
        "The field name was not recognized. Please enter a valid field \
         name."
        st contact_name

let rec sort_by_network (st : Network.t) valid error : string =
  if not valid then ANSITerminal.print_string [ ANSITerminal.red ] error;
  print_endline
    "What would you like to sort by? Please enter either 'name' or \
     'none'.";
  match read_line () with
  | "name" -> string_of_list (Network.print_t (Network.sort_by_name st))
  | "none" -> string_of_list (Network.print_t st)
  | _ -> sort_by_network st false "Could not recognize your command."

let rec make_network msg (network : Network.t) =
  print_endline msg;
  Printf.printf "> ";
  match parse (read_line ()) with
  | Add ->
      Network.add
        (build_contact (Contact.Name "name") [] true ""
        |> process_lst_to_contact)
        network
      |> make_network network_msg
  | Delete s -> (
      try
        let str = cmd_string s in
        Network.delete str network
        |> make_network (deleted_msg str ^ network_msg)
      with NotFound ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          (notfound_message ^ "\n");
        make_network network_msg network)
  | View ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        (sort_by_network network true "" ^ "\n");
      make_network network_msg network
  | Update s -> (
      try
        let contact_name = cmd_string s in
        let contact = find_contact contact_name network in
        update_contact [] true "" network contact_name
        |> process_update_acc_network network contact
        |> make_network network_msg
      with NotFound ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          (notfound_message ^ "\n");
        make_network network_msg network)
  | Quit -> network
  | Notes n -> (
      try
        let contact_name = cmd_string n in
        let contact = find_contact contact_name network in
        print_endline ("Your notes for entry " ^ contact_name ^ " are: ");
        print_endline (notes_str contact);
        make_network network_msg network
      with NotFound ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          (notfound_message ^ "\n");
        make_network network_msg network)
  | Network -> make_network network_msg network
  | Calendar -> make_network network_msg network
  | exception Malformed ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "WARNING: Could not recognize your command\n";
      make_network network_msg network
