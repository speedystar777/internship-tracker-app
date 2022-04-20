open Tracker
open Entry
open Date
open Contact
open Network
open Yojson.Basic

let read_file name = from_file ("data/" ^ name ^ ".json")

let get_entry entry =
  let name =
    let p = to_string (List.assoc "name" entry) in
    String.sub p 1 (String.length p - 2)
  in
  let date =
    let s = to_string (List.assoc "date" entry) in
    let p = String.sub s 1 (String.length s - 2) in
    create_date p
  in
  let status =
    let p = to_string (List.assoc "status" entry) in
    String.sub p 1 (String.length p - 2)
  in
  let notes : Entry.has_note =
    let n =
      let p = to_string (List.assoc "notes" entry) in
      String.sub p 1 (String.length p - 2)
    in
    match n with
    | "No notes" -> No
    | s -> Yes s
  in
  create_entry name date status notes

let get_internships_list =
  List.map (fun x -> x |> Util.to_assoc |> get_entry)

let from_json_internships name =
  Util.to_assoc (read_file name)
  |> List.assoc "internships"
  |> Util.to_list |> get_internships_list

let get_contact contact =
  let name = to_string (List.assoc "name" contact) in
  let email =
    let e = to_string (List.assoc "email" contact) in
    match e with
    | "N/A" -> None
    | em -> Some em
  in
  let phone =
    let p = to_string (List.assoc "phone" contact) in
    match p with
    | "N/A" -> None
    | pn -> Some pn
  in
  let notes : Contact.has_note =
    let n = to_string (List.assoc "notes" contact) in
    match n with
    | "No notes" -> No
    | s -> Yes s
  in
  create_contact name email phone notes

let get_network_list =
  List.map (fun x -> x |> Util.to_assoc |> get_contact)

let from_json_network name =
  Util.to_assoc (read_file name)
  |> List.assoc "network" |> Util.to_list |> get_network_list
