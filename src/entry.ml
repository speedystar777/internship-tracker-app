open Date

type state =
  | New
  | Applied
  | Interviewed
  | Accepted
  | Rejected

type has_note =
  | Yes of string
  | No

type entry = {
  name : string;
  date : Date.t;
  status : state;
  notes : has_note;
}

type id =
  | Name of string
  | Date of string
  | Status of string
  | Notes of string

exception InvalidArg

let string_to_state status_string =
  match status_string with
  | "new" -> New
  | "applied" -> Applied
  | "interviewed" -> Interviewed
  | "accepted" -> Accepted
  | "rejected" -> Rejected
  | _ -> raise InvalidArg

let state_to_string state =
  match state with
  | New -> "new"
  | Applied -> "applied"
  | Interviewed -> "interviewed"
  | Accepted -> "accepted"
  | Rejected -> "rejected"

let grab_id_phrase = function
  | Name n -> n
  | Date d -> d
  | Status s -> s
  | Notes n -> n

let process s =
  s
  |> String.split_on_char ' '
  |> List.filter (fun x -> x <> "")
  |> List.map String.lowercase_ascii
  |> String.concat " "

let create_entry name date status_string note =
  { name; date; status = string_to_state status_string; notes = note }

let name entry = entry.name
let status entry = entry.status |> state_to_string

let valid_s s =
  s = "new" || s = "applied" || s = "interviewed" || s = "accepted"
  || s = "rejected"

let valid_n n = process n <> ""
let date entry = entry.date
let notes entry = entry.notes

let notes_string entry =
  match entry.notes with
  | Yes s -> s
  | No -> "No notes"

let compare_names e1 e2 = String.compare e1.name e2.name

let compare_status e1 e2 =
  String.compare (state_to_string e1.status) (state_to_string e2.status)

let is_equal_entry e1 e2 = compare_names e1 e2 = 0

let print_entry e =
  let note =
    match e.notes with
    | Yes s -> "yes"
    | No -> "no"
  in
  "name: " ^ e.name ^ " | date: " ^ date_string e.date ^ " | status: "
  ^ state_to_string e.status
  ^ " | has notes: " ^ note
