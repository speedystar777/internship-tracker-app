type state =
  | New
  | Applied
  | Interviewed
  | Accepted
  | Rejected

type entry = {
  name : string;
  date : string;
  status : state;
}

type id =
  | Name of string
  | Date of string
  | Status of string

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

let create_entry name date status_string =
  { name; date; status = string_to_state status_string }

let name entry = entry.name
let status entry = entry.status |> state_to_string

let valid_s s =
  s = "new" || s = "applied" || s = "interviewed" || s = "accepted"
  || s = "rejected"

let date entry = entry.date
let valid_d d = true
let compare_names e1 e2 = String.compare e1.name e2.name

let compare_status e1 e2 =
  String.compare (state_to_string e1.status) (state_to_string e2.status)

let is_equal_entry e1 e2 = compare_names e1 e2 = 0

let print_entry e =
  "name: " ^ e.name ^ " | date: " ^ e.date ^ " | status: "
  ^ state_to_string e.status
