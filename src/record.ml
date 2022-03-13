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

type t = entry list

exception NotFound
exception Duplicate
exception InvalidArg

let empty = []

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

let create_entry name date status_string =
  { name; date; status = string_to_state status_string }

let name entry = entry.name
let status entry = entry.status |> state_to_string
let date entry = entry.date
let compare_entries e1 e2 = String.compare e1.name e2.name
let is_equal_entry e1 e2 = compare_entries e1 e2 = 0
let entry_names = List.map name
let mem entry = List.exists (is_equal_entry entry)

let add entry lst =
  if mem entry lst then raise Duplicate else entry :: lst

let rec delete e lst =
  match lst with
  | [] -> raise NotFound
  | h :: t -> if e = h.name then t else h :: delete e t

let change_status entry lst new_entry =
  let new_lst = delete entry.name lst in
  add new_entry new_lst

let change_name entry lst new_entry =
  let new_lst = delete entry.name lst in
  add new_entry new_lst

let change_date entry lst new_entry =
  let new_lst = delete entry.name lst in
  add new_entry new_lst

let rec find_entry s = function
  | [] -> raise NotFound
  | h :: t -> if h.name = s then h else find_entry s t

let print_entry e =
  "name: " ^ e.name ^ " | date: " ^ e.date ^ " | status: "
  ^ state_to_string e.status

let print_t = List.map print_entry