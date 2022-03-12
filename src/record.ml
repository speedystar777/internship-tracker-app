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

let create_entry name date status_string = { name; date; status = match status_string with
| "new" -> New
| "applied" -> Applied 
| "interviewed" -> Interviewed
| "accepted" -> Accepted
| "rejected" -> Rejected 
| _ -> raise InvalidArg}

let compare_entries e1 e2 = String.compare e1.name e2.name

let is_equal_entry e1 e2 = compare_entries e1 e2 = 0

let mem entry = List.exists (is_equal_entry entry)

let add entry lst =
  if mem entry lst then raise Duplicate else entry :: lst

let rec delete entry lst =
  match lst with
  | [] -> raise NotFound
  | h :: t -> if is_equal_entry entry h then t else h :: delete entry t

let change_status entry lst s = 
  let new_entry = {entry with status = s} in 
  let new_lst = delete entry lst in
add new_entry new_lst

let print_entry e = "name: " ^ e.name ^ " | date: " ^ e.date ^ " | status: " ^ (match e.status with
| New -> "new" | Applied -> "applied" | Interviewed -> "interviewed" | Accepted -> "accepted" | Rejected -> "rejected")

let print_t = List.map print_entry