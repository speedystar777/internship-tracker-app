type entry = {
  name : string;
  date : string;
  status : string;
}

type t = entry list

exception NotFound
exception Duplicate

let create_entry name date status = { name; date; status }
let compare_entries e1 e2 = String.compare e1.name e2.name
let is_equal_entry e1 e2 = compare_entries e1 e2 = 0
let mem entry = List.exists (is_equal_entry entry)

let add entry lst =
  if mem entry lst then raise Duplicate else entry :: lst

let rec delete entry lst =
  match lst with
  | [] -> raise NotFound
  | h :: t -> if is_equal_entry entry h then t else h :: delete entry t
