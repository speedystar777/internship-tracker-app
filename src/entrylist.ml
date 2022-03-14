open Entry

type t = entry list

exception NotFound
exception Duplicate

let entry_names = List.map name
let mem entry = List.exists (is_equal_entry entry)

let add entry lst =
  if mem entry lst then raise Duplicate else entry :: lst

let rec delete e (lst : t) =
  match lst with
  | [] -> raise NotFound
  | h :: t -> if e = name h then t else h :: delete e t

let change_status entry lst new_entry =
  let new_lst = delete (name entry) lst in
  add new_entry new_lst

let change_name entry lst new_entry =
  let new_lst = delete (name entry) lst in
  add new_entry new_lst

let change_date entry lst new_entry =
  let new_lst = delete (name entry) lst in
  add new_entry new_lst

let rec find_entry s (lst : t) =
  match lst with
  | [] -> raise NotFound
  | h :: t -> if name h = s then h else find_entry s t

let print_t = List.map print_entry
let sort_by_name = List.sort compare_names
let sort_by_status = List.sort compare_status
