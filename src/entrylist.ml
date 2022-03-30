open Entry
open Date

type t = entry list

exception NotMem
exception Duplicate

let entry_names = List.map name
let entry_dates = List.map date
let mem entry = List.exists (is_equal_entry entry)

let add entry lst =
  if mem entry lst then raise Duplicate else entry :: lst

let rec delete e (lst : t) =
  match lst with
  | [] -> raise NotMem
  | h :: t -> if e = name h then t else h :: delete e t

let change entry lst new_entry =
  let new_lst = delete (name entry) lst in
  add new_entry new_lst

let rec find_entry s (lst : t) =
  match lst with
  | [] -> raise NotMem
  | h :: t -> if name h = s then h else find_entry s t

let print_t = List.map print_entry
let sort_by_name = List.sort compare_names
let sort_by_status = List.sort compare_status
let sort_by_date = List.sort compare_entry_dates
