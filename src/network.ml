open Contact

type t = contact list

exception NotFound

let contact_names = List.map name
let add contact lst = contact :: lst

let rec delete c (lst : t) =
  match lst with
  | [] -> raise NotFound
  | h :: t -> if c = name h then t else h :: delete c t

let change contact lst new_contact =
  let new_lst = delete (name contact) lst in
  add new_contact new_lst

let rec find_contact s (lst : t) =
  match lst with
  | [] -> raise NotFound
  | h :: t -> if name h = s then h else find_contact s t

let print_t = List.map print_contact
let sort_by_name = List.sort compare_names