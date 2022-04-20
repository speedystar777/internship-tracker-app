type has_note =
  | Yes of string
  | No

type id =
  | Name of string
  | Email of string
  | Phone of string
  | Notes of string

type contact = {
  name : string;
  email : string option;
  phone : string option;
  notes : has_note;
}

let grab_id_phrase = function
  | Name n -> n
  | Email e -> e
  | Phone p -> p
  | Notes n -> n

let name contact = contact.name
let email contact = contact.email

let email_str contact =
  match contact.email with
  | Some s -> s
  | None -> "N/A"

let phone_number contact = contact.phone

let phone_number_str contact =
  match contact.phone with
  | Some s -> s
  | None -> "N/A"

let notes contact = contact.notes

let notes_str contact =
  match contact.notes with
  | Yes s -> s
  | No -> "No notes"

let create_contact name email phone note =
  { name; email; phone; notes = note }

let process s =
  s
  |> String.split_on_char ' '
  |> List.filter (fun x -> x <> "")
  |> List.map String.lowercase_ascii
  |> String.concat " "

let valid_n n = process n <> ""
let compare_names c1 c2 = String.compare c1.name c2.name

let print_contact c =
  let note =
    match c.notes with
    | Yes s -> "yes"
    | No -> "no"
  in
  "name: " ^ c.name ^ " | email: " ^ email_str c ^ " | phone : "
  ^ phone_number_str c ^ " | has notes: " ^ note
