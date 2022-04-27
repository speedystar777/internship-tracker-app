open Tracker
open Entry
open Date
open Contact
open Network
open Yojson.Basic

let internship_to_json (internships : entry list) : Yojson.Basic.t =
  let rec build_entries (lst : entry list) =
    match lst with
    | [] -> []
    | h :: t ->
        `Assoc
          [
            ("name", `String (Entry.name h));
            ("date", `String (date_string (date h)));
            ("status", `String (status h));
            ("notes", `String (notes_string h));
          ]
        :: build_entries t
  in
  `List (build_entries internships)

let network_to_json (network : contact list) : Yojson.Basic.t =
  let rec build_network lst =
    match lst with
    | [] -> []
    | h :: t ->
        `Assoc
          [
            ("name", `String (Contact.name h));
            ("email", `String (email_str h));
            ("phone", `String (phone_number_str h));
            ("notes", `String (notes_str h));
          ]
        :: build_network t
  in
  `List (build_network network)

let to_json password internships network : Yojson.Basic.t =
  let internship_array = internship_to_json internships in
  let network_array = network_to_json network in
  `Assoc
    [
      ("internships", internship_array);
      ("network", network_array);
      ("password", `String password);
    ]

let write_file password internships network name =
  to_file
    ("data/" ^ name ^ ".json")
    (to_json password internships network)
