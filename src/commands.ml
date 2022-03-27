type command_phrase = string list

type command =
  | Add
  | Delete of command_phrase
  | View
  | Quit
  | Update of command_phrase
  | Notes of command_phrase
  | Network
  | Calendar

exception Malformed

let parse str =
  let get_command = function
    | [ "add" ] -> Add
    | "delete" :: t -> Delete t
    | [ "view" ] -> View
    | [ "quit" ] -> Quit
    | "update" :: t -> Update t
    | "notes" :: "for" :: t -> Notes t
    | [ "network" ] -> Network
    | [ "calendar" ] -> Calendar
    | _ -> raise Malformed
  in
  get_command
    (List.filter (fun x -> x <> "") (String.split_on_char ' ' str))
