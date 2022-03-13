type command_phrase = string list

type command =
  | Add
  | Delete of command_phrase
  | View
  | Quit
  | Update of command_phrase

exception Malformed

val parse : string -> command
