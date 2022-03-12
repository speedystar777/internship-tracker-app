type command_phrase = string list

type command =
  | Add
  | Delete of command_phrase
  | View
  | Quit

exception Malformed

val parse : string -> command
