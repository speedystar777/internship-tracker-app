type command_phrase = string list
(** The type [command_phrase] represents the command phrase that can be
    part of a player command. Each element of the list represents a word
    of the command phrase, where a {i word} is defined as a consecutive
    sequence of non-space characters. Thus, no element of the list
    should contain any leading, internal, or trailing spaces. The list
    is in the same order as the words in the original user command.

    An [command_phrase] is not permitted to be the empty list. *)

(** The type [command] represents a user command that is decomposed into
    a verb and possibly an command phrase. *)
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
(** Raised when a malformed command is encountered. *)

val parse : string -> command
(** [parse str] parses a player's input into a [command], as follows.
    The first word (i.e., consecutive sequence of non-space characters)
    of [str] becomes the verb. The rest of the words, if any, become the
    object phrase. Examples:

    - [parse "    add   "] is [Add]
    - [parse "delete name "] is [Delete \["name"\]].
    - [parse "    view   "] is [View]
    - [parse "    quit   "] is [Quit]
    - [parse "    update   name    "] is [Update \["name"\]]
    - [parse "    notes for name    "] is [Notes \["ladder"\]]
    - [parse "    network   "] is [Network]
    - [parse "calendar"] is [Calendar].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines,
    etc.).

    Raises: [Malformed] if the command is malformed.*)