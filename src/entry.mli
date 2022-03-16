type entry
(** The abstract type of values representing an entry. *)

type state =
  | New
  | Applied
  | Interviewed
  | Accepted
  | Rejected
      (** The type of values representing the state of an entry *)

type has_note = 
 | Yes of string
 | No 

type id =
  | Name of string
  | Date of string
  | Status of string
  | Notes of string

val compare_names : entry -> entry -> int
(** [compare_names e1 e2] is positive if [name e1] is greater than
    [name e2], negative if [name e1] is less than [name e2], and zero
    otherwise. *)

val compare_status : entry -> entry -> int
(** [compare_status e1 e2] is positive if [status e1] is greater than
    [status e2], negative if [statuse1] is less than [status e2], and
    zero otherwise. *)

val create_entry : string -> string -> string -> has_note -> entry
(** [create_entry s1 s2 s3] is an entry type. *)

exception InvalidArg
(** Raised when an invalid argument is given. *)

val name : entry -> string
(** [name e] is the name of entry [e] *)

val date : entry -> string
(** [date e] is the date in entry [e]*)

val notes : entry -> has_note
(** [notes e] is the note in entry [e]*)

val notes_string : entry -> string
(** [notes_string e] is the content of the note in entry [e] if it exists, or the string ["No notes"] if there is no note.*)

val valid_d : string -> bool
(** [valid_d d] is true if [d] is in a valid format *)

val status : entry -> string
(** [status e] is the status of entry [e] *)

val process : string -> string
(** [process s] is the lowercase string [s] separated by only one space between each word, and with no trailing spaces*)
val valid_s : string -> bool
(** [valid_s s] is true if [s] is a valid status *)

val valid_n : string -> bool
(** [valid_n n] is true if [n] is a non-emtpy string*)

val is_equal_entry : entry -> entry -> bool
(** [is_equal_entry e1 e2] is true if entry [e1] is equal to [e2]. *)

val print_entry : entry -> string
(** [print_entry e] is the string of entry [e]. *)

val string_to_state : string -> state
(** [string_to_state s] is the status type of string [s]. Raises:
    InvalidArg if [s] is not a valid status type*)

val state_to_string : state -> string
(** [state_to_string s] is the string of status type [s]*)

val grab_id_phrase : id -> string
(** [grab_id_phrase id] is the string value of id [id]*)

