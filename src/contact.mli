type contact
(** The type of value representing a contact. *)

(** The type indicating whether a note exists. *)
type has_note =
  | Yes of string
  | No

(** The type representing different forms of [id] *)
type id =
  | Name of string
  | Email of string
  | Phone of string
  | Notes of string

val name : contact -> string
(** [name e] is the name in entry [e]*)

val phone_number : contact -> string option
(** [phone_number e] is the phone number in entry [e]*)

val phone_number_str : contact -> string
(** [phone_number_string e] is the content of the phone number in entry
    [e] if it exists, or the string ["N/A"] if there is no phone number.*)

val email : contact -> string option
(** [email e] is the email in entry [e]*)

val email_str : contact -> string
(** [email_string c] is the string of notes in contact [c] and is
    ["N/A"] if notes is None *)

val notes : contact -> has_note
(** [notes e] is the note in entry [e]*)

val notes_str : contact -> string
(** [notes_string e] is the content of the note in entry [e] if it
    exists, or the string ["N/A"] if there is no note.*)

val valid_n : string -> bool
(** [valid_n n] is true if [n] is a non-emtpy string*)

val print_contact : contact -> string
(** [print_contact c] is the string of contact [c]*)

val compare_names : contact -> contact -> int
(** [compare_names e1 e2] is positive if [name e1] is greater than
    [name e2], negative if [name e1] is less than [name e2], and zero
    otherwise. *)

val create_contact :
  string -> string option -> string option -> has_note -> contact
(** [create_contact n e p n] is a contact type *)

val grab_id_phrase : id -> string
(** [grab_id_phrase id] is the string value of id [id]*)
