type entry
(** The abstract type of values representing an entry. *)

type state =
  | New
  | Applied
  | Interviewed
  | Accepted
  | Rejected
      (** The type of values representing the state of an entry *)

type t = entry list
(** The type of values representing a collection of entries without
    duplicates. *)

val empty : t
(** [empty] is the empty entry list *)

val compare_entries : entry -> entry -> int
(** [compare_entries e1 e2] is positive if [e1] is greater than [e2],
    negative if [e1] is less than [e2], and zero otherwise. *)

val create_entry : string -> string -> string -> entry
(** [create_entry s1 s2 s3] is an entry type. *)

exception NotFound
(** Raised when an entry is not in the list. *)

exception Duplicate
(** Raised when an entry is already in the list. *)

exception InvalidArg
(** Raised when an invalid argument is given. *)

val name : entry -> string
(** [name e] is the name of entry [e] *)

val date : entry -> string
(** [date e] is the date in entry [e]*)

val status : entry -> string
(** [status e] is the status of entry [e] *)

val is_equal_entry : entry -> entry -> bool
(** [is_equal_entry e1 e2] is true if entry [e1] is equal to [e2]. *)

val entry_names : t -> string list
(** [entry_names] returns the list of names of each entry in [t] *)

val add : entry -> t -> t
(** [add e t] is the entry list [t] with [e] added, if [e] is not in
    [t]. Raises: Duplicate if [e] is already in [t]. *)

val delete : string -> t -> t
(** [delete e t] is the entry list [t] with [e] removed. Raises:
    NotFound if entry is not found in [t]. *)

val change_status : entry -> t -> entry -> entry list
(** [change_status e t s] is the entry list [t] with the status of [e]
    changed to [s]. Raises: NotFound if entry is not found in [t]. *)

val change_name : entry -> t -> entry -> entry list
(** [change_name e t n] is the entry list [t] with the name of [e]
    changed to [n]. Raises: NotFound if entry is not found in [t]. *)

val change_date : entry -> t -> entry -> entry list
(** [change_date e t d] is the entry list [t] with the date of [e]
    changed to [d]. Raises: NotFound if entry is not found in [t]. *)

val print_entry : entry -> string
(** [print_entry e] is the string of entry [e]. *)

val string_to_state : string -> state
(** [string_to_state s] is the status type of string [s]. Raises:
    InvalidArg if [s] is not a valid status type*)

val find_entry : string -> t -> entry
(** [string_to_state s t] is the entry [e] whose name matches [s].
    Raises: NotFound if [e] is not a valid entry in [t]*)

val print_t : t -> string list
(** [print_t t] is the string list of [t]*)
