open Entry

type t = entry list
(** The type of values representing a collection of entries without
    duplicates. *)

exception NotFound
(** Raised when an entry is not in the list. *)

exception Duplicate
(** Raised when an entry is already in the list. *)

val mem : entry -> t -> bool
(** [mem e t] if [e] is a member of [t]*)

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

val find_entry : string -> t -> entry
(** [find_entry s t] is the entry with name [s] in entry list [t].
    Raises: NotFound if [s] is not a name with a valid entry in [t]*)

val print_t : t -> string list
(** [print_t t] is the string list of [t]*)

val sort_by_name : t -> t
(** [sort_by_name t] is [t] sorted by name in alphabetical order *)

val sort_by_status : t -> t
(** [sort_by_status t] is [t] sorted by status in alphabetical order *)
