open Contact

type t = contact list
(** The type of values representing a collection of contacts without
    duplicates. *)

exception NotFound
(** Raised when an entry is not in the list. *)

val contact_names : contact list -> string list
(** [contact_names c] is the list of contacts names in [c] *)

val add : contact -> t -> t
(** [add e t] is the entry list [t] with [e] added, if [e] is not in
    [t]. Raises: Duplicate if [e] is already in [t]. *)

val delete : string -> t -> t
(** [delete e t] is the entry list [t] with [e] removed. Raises:
    NotFound if entry is not found in [t]. *)

val change : contact -> t -> contact -> contact list
(** [change e t s] is the entry list [t] with entry [e] changed to [s]. *)

val find_contact : string -> t -> contact
(** [find_entry s t] is the entry with name [s] in entry list [t].
    Raises: NotFound if [s] is not a name with a valid entry in [t]*)

val print_t : t -> string list
(** [print_t t] is the string list of [t]*)

val sort_by_name : t -> t
(** [sort_by_name t] is [t] sorted by name in alphabetical order *)
