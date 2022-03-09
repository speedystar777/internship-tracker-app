type entry
(** The abstract type of values representing an entry. *)

type t = entry list
(** The type of values representing a collection of entries without
    duplicates. *)

val compare_entries : entry -> entry -> int
(** [compare_entries e1 e2] is positive if [e1] is greater than [e2],
    negative if [e1] is less than [e2], and zero otherwise. *)

val create_entry : string -> string -> string -> entry
(** [create_entry s1 s2 s3] is an entry type. *)

exception NotFound
(** Raised when an entry is not in the list. *)

exception Duplicate
(** Raised when an entry is already in the list. *)

val is_equal_entry : entry -> entry -> bool
(** [is_equal_entry e1 e2] is true if entry [e1] is equal to [e2]. *)

val add : entry -> t -> t
(** [add e t] is the entry list [t] with [e] added, if [e] is not in
    [t]. Raises: Duplicate if [e] is already in [t]. *)

val delete : entry -> t -> t
(** [delete e t] is the entry list [t] with [e] removed. Raises:
    NotFound if entry is not found in [t]. *)
