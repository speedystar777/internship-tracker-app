type months =
  | Jan of int
  | Feb of int
  | Mar of int
  | Apr of int
  | May of int
  | Jun of int
  | Jul of int
  | Aug of int
  | Sep of int
  | Oct of int
  | Nov of int
  | Dec of int

type t
(** The type of values representing a date. *)

exception InvalidDate
(** Raised when date is not valid. *)

val valid_day : string -> string -> string -> bool
(** [valid_day m d y] is true if [d] is a valid string representation of
    a day number in a given month [m] and year [y] otherwise it is
    false. *)

val valid_month : string -> bool
(** [valid_day m] is true if [m] is a valid string representation of a
    month such that a month must be '01', '02', ..., '12'. *)

val valid_year : string -> bool
(** [valid_day y] is true if [y] is a valid string representation of a
    year such that the year is greater than or equal to the current year
    and less than 2100. *)

val create_date : string -> t
(** [create_date s] is a date type of the input string [s], which is in
    the format 'mm/dd/yyyy'. *)

val get_days : months -> int
(** [get_days m] is an integer corresponding to the number of days in
    the months type [m]. *)

val is_leap : string -> bool
(** [is_leap y] is true if the year [y] is a leap year. *)

val days_in_month : bool -> string -> months
(** [days_in_month l m] is the month type of a given month [m] given
    [l], which denotes whether or not it is a leap year. *)

val date_string : t -> string
(** [date_string d] is the string of date type [d]. *)
    
val month : t -> string
(** [month t] is the month of date [t] *)
val year : t -> string
(** [year t] is the year of date [t] *)
val day : t -> string
(** [day t] is the day of date [t] *)

val compare_dates : t -> t -> int
(** [compare_dates d1 d2] is positive if date [d1] is greater than date
    [d2], negative if date [d1] is less than date [d2], and zero
    otherwise. *)
