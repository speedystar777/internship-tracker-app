open Date

val start_weekday : string -> string -> int
(** [start_weekday m y] returns a numeric representation of the weekday
    for which month [m] in year [y] starts on. *)

val first_weekday : string -> int
(** [first_weekday y] returns a numeric representation of the weekday
    for which January of year [y] starts on. *)

val start_day : bool -> months -> int
(** [start_day b m] returns the numeric day of the year that month [m]
    starts on, in the range 1..336/337 conditioned on if it is a leap
    year. *)

val calendar_header : string -> string -> string
(** [calendar_header m y] is a string for month [m] in year [y] *)

val weekday_header : string
(** [weekday_header] is a string of the weekdays *)

val filter_days : Date.t list -> string -> string -> Date.t list
(** [filter_days lst m y] is the list of of days in month [m] in year
    [y] for which an internship application is due *)
