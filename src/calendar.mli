open Date

val start_weekday : string -> string -> int
val first_weekday : string -> int
val start_day : bool -> months -> int
val calendar_header : string -> string -> string
val weekday_header : string
val filter_days : Date.t list -> string -> string -> Date.t list
