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

exception InvalidDate

val valid_day : string -> string -> string -> bool
val valid_month : string -> bool
val valid_year : string -> bool
val create_date : string -> t
val get_days : months -> int
val is_leap : string -> bool
val days_in_month : bool -> string -> months
val date_string : t -> string
val month : t -> string
val year : t -> string
val day : t -> string
val compare_dates : t -> t -> int
