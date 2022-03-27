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

type t = {
  month : string;
  day : string;
  year : string;
}

exception InvalidDate

(*let current_time = Unix.localtime (Unix.time ())*)

let valid_month m =
  String.length m = 2 && int_of_string m > 0 && int_of_string m <= 12

let is_leap y = int_of_string y mod 4 = 0

let parse_date s =
  match String.split_on_char '/' s with
  | [ m; d; y ] -> (m, d, y)
  | _ -> raise InvalidDate

let days_in_month leap = function
  | "01" -> Jan 31
  | "02" -> Feb (if leap then 29 else 28)
  | "03" -> Mar 31
  | "04" -> Apr 30
  | "05" -> May 31
  | "06" -> Jun 30
  | "07" -> Jul 31
  | "08" -> Aug 31
  | "09" -> Sep 30
  | "10" -> Oct 31
  | "11" -> Nov 30
  | "12" -> Dec 31
  | _ -> raise InvalidDate

let valid_year y =
  String.length y = 4
  && int_of_string y >= 2000
  && int_of_string y <= 2100

let get_days = function
  | Jan d -> d
  | Feb d -> d
  | Mar d -> d
  | Apr d -> d
  | May d -> d
  | Jun d -> d
  | Aug d -> d
  | Sep d -> d
  | Oct d -> d
  | Nov d -> d
  | Dec d -> d
  | _ -> 0

let valid_day m d y =
  let leap = is_leap y in
  let max_days = days_in_month leap m |> get_days in
  int_of_string d <= max_days && int_of_string d > 0

let create_date s =
  let m, d, y = parse_date s in
  if valid_month m && valid_day m d y && valid_year y then
    { month = m; day = d; year = y }
  else raise InvalidDate

let date_string d = d.month ^ "/" ^ d.day ^ "/" ^ d.year
let month t = t.month
let year t = t.year
let day t = t.day
