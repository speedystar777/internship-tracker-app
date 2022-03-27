open Date

let first_weekday y = 6

let start_day l = function
  | Jan d -> 1
  | Feb d -> 32
  | Mar d -> if l then 61 else 60
  | Apr d -> if l then 92 else 91
  | May d -> if l then 122 else 121
  | Jun d -> if l then 153 else 152
  | Jul d -> if l then 183 else 182
  | Aug d -> if l then 214 else 213
  | Sep d -> if l then 245 else 244
  | Oct d -> if l then 275 else 274
  | Nov d -> if l then 306 else 305
  | Dec d -> if l then 336 else 335

let start_weekday m y =
  let d = days_in_month (is_leap y) m |> start_day (is_leap y) in
  let day = first_weekday y in
  ((day + (d mod 7)) mod 7) - 1

let calendar_header m y =
  match m with
  | "01" -> "January" ^ " " ^ y
  | "02" -> "February" ^ " " ^ y
  | "03" -> "March" ^ " " ^ y
  | "04" -> "April" ^ " " ^ y
  | "05" -> "May" ^ " " ^ y
  | "06" -> "June" ^ " " ^ y
  | "07" -> "July" ^ " " ^ y
  | "08" -> "August" ^ " " ^ y
  | "09" -> "September" ^ " " ^ y
  | "10" -> "October" ^ " " ^ y
  | "11" -> "November" ^ " " ^ y
  | "12" -> "December" ^ " " ^ y
  | _ -> raise InvalidDate

let weekday_header = "Su  Mo  Tu  We  Th  Fr  Sa"

let filter_days (d_list : Date.t list) m y =
  List.filter (fun x -> month x = m && year x = y) d_list
