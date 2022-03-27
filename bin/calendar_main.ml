open Tracker
open ANSITerminal
open Date
open Entry
open Entrylist
open Commands
open Calendar
open Message_strings

let rec get_month_year s =
  try
    let lst = String.split_on_char '/' s in
    match lst with
    | [ m; y ] ->
        if valid_month m && valid_year y then [ m; y ]
        else raise InvalidDate
    | [ "quit" ] -> [ "quit" ]
    | _ -> raise InvalidDate
  with InvalidDate ->
    print_endline
      "The date you have entered is invalid. Please enter a date in \
       the format 'mm/yyyy'.";
    Printf.printf "> ";
    get_month_year (read_line ())

let rec repeat_spaces n =
  match n with
  | 0 -> ""
  | n -> "    " ^ repeat_spaces (n - 1)

let rec build_calendar max count days curr =
  if count <= 26 && curr <= max then (
    ANSITerminal.print_string [ ANSITerminal.white ]
      (if String.length (string_of_int curr) = 1 then
       " " ^ string_of_int curr ^ "  "
      else string_of_int curr ^ "  ");
    build_calendar max (count + 4) days (curr + 1))
  else if count > 26 && curr <= max then (
    print_endline "";
    build_calendar max 0 days curr)
  else print_endline ""

let print_calendar st m y =
  print_endline (calendar_header m y);
  print_endline weekday_header;
  let start_d = start_weekday m y in
  let days = entry_dates st |> filter_days in
  ANSITerminal.print_string [ ANSITerminal.white ]
    (repeat_spaces start_d);
  build_calendar
    (days_in_month (is_leap y) m |> get_days)
    (start_d * 2) days 1

let rec make_calendar (st : Entrylist.t) msg =
  print_endline msg;
  Printf.printf "> ";
  match get_month_year (read_line ()) with
  | [ m; y ] ->
      print_calendar st m y;
      make_calendar st cal_start_msg
  | [ "quit" ] -> ""
  | _ -> make_calendar st cal_start_msg