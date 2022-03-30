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
    print_endline date_error;
    Printf.printf "> ";
    get_month_year (read_line ())

let rec repeat_spaces n =
  match n with
  | 0 -> ""
  | n -> "    " ^ repeat_spaces (n - 1)

let add_needed_spaces curr =
  let curr_string = string_of_int curr in
  if String.length curr_string = 1 then " " ^ curr_string ^ "  "
  else curr_string ^ "  "

let is_mem days day = List.exists (fun x -> int_of_string x = day) days

let rec build_calendar max count days curr =
  if count <= 26 && curr <= max then (
    if is_mem days curr then
      ANSITerminal.print_string [ ANSITerminal.red ]
        (add_needed_spaces curr)
    else
      ANSITerminal.print_string [ ANSITerminal.white ]
        (add_needed_spaces curr);
    build_calendar max (count + 4) days (curr + 1))
  else if count > 26 && curr <= max then (
    print_endline "";
    build_calendar max 0 days curr)
  else print_endline ""

let print_calendar st m y =
  print_endline "";
  print_endline (calendar_header m y);
  print_endline weekday_header;
  let start_d = start_weekday m y in
  let days = filter_days (entry_dates st) m y in
  let starred_days = List.map day days in
  ANSITerminal.print_string [ ANSITerminal.white ]
    (repeat_spaces start_d);
  build_calendar
    (days_in_month (is_leap y) m |> get_days)
    (start_d * 4) starred_days 1

let rec make_calendar (st : Entrylist.t) msg =
  print_endline msg;
  Printf.printf "> ";
  match get_month_year (read_line ()) with
  | [ m; y ] ->
      print_calendar st m y;
      make_calendar st cal_start_msg
  | [ "quit" ] -> ""
  | _ -> make_calendar st cal_start_msg