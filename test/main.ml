open OUnit2
open Tracker
open Entry
open Entrylist
open Commands
open Date
open Calendar

(* Test Plan:

   Our system was comphrensively tested thorugh a mixture of OUnit and
   manual testing. Below is a breakdown of which components of our
   program were tested via OUnit and which were tested manually:

   OUnit: Entry, Calendar, Date, Command, Entrylist, Contact, Network
   Note: Contact and Network are indirectly tested via the testing of
   Entry and Entrylist as most functionality was identical with
   identical functions.

   Manual: Interface

   OUnit test cases were developed via glass-box testing. Bisect was
   used to ensure good code coverage of all executable paths.

   This testing approach demonstrates the correctness of the system as
   it ensures that all lines of written code worked as they were
   intented to. Further, beyond this check, through manually testing the
   program, not only were we able to witness the correctness of our
   interface, but through running commands reliant on the code, we were
   further able to validate that our implementation was correct. *)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare_names lst1 in
  let uniq2 = List.sort_uniq compare_names lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

let test_entry =
  create_entry "test_entry"
    (create_date "06/23/2022")
    "applied" (Yes "note")

let test_entry_s_changed =
  create_entry "test_entry" (create_date "06/23/2022") "interviewed" No

let test_entry_d_changed =
  create_entry "test_entry" (create_date "08/06/2022") "interviewed" No

let test_entry_n_changed =
  create_entry "name_change" (create_date "06/23/2022") "interviewed" No

let test_entry2 =
  create_entry "test_entry2"
    (create_date "06/23/2022")
    "applied" (Yes "note")

let date_list =
  [
    create_date "06/23/2022";
    create_date "06/05/2022";
    create_date "06/25/2023";
    create_date "12/23/2022";
  ]

let date1 = create_date "01/02/2022"
let date2 = create_date "01/01/2022"
let date3 = create_date "01/01/2023"
let date4 = create_date "02/01/2023"
let date_list_2 = [ create_date "06/23/2022"; create_date "06/05/2022" ]

let add_test
    (name : string)
    (e : Entry.entry)
    (input : Entrylist.t)
    (expected_output : Entrylist.t) : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists expected_output
    (Entrylist.add e input)

let add_test_illegal (name : string) e input expected_output : test =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> add e input)

let is_equal_test
    (name : string)
    (e1 : entry)
    (e2 : entry)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (is_equal_entry e1 e2)

let delete_test (name : string) e input expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (delete e input) ~cmp:cmp_set_like_lists

let delete_test_illegal (name : string) e input (expected_output : exn)
    : test =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> delete e input)

let command_test
    (name : string)
    (input : string)
    (expected_output : command) : test =
  name >:: fun _ -> assert_equal expected_output (parse input)

let command_test_illegal
    (name : string)
    (input : string)
    (expected_output : exn) : test =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> parse input)

let entry_names_test
    (name : string)
    (t : entry list)
    (expected_output : string list) : test =
  name >:: fun _ -> assert_equal expected_output (entry_names t)

let change_test
    (name : string)
    (e : entry)
    (lst : Entrylist.t)
    (ne : entry)
    (expected_output : Entrylist.t) : test =
  name >:: fun _ ->
  assert_equal expected_output (Entrylist.change e lst ne)

let state_to_string_test
    (name : string)
    (s : state)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (state_to_string s)

let string_to_state_test
    (name : string)
    (s : string)
    (expected_output : state) : test =
  name >:: fun _ -> assert_equal expected_output (string_to_state s)

let string_to_state_illegal
    (name : string)
    (s : string)
    (expected_output : exn) : test =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> string_to_state s)

let find_entry_test
    (name : string)
    (entry : string)
    (list : Entrylist.t)
    (expected_output : entry) : test =
  name >:: fun _ -> assert_equal expected_output (find_entry entry list)

let find_entry_test_illegal
    (name : string)
    (entry : string)
    (list : Entrylist.t)
    (expected_output : exn) : test =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> find_entry entry list)

let sort_by_name_test
    (name : string)
    (list : Entrylist.t)
    (expected_output : Entrylist.t) : test =
  name >:: fun _ ->
  assert_equal expected_output (Entrylist.sort_by_name list)

let sort_by_status_test
    (name : string)
    (list : Entrylist.t)
    (expected_output : Entrylist.t) : test =
  name >:: fun _ -> assert_equal expected_output (sort_by_status list)

let notes_string_test
    (name : string)
    (e : entry)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (notes_string e)

let start_weekday_test
    (name : string)
    (month : string)
    (year : string)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (start_weekday month year)

let calendar_header_test
    (name : string)
    (month : string)
    (year : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (calendar_header month year)

let calendar_header_test_illegal
    (name : string)
    (month : string)
    (year : string)
    (expected_output : exn) : test =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> calendar_header month year)

let first_weekday_test
    (name : string)
    (year : string)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (first_weekday year)

let start_day_test
    (name : string)
    (leap : bool)
    (m : months)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (start_day leap m)

let filter_days_test
    (name : string)
    (lst : Date.t list)
    (month : string)
    (year : string)
    (expected_output : Date.t list) =
  name >:: fun _ ->
  assert_equal expected_output (filter_days lst month year)

let create_date_test
    (name : string)
    (s : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (date_string (create_date s))

let create_date_test_illegal
    (name : string)
    (s : string)
    (expected_output : exn) : test =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> create_date s)

let get_days_test (name : string) (m : months) (expected_output : int) :
    test =
  name >:: fun _ -> assert_equal expected_output (get_days m)

let is_leap_test (name : string) (y : string) (expected_output : bool) :
    test =
  name >:: fun _ -> assert_equal expected_output (is_leap y)

let days_in_month_test
    (name : string)
    (b : bool)
    (m : string)
    (expected_output : months) : test =
  name >:: fun _ -> assert_equal expected_output (days_in_month b m)

let date_string_test
    (name : string)
    (d : Date.t)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (date_string d)

let day_test (name : string) (d : Date.t) (expected_output : string) :
    test =
  name >:: fun _ -> assert_equal expected_output (day d)

let month_test (name : string) (d : Date.t) (expected_output : string) :
    test =
  name >:: fun _ -> assert_equal expected_output (month d)

let year_test (name : string) (d : Date.t) (expected_output : string) :
    test =
  name >:: fun _ -> assert_equal expected_output (year d)

let compare_dates_test
    (name : string)
    (d1 : Date.t)
    (d2 : Date.t)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (compare_dates d1 d2)

let process_test (name : string) (s : string) (expected_output : string)
    : test =
  name >:: fun _ -> assert_equal expected_output (process s)

let valid_s_test (name : string) (s : string) (expected_output : bool) :
    test =
  name >:: fun _ -> assert_equal expected_output (valid_s s)

let date_test (name : string) (e : entry) (expected_output : Date.t) :
    test =
  name >:: fun _ -> assert_equal expected_output (date e)

let notes_test (name : string) (e : entry) (expected_output : has_note)
    : test =
  name >:: fun _ -> assert_equal expected_output (notes e)

let entry_tests =
  [
    is_equal_test "testing equality of two equal entries" test_entry
      test_entry true;
    is_equal_test "testing equalityof two unequal entries" test_entry
      test_entry2 false;
    string_to_state_test
      "matching the string applied with the constructor Applied"
      "applied" Applied;
    state_to_string_test
      "matching the constructor Interviewed with interviewed"
      Interviewed "interviewed";
    string_to_state_illegal
      "attempting to illegally match a string with a constructor"
      "completed" InvalidArg;
    notes_string_test "retrieving notes when they exist" test_entry2
      "note";
    notes_string_test "retrieving notes when they don't exist"
      test_entry_n_changed "No notes";
    process_test "processing empty string" "" "";
    process_test "processing string with extra spaces"
      "    process   test " "process test";
    valid_s_test "'new' is a valid state" "new" true;
    valid_s_test "'old' is not a valid state" "old" false;
    date_test "date of test_entry is 06/23/2022" test_entry
      (create_date "06/23/2022");
    notes_test "notes of test_entry is Yes" test_entry (Yes "note");
  ]

let entry_list_tests =
  [
    add_test "adding a new entry to an entry list" test_entry []
      [ test_entry ];
    add_test_illegal "adding a duplicate entry to an entry list"
      test_entry [ test_entry ] Duplicate;
    delete_test "removing an entry from a non-empty entry list"
      (name test_entry) [ test_entry ] [];
    delete_test "removing an entry from an entry list of size 2"
      (name test_entry)
      [ test_entry2; test_entry ]
      [ test_entry2 ];
    delete_test_illegal "removing an entry from an empty entry list"
      (name test_entry) [] NotMem;
    delete_test_illegal
      "removing an entry that is not a member of an entry list"
      (name test_entry2) [ test_entry ] NotMem;
    change_test
      "changing the status of an entry from applied to interviewed"
      test_entry [ test_entry ] test_entry_s_changed
      [ test_entry_s_changed ];
    change_test
      "changing the status of an entry from applied to interviewed"
      test_entry [ test_entry ] test_entry_n_changed
      [ test_entry_n_changed ];
    change_test
      "change the date of an entry from 06/23/2022 to 08/06/2022"
      test_entry [ test_entry ] test_entry_d_changed
      [ test_entry_d_changed ];
    find_entry_test_illegal
      "finding an entry that is not a member of an entry list"
      (name test_entry2) [ test_entry ] NotMem;
    find_entry_test "finding an entry that is within an entry list"
      (name test_entry) [ test_entry ] test_entry;
    sort_by_name_test "sorting by name for internships"
      [ test_entry_n_changed; test_entry2; test_entry ]
      [ test_entry_n_changed; test_entry; test_entry2 ];
    sort_by_status_test "sorting by status for internships"
      [ test_entry_s_changed; test_entry2; test_entry ]
      [ test_entry2; test_entry; test_entry_s_changed ];
    entry_names_test "getting names of all entries in a list"
      [ test_entry; test_entry2; test_entry_n_changed ]
      [ "test_entry"; "test_entry2"; "name_change" ];
  ]

let command_tests =
  [
    command_test "parsing add command" "add" Add;
    command_test "parsing delete command" "delete internship name"
      (Delete [ "internship"; "name" ]);
    command_test "parsing view command" "view" View;
    command_test "parsing quit command" "quit" Quit;
    command_test "parsing update command" "update internship name"
      (Update [ "internship"; "name" ]);
    command_test "parsing notes command" "notes for internship name"
      (Notes [ "internship"; "name" ]);
    command_test "parsing network command" "network" Network;
    command_test "parsing command with spaces" "     add     " Add;
    command_test "parsing command with spaces" "     calendar     "
      Calendar;
    command_test_illegal "parsing an empty command" "" Malformed;
    command_test_illegal "parsing a malformed delete command" "Delete"
      Malformed;
    command_test_illegal "parsing a malformed add command" "Ad"
      Malformed;
  ]

let date_tests =
  [
    create_date_test "February 29th, 2024 is a valid date " "02/29/2024"
      "02/29/2024";
    create_date_test "April 1st, 2022 is a valid date " "04/01/2022"
      "04/01/2022";
    create_date_test_illegal "February 29th, 2020 is not a valid date "
      "02/29/2020" InvalidDate;
    create_date_test_illegal "00/00/0000 is not a valid date "
      "00/00/0000" InvalidDate;
    is_leap_test "a non-leap year" "2001" false;
    is_leap_test "a leap year" "2004" true;
    get_days_test "There are 31 days in January" (Jan 31) 31;
    date_string_test "converting a date to a string" date1 "01/02/2022";
    days_in_month_test "extracting the number of days in June" true "06"
      (Jun 30);
    compare_dates_test
      "comparing two unequal dates with the first greater than the \
       second"
      date1 date2 1;
    year_test "extracting the year from a date" date1 "2022";
    month_test "extracting the month from a date" date1 "01";
    day_test "extracting the day from a date" date1 "02";
    compare_dates_test
      "comparing two unequal dates with the first smaller than the \
       second"
      date2 date1 (-1);
    compare_dates_test "comparing two equal dates" date1 date1 0;
    compare_dates_test
      "comparing two uequal dates with the first greater than the \
       second"
      date3 date2 1;
    compare_dates_test
      "comparing two unequal dates with the first greater than the \
       second"
      date4 date3 1;
  ]

let calendar_tests =
  [
    start_weekday_test "start weekday of regular month" "03" "2022" 2;
    start_weekday_test "start weekday of month that starts on Friday"
      "01" "2022" 6;
    start_weekday_test "start weekday of month in leap year" "07" "2024"
      1;
    first_weekday_test "first weekday of leap year" "2024" 1;
    first_weekday_test "first weekday of non-leap year" "2023" 0;
    start_day_test "start day for january in leap year" true (Jan 31) 1;
    start_day_test "start day for january in non-leap year" false
      (Jan 31) 1;
    start_day_test "start day for june in non-leap year" false (Jun 30)
      152;
    start_day_test "start day for june in leap year" true (Jun 30) 153;
    start_day_test "start day for december in leap year" true (Dec 31)
      336;
    start_day_test "start day for december in non-leap year" false
      (Dec 31) 335;
    filter_days_test "filter days in june 2022" date_list "06" "2022"
      date_list_2;
    filter_days_test "filter days in june 2023" date_list "06" "2023"
      [ create_date "06/25/2023" ];
    filter_days_test "filter empty list" [] "12" "2025" [];
    filter_days_test "filter list with date that doesn't exist"
      date_list "07" "08" [];
    calendar_header_test "Calendar header should be October 2022" "10"
      "2022" "October 2022";
    calendar_header_test "Calendar header should be July 2001" "07"
      "2001" "July 2001";
    calendar_header_test "Calendar header should be June 2013" "06"
      "2013" "June 2013";
    calendar_header_test_illegal
      "Calendar header should raise an expression" "invalid" "2022"
      InvalidDate;
  ]

let suite =
  "test suite for Internship Tracker"
  >::: List.flatten
         [
           entry_tests;
           entry_list_tests;
           command_tests;
           date_tests;
           calendar_tests;
         ]

let _ = run_test_tt_main suite
