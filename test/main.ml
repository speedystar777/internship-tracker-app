open OUnit2
open Tracker
open Entry
open Entrylist
open Commands
open Date

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

let add_test (name : string) e input expected_output : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists expected_output (add e input)

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
  name >:: fun _ -> assert_equal expected_output (change e lst ne)

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
  name >:: fun _ -> assert_equal expected_output (sort_by_name list)

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
      (name test_entry) [] NotFound;
    delete_test_illegal
      "removing an entry that is not a member of an entry list"
      (name test_entry2) [ test_entry ] NotFound;
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
      (name test_entry2) [ test_entry ] NotFound;
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
    command_test_illegal "parsing an empty command" "" Malformed;
    command_test_illegal "parsing a malformed delete command" "Delete"
      Malformed;
    command_test_illegal "parsing a malformed add command" "Ad"
      Malformed;
  ]

let suite =
  "test suite for Internship Tracker"
  >::: List.flatten [ entry_tests; entry_list_tests; command_tests ]

let _ = run_test_tt_main suite
