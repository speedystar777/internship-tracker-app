open OUnit2
open Tracker
open Record

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare_entries lst1 in
  let uniq2 = List.sort_uniq compare_entries lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ ";\n   ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let test_entry = create_entry "test_entry" "00-00-0000" "applied"
let test_entry2 = create_entry "test_entry2" "00-00-0000" "applied"

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
    (expected_output : Commands.command) : test =
  name >:: fun _ -> assert_equal expected_output (Commands.parse input)

let command_test_illegal
    (name : string)
    (input : string)
    (expected_output : exn) : test =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> Commands.parse input)

let record_tests =
  [
    is_equal_test "testing equality of two equal entries" test_entry
      test_entry true;
    is_equal_test "testing equalityof two unequal entries" test_entry
      test_entry2 false;
    add_test "adding a new entry to an entry list" test_entry []
      [ test_entry ];
    add_test_illegal "adding a duplicate entry to an entry list"
      test_entry [ test_entry ] Duplicate;
    delete_test "removing an entry from a non-empty entry list"
      (name test_entry) [ test_entry ] [];
    delete_test_illegal "removing an entry from an empty entry list"
      (name test_entry) [] NotFound;
    delete_test_illegal
      "removing an entry that is not a member of an entry list"
      (name test_entry2) [ test_entry ] NotFound;
  ]

let command_test =
  [
    command_test "parsing add command" "add" Add;
    command_test "parsing delete command" "delete internship name"
      (Delete [ "internship"; "name" ]);
    command_test "parsing view command" "view" View;
    command_test "parsing command with spaces" "     add     " Add;
    command_test_illegal "parsing an empty command" ""
      Commands.Malformed;
    command_test_illegal "parsing a malformed delete command" "Delete"
      Commands.Malformed;
    command_test_illegal "parsing a malformed add command" "Ad"
      Commands.Malformed;
  ]

let suite =
  "test suite for Internship Tracker"
  >::: List.flatten [ record_tests; command_test ]

let _ = run_test_tt_main suite
