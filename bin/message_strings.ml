let string_of_list =
  let rec helper acc = function
    | [] -> acc
    | h :: t -> helper (acc ^ "[" ^ h ^ "] \n") t
  in
  helper ""

let date_error =
  "The date you have entered is invalid. The date must be in the form \
   mm/dd/yyyy and must be within or after the current year.\n"

let status_error =
  "Please enter a valid status from: new, applied, interviewed, \
   accepted, rejected"

let dup_message = "\nWARNING: You cannot enter duplicate entry names."

let command_message =
  "\n\nPlease enter the action you would like to complete."
  ^ "\n\
     The options include 'add', 'delete [name of internship]', 'view', \
     'update [name of internship]', \n\
    \ 'notes for [name of internship]', 'network', 'calendar', 'quit'.\n"

let notfound_message =
  "\nWARNING: The entry you are trying to query does not exist."

let deleted_msg name = "\nThe deletion of " ^ name ^ " was successful!"
let cmd_string lst = String.concat " " lst
let notes_error_msg = "\nCould not recognize your command."

let network_msg =
  "\n\
   Please enter the action you would like to complete.\n\
  \ The options include 'add' 'delete [name of contact]', 'view', \
   'update [name of contact]', 'notes for [name of contact]', 'quit'."

let name_error =
  "\nCannot enter an empty string or just spaces for internship name"

let cal_start_msg =
  "\n\
   Please enter the month and year you would like to view a calendar \
   for in the form 'mm/yyyy'."

let cal_illegal_msg =
  "Invalid command. Please enter a valid month and year or 'quit'."

let status_msg =
  "The options are 'new', 'applied', 'interviewed', 'accepted', \
   'rejected'."
