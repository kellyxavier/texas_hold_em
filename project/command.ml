type amount = int

type command = 
  | Default
  | Quit
  | Continue
  | Fold
  | Call
  | Check 
  | Allin
  | Raise of amount

type difficulty =
  | Easy
  | Med
  | Hard

exception Empty

exception Malformed

(** [parse_raise trm_word_list] parses player's input into a [Call] command if 
    attempting to raise 0, parses player's input into a [Raise i] command if 
    attempting to raise another integer.
    Raises: [Malformed] if the commmand is malformed*)
let parse_raise trm_word_list =
  match int_of_string (List.nth trm_word_list 1) with
  | 0 -> Call
  | i -> Raise i
  | exception _ -> raise Malformed

let parse str = 
  let str_lower = String.lowercase_ascii str in
  let phrase = String.trim str_lower in
  if phrase = "" then raise Empty else
    begin
      let word_list = String.split_on_char ' ' (phrase) in
      let trimmed_word_list = List.filter (fun s -> s <> "") word_list in
      let command = List.nth trimmed_word_list 0 in
      if phrase = "quit" then Quit
      else if phrase = "continue" then Continue
      else if phrase = "fold" then Fold
      else if phrase = "call" then Call
      else if phrase = "check" then Check
      else if phrase = "allin" then Allin
      else if List.length trimmed_word_list = 2 &&  command = "raise" 
      then parse_raise trimmed_word_list
      else raise Malformed
    end

let diff str =
  let str_lower = String.lowercase_ascii str in
  let phrase = String.trim str_lower in
  if phrase = "" then raise Empty
  else if phrase = "easy" then Easy
  else if phrase = "medium" then Med
  else if phrase = "hard" then Hard
  else raise Malformed

let quit_error =
  "You cannot quit in the middle of the game. Please try again!"

let continue_error =
  "Please enter a betting action."

let check_error =
  "You cannot check when the current bet is not 0. Please try again!"

let raise_error =
  "You may only bet an amount between $0 and the wallet of the poorest player. "
  ^ "Please try again!"

let empty_error =
  "You cannot enter an empty command. Please try again!"

let malformed_error =
  "We did not recognize that. Please try again!"

let move_to_string m =
  match m with
  | Default -> "Has not made a move yet"
  | Quit -> "Quit"
  | Continue -> "Continue"
  | Fold -> "Folded"
  | Call -> "Called"
  | Check -> "Checked"
  | Allin -> "Went all in"
  | Raise i -> "Raised " ^ string_of_int i
