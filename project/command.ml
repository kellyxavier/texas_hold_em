type amount = int

type command = 
  | Quit
  | Fold
  | Call
  | Check 
  | Allin
  | Bet of amount
  | Raise of amount

exception Empty

exception Malformed

let parse str = 
  let phrase = String.trim str in
  if phrase = "" then raise Empty else
    let word_list = String.split_on_char ' ' (phrase) in
    let trimmed_word_list = List.filter (fun s -> s <> "") word_list in
    let command = List.nth trimmed_word_list 0 in
    if phrase = "quit" then Quit
    else if phrase = "fold" then Fold
    else if phrase = "call" then Call
    else if phrase = "allin" then Allin
    else if List.length trimmed_word_list = 2 && (command = "bet" 
                                                  || command = "raise") 
    then match int_of_string (List.nth trimmed_word_list 1) with
      | i -> if List.nth trimmed_word_list 0 = "raise" then Raise i else Bet i
      | exception _ -> raise Malformed
    else raise Malformed
