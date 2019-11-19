open Deck
open Player
open State
open Command
open Hands
open Rank_hand

(** [create_player_list x acc] is the player list consisting of [x] players*)
let rec create_player_list x acc =
  if x = 0 then List.rev acc
  else (print_endline ("Player "^(string_of_int((List.length acc)+1))^ ": enter your name\nNote: your name must be unique.");
        print_string "> ";
        match read_line () with
        | n -> create_player_list (x-1) (create_player n::acc))

(** [remove_blinds_aux lst acc] is [lst] without any blinds on the players. *)
let rec remove_blinds_aux lst acc =
  match lst with 
  | h :: t -> remove_blinds_aux t (change_blind h None :: acc)
  | _ -> List.rev acc

(** [remove_blinds lst] is [lst] with all blinds removed from players. *)
let rec remove_blinds lst =
  remove_blinds_aux lst []

(** [rotate__game_aux lst acc] is the tuple of the last element in [lst] and the 
    remaining elements of [lst]. *)
let rec rotate_game_aux lst acc =
  match lst with
  | h :: [] -> (h, List.rev acc)
  | h :: t -> rotate_game_aux t (h :: acc)
  | [] -> failwith "no last element in an empty list"

(** [rotate_game lst] is a list that is logically shifted right. *)
let rotate_game lst =
  match rotate_game_aux (remove_blinds lst) [] with
  | (l, rem) -> [l] @ rem

(** [rotate_first_round lst] is [lst] logically shifted left twice. *)
let rec rotate_first_round lst =
  match lst with
  | h1 :: h2 :: t -> t @ [h1] @ [h2]
  | _ -> failwith "rotating an invalid player list"

let rec cb_lst_aux lst p acc =
  match lst with
  | [] -> failwith "player is not in the list"
  | h :: [] -> (List.rev acc) @ [h]
  | h :: t -> if name h = name p then t @ (List.rev acc) @ [h] else cb_lst_aux t p (h :: acc)

let rec cb_lst p lst =
  cb_lst_aux lst p []

(** [set_blinds_aux lst] is [lst] with the first player given a small blind
    and the second player a big blind in the rotated list. *)
let set_blinds_aux lst  =
  match lst with
  | h1 :: h2 :: t -> (change_blind h1 Small) :: (change_blind h2 Big) :: t
  | _ -> failwith "can't set blinds of this list of players"

(** [set_blinds st] is a state with the first player in the active player list
    having a small blind and the second player having a big blind. *)
let set_blinds st =
  change_active_players st (set_blinds_aux (active_players st))

let take_blind_money'' p =
  match blind p with
  | Small -> change_money p (-25) 
  | Big -> change_money p (-50) 
  | None -> change_money p (0)

let rec take_blind_money' players acc =
  match players with 
  | [] -> List.rev acc
  | h :: t -> take_blind_money' t (take_blind_money'' h :: acc)

let take_blind_money st =
  let st' = change_active_players st (take_blind_money' (active_players st) []) in
  let st'' = change_current_bet st' 50 in
  change_betting_pool st'' 75 

let display_blind_money p =
  match blind p with
  | Small -> print_endline (name p ^ ", you are small blind, so you automatically bet 25.
You now only have $" ^ string_of_int (money p) ^ " left.")
  | Big -> print_endline (name p ^ ", you are big blind, so you automatically bet 50.
You now only have $" ^ string_of_int (money p) ^ " left.")
  | None -> failwith "not a blind" 

let rec show_blind_info st players=
  match players with 
  | [] -> ()
  | h :: t -> if blind h = None then show_blind_info st t 
    else display_blind_money h; show_blind_info st t


(** [show_info_helper p] prints the private information of player [p]*)
let show_info p st =
  let money = money p in let hand = hand p |> to_string in 
  print_endline ("You have " ^ (string_of_int money) ^ " dollars and your cards are\n" ^ hand);
  let t = st |> table |> to_string in
  if t = "" then  print_endline "There are no cards currently on the table"
  else print_endline ("The table currently has the cards: \n" ^ t)

let show_flop st =
  print_endline "We will now reveal the flop.";
  print_endline ("The table currently has the cards: \n" ^ 
                 (st |> table |> to_string));
  print_endline "Ready to bet?\n"

let show_river st =
  print_endline "We will now reveal the river.";
  print_endline ("The table currently has the cards: \n" ^ 
                 (st |> table |> to_string));
  print_endline "Ready to bet?\n"

let show_turn st =
  print_endline "We will now reveal the turn.";
  print_endline ("The table currently has the cards: \n" ^ 
                 (st |> table |> to_string));
  print_endline "Ready to bet?\n"

let rec execute str p st =
  match parse str with
  | Quit -> 
    begin
      print_endline "You cannot quit in the middle of the game. Please try again!";
      print_string "> ";
      match read_line () with
      | str -> execute str p st
    end
  | Fold -> (fold st p, false)
  | Check -> 
    begin
      match check st p with 
      | exception InvalidBet -> 
        begin
          print_endline "You cannot check when the current bet is not 0. Please try again!";
          print_string "> ";
          match read_line () with
          | str -> execute str p st
        end
      | st' -> (st', false)
    end
  | Call -> (call st p, false)
  | Allin -> 
    begin
      match all_in st p with 
      | exception InvalidBet -> 
        begin
          print_endline "You cannot check when the current bet is not 0. Please try again!";
          print_string "> ";
          match read_line () with
          | str -> execute str p st
        end
      | st' -> (st', false)
    end
  | Raise i -> 
    begin
      match raise i st p with 
      | exception InvalidBet -> 
        begin
          print_endline "You cannot check when the current bet is not 0. Please try again!";
          print_string "> ";
          match read_line () with
          | str -> execute str p st
        end
      | st' -> (st', true)
    end
  | exception Empty -> 
    begin
      print_endline "You cannot enter an empty command. Please try again!";
      print_string "> ";
      match read_line () with
      | str -> execute str p st
    end
  | exception Malformed -> 
    begin
      print_endline "We did not recognize that. Please try again!";
      print_string "> ";
      match read_line () with
      | str -> execute str p st
    end

let rec everyone_gets_a_turn players st =
  match players with
  | [] -> st
  | h::t -> print_endline ((name h)^", press enter when you are alone");
    print_string "> ";
    match read_line () with 
    | _ -> 
      show_info h st; 
      print_endline ("What would you like to do?");
      print_string "> ";
      begin 
        match read_line () with
        | str -> ANSITerminal.erase Screen; 
          let act = execute str h st in 
          match act with
          | (st', b) -> 
            if b then continued_betting (st' |> active_players |> cb_lst h) st' 
            else betting_aux true t st'
      end

and continued_betting players st = 
  match players with
  | [] -> failwith "betting without players"
  | h :: [] -> st
  | h::t -> print_endline "previous player just raised"; print_endline ((name h)^", press enter when you are alone");
    print_string "> ";
    match read_line () with 
    | _ -> 
      show_info h st; 
      print_endline ("What would you like to do?");
      print_string "> ";
      begin 
        match read_line () with
        | str -> ANSITerminal.erase Screen; 
          let act = execute str h st in 
          match act with
          | (st', b) -> 
            if b then continued_betting (st' |> active_players |> cb_lst h) st' 
            else betting_aux false t st'
      end

(** [betting_aux players] prints the private information of each players to 
    the terminal, clearing the terminal between each player*)
and betting_aux b players st =
  if b then everyone_gets_a_turn players st else continued_betting players st

let first_round_betting st =
  betting_aux true (st |> active_players |> rotate_first_round) st

(** [betting st] prints the private information of each players to 
    the terminal, clearing the terminal between each player*)
let rec betting st =
  betting_aux true (st |> active_players) st

let flop st = 
  let d = draw_card 3 (rem_deck st) in
  let st' = change_table (change_rem_deck (snd d) st) (fst d) in 
  show_flop st'; st'

let turn st = 
  let d = draw_card 1 (rem_deck st) in
  let st' = change_table (change_rem_deck (snd d) st) (fst d) in 
  show_turn st'; st'

let river st = 
  let d = draw_card 1 (rem_deck st) in
  let st' = change_table (change_rem_deck (snd d) st) (fst d) in 
  show_river st'; st'

let finish st =
  print_endline "That's all for the demo game folks!";
  st

(** [start_game x] initializes a game with [x] players*)
let start_game x =
  let st = (create_player_list x []) |> new_round |> set_blinds in
  let st' = take_blind_money st in
  show_blind_info st' (active_players st');
  st' 
  |> first_round_betting  
  |> flop 
  |> betting 
  |> turn 
  |> betting 
  |> river 
  |> betting 
  (*|> showdown *) 
  |> finish

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  (print_string
     "\n\nWelcome to Texas Hold 'Em.\n");
  print_endline "Please enter the number of players in your game. \n Note there may only be 2-10 players in your game";
  print_string  "> ";
  match int_of_string(read_line ()) with
  | exception e -> print_endline "You must enter a number. 
                            Start the game engine again to play"
  | x -> if (x > 10 || x <2 ) then print_endline "You must enter a number between 2 and 10.
                            Start the game engine again to play" 
    else match start_game x with 
      | _ -> ()

let () = main ()


