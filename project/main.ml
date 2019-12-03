open Deck
open Player
open State
open Command
open Hands
open Rank_hand

(** [get_valid_name lst i] is a name prompted by the [i]th user that is neither 
    empty nor already in [lst]. *)
let rec get_valid_name lst i =
  print_endline ("Player " ^ (string_of_int i) ^
    ": enter your name\nNote: your name must be unique.");
  print_string "> ";
  match read_line () with
  | "" ->
    begin 
      print_endline "You cannot have an empty name. Please try again.";
      get_valid_name lst i
    end
  | n -> 
    if List.mem n lst then 
      begin
        print_endline "You cannot choose a name that someone has already selected. Please try again.";
        get_valid_name lst i
      end
    else n

(** [create_player_list x acc] is the player list built off of [acc] 
    consisting of [x] players. *)
let rec create_player_list x acc =
  if x = 0 then List.rev acc
  else create_player_list (x - 1) (
    (List.length acc 
    |> (+) 1 
    |> get_valid_name (List.map (fun p -> name p) acc) 
    |> create_player
    ) :: acc)

(** [more_than_one_player st] is true if there's more than one active player
    in the game and false otherwise. *)
let only_one_player st =
  (active_players st) |> List.length < 2

(** [remove_blinds_aux lst acc] is [lst] without any blinds on the players. *)
let rec remove_blinds_aux lst acc =
  match lst with 
  | h :: t -> remove_blinds_aux t (change_blind h None :: acc)
  | _ -> List.rev acc

(** [remove_blinds lst] is [lst] with all blinds removed from players. *)
let rec remove_blinds lst =
  remove_blinds_aux lst []

(** [rotate lst] is [lst] rotated left once. *)
let rotate lst =
  match remove_blinds lst with
  | [] -> failwith "cannot rotate an empty list"
  | h :: t -> t @ [h]

(** [rotate_first_round lst] is [lst] logically shifted left twice. *)
let rec rotate_first_round lst =
  match lst with
  | h1 :: h2 :: t -> t @ [h1] @ [h2]
  | _ -> failwith "rotating an invalid player list"

(** [place_last_aux p lst acc] is the tail-recursive implementation of 
    [place_last p lst]. *)
let rec place_last_aux lst p acc =
  match lst with
  | [] -> failwith "player is not in the list"
  | h :: [] -> (List.rev acc) @ [h]
  | h :: t -> 
    if name h = name p 
    then t @ (List.rev acc) @ [h] 
    else place_last_aux t p (h :: acc)

(** [place_last p lst] is [lst] with [p] placed at the end, 
    while perserving order. *)
let rec place_last p lst =
  place_last_aux lst p []

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

(** [blinded p] is [p] with the appropriate amount of money removed for 
    its blind or lackthereof. Small blinds are worth $25, big blinds are $50. *)
let blinded p =
  match blind p with
  | Small -> change_money p (-25) 
  | Big -> change_money p (-50) 
  | None -> change_money p (0)

(** [take_blind_money_aux players acc] is [players] with the appropriate
    amount of money removed from their wallet according to their blind. *)
let rec take_blind_money_aux players acc =
  match players with 
  | [] -> List.rev acc
  | h :: t -> take_blind_money_aux t (blinded h :: acc)

(** [take_blind_money st] is [st] with an active player list, current bet, and
    betting pool reflective of the blinds. *)
let take_blind_money st =
  let st' = 
    change_active_players st (take_blind_money_aux (active_players st) []) in
  let st'' = change_current_bet st' 50 in
  change_betting_pool st'' 75 

(** [display_blind_money p] prints out [p]'s blind and value, along with how
    much money they have left. *)
let display_blind_money p =
  match blind p with
  | Small -> print_endline ("\n\n" ^ name p ^ 
    ", you are small blind, so you automatically bet $25.\nYou now only have $" 
    ^ string_of_int (money p) ^ " left.")
  | Big -> print_endline ("\n\n" ^ name p ^ 
    ", you are big blind, so you automatically bet $50.\nYou now only have $" 
    ^ string_of_int (money p) ^ " left.")
  | None -> print_string ""

(** [show_blind_info players] prints out the blind information of each
    player in [players]. *)
let rec show_blind_info players=
  match players with 
  | [] -> ()
  | h :: t -> display_blind_money h; show_blind_info t 

let rec print_prev_moves players =
  match players with
  | [] -> ()
  | h :: t -> print_endline (name h ^ ": " ^ move_to_string (last_move h));
    print_prev_moves t

(** [show_info p st] prints the private information of player [p]*)
let show_info p st =
  let money = money p in let hand = hand p |> to_string in 
  let t = st |> table |> to_string in
  print_endline 
    ("These are the last moves of each player currently at the table:");
  print_prev_moves (active_players st);
  print_endline 
    ("\nYou have $" ^ (string_of_int money) ^ " and your cards are:\n" ^ hand);
  if t = "" then print_endline "There are no cards currently on the table"
  else print_endline ("The table currently has the cards: \n" ^ t)

(** [show_flop st] reveals the table with the newly-added flop to all 
    players. *)
let show_flop st =
  print_endline "We will now reveal the flop.";
  print_endline ("The table currently has the cards: \n" ^ 
                 (st |> table |> to_string))

(** [show_river st] reveals the table with the newly-added river to all 
    players. *)
let show_river st =
  print_endline "We will now reveal the river.";
  print_endline ("The table currently has the cards: \n" ^ 
                 (st |> table |> to_string))

(** [show_turn st] reveals the table with the newly-added turn to all 
    players. *)
let show_turn st =
  print_endline "We will now reveal the turn.";
  print_endline ("The table currently has the cards: \n" ^ 
                 (st |> table |> to_string))

(* Valeria thinks these message functions could maybe go someplace else -
   command maybe? *)

(** [quit_error] is the message for when a player tries to quit in the
    middle of a game. *)
let quit_error =
  print_endline "You cannot quit in the middle of the game. Please try again!";
  print_string "> "

(** [continue_error] is the message for when a player tries to continue
    instead of specifying a betting action. *)
let continue_error =
  print_endline "Please enter a betting action.";
  print_string "> "

(** [check_error] is the message for when a player tries to check when they
    haven't betted enough. *)
let check_error =
  print_endline "You cannot check when the current bet is not 0. Please try again!";
  print_string "> "

(** [allin_correction] is the message that explicitly states which actions may
    be taken when a player has gone all in. *)
let allin_correction =
  print_endline "Please either quit, call, or check";
  print_string "> "

(** [empty_command_message] is the message for when a player enters and empty
    command. *)
let empty_command_message =
  print_endline "You cannot enter an empty command. Please try again!";
  print_string "> "

(** [malformed_command_message] is the message for when a player enters a
    malformed command. *)
let malformed_command_message = 
  print_endline "We did not recognize that. Please try again!";
  print_string "> "
(*
(** [try_allin_read_again p st] reads the user's input and passes it into 
    [f]. *)
let rec try_allin_read_again p st =
  match read_line () with
  | str -> execute_all_in str p st*)

(** [execute_all_in str p st] interprets [p]'s input [str] in the context that a 
    player has gone all in and returns [st] that reflects as such. *)
let rec execute_all_in str p st =
  match parse str with
  | Quit -> 
    begin (*
      quit_error;
      try_allin_read_again p st*)
      match read_line () with
      | str -> execute_all_in str p st
    end
  | Continue -> 
    begin
      continue_error;
      match read_line () with
      | str -> execute_all_in str p st
    end
  | Fold -> (fold st p, false)
  | Check -> 
    begin
      match check st p with 
      | exception InvalidBet -> 
        begin
          check_error;
          match read_line () with
          | str -> execute_all_in str p st
        end
      | st' -> (st', false)
    end
  | Call -> (call st p, false)
  | Allin -> 
    begin
      allin_correction;
      match read_line () with
      | str -> execute_all_in str p st
    end
  | Raise i -> 
    begin
      allin_correction;
      match read_line () with
      | str -> execute_all_in str p st
    end
  | exception Empty -> 
    begin
      empty_command_message;
      match read_line () with
      | str -> execute_all_in str p st
    end
  | exception Malformed -> 
    begin
      malformed_command_message;
      match read_line () with
      | str -> execute_all_in str p st
    end
  | Default -> failwith "player-entered command should never parse to Default"

(** [execute str p st] inteprets [p]'s [str] in the context of a regular betting
    round and returns an updated [st] which reflects it. *)
let rec execute str p st =
  match parse str with
  | Quit -> 
    begin
      quit_error;
      match read_line () with
      | str -> execute str p st
    end
  | Continue -> 
    begin
      continue_error;
      match read_line () with
      | str -> execute str p st
    end
  | Fold -> (fold st p, false)
  | Check -> 
    begin
      match check st p with 
      | exception InvalidBet -> 
        begin
          check_error;
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
          print_endline "You may only bet an amount between $0 and the wallet of the poorest player. Please try again!";
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
          print_endline "You may only bet an amount between $0 and the wallet of the poorest player. Please try again!";
          print_string "> ";
          match read_line () with
          | str -> execute str p st
        end
      | st' -> (st', true)
    end
  | exception Empty -> 
    begin
      empty_command_message;
      match read_line () with
      | str -> execute str p st
    end
  | exception Malformed -> 
    begin
      malformed_command_message;
      match read_line () with
      | str -> execute str p st
    end
  | Default -> failwith "player entered command should never parse to Default"

(** [all_no_money players] is true if no player in [players] has money and 
    false otherwise. *)
let rec all_no_money players  =
  match players with
  | [] -> true
  | h :: t -> if money h = 0 then all_no_money t else false 

(** [one_no_money players] is true if at least one player has no money and
    false otherwise. *)
let rec one_no_money players =
  match players with 
  | [] -> false
  | h :: t -> if money h = 0 then true else one_no_money t

(** [everyone_gets_a_turn players st] performs a betting round that will
    terminate if no one decides to raise a bet, and will go to
    [continued_betting players st] if someone does. *)
let rec everyone_gets_a_turn players st =
  if all_no_money players || only_one_player st then st 
  (* else if one_no_money players then all_in_mode players st *)
  else
    begin
      match players with
      | [] -> st
      | h::t -> 
        if money h = 0 then everyone_gets_a_turn t st 
        else if one_no_money players then st
        else if is_ai h 
        then 
          begin
            let act = execute "call" h st in 
            match act with
            | (st', raised) -> 
              if raised then continued_betting (st' |> active_players |> place_last h) st' 
              else betting_aux true t st'
          end
        else 
          begin
            print_endline ("\n\n"^(name h)^", press enter when you are alone");
            print_string "> ";
            match read_line () with 
            | _ -> 
              show_info h st; 
              let need_to_stay_in =  current_bet st - money_betted h in
              print_endline ("You must bet at least $" ^ string_of_int (current_bet st - money_betted h) ^ " to stay in the game.");
              if need_to_stay_in = 0 
              then
                print_endline ("Your options are 'fold', 'check', 'call', 'allin', 'raise x',\nwhere x is a positive integer.")
              else
                print_endline ("Your options are 'fold', 'call', 'allin', 'raise x', \nwhere x is a positive integer.\n");
              print_endline "What would you like to do?";         
              print_string "> ";
              match read_line () with
              | str -> ANSITerminal.erase Screen; 
                let act = execute str h st in 
                match act with
                | (st', raised) -> 
                  if raised then continued_betting (st' |> active_players |> place_last h) st' 
                  else betting_aux true t st'
          end
    end

(** [continued_betting players st] performs as many rounds of betting as
    necessary until all players have called or folded on the current bet. *)
and continued_betting players st = 
  if all_no_money players || only_one_player st then st
  else if one_no_money players then st
  else
    begin
      match players with
      | [] -> failwith "betting without players"
      | h :: [] -> st
      | h::t -> if money h = 0 then continued_betting t st else 
          begin
            if is_ai h 
            then 
              begin
                let act = execute "call" h st in 
                match act with
                | (st', raised) -> 
                  if raised then continued_betting (st' |> active_players |> place_last h) st' 
                  else betting_aux false t st'
              end
            else 
              begin
                print_endline ("\n\n"^(name h)^", press enter when you are alone");
                print_string "> ";
                match read_line () with 
                | _ -> 
                  show_info h st;
                  let need_to_stay_in =  current_bet st - money_betted h in
                  print_endline ("You must bet at least $" ^ string_of_int (current_bet st - money_betted h) ^ " to stay in the game.");
                  if need_to_stay_in = 0 
                  then
                    print_endline ("Your options are 'fold', 'check', 'call', 'allin', 'raise x', \n where x is a positive integer.")
                  else
                    print_endline ("Your options are 'fold', 'call', 'allin', 'raise x', \n where x is a positive integer.");
                  print_endline "What would you like to do?";
                  print_string "> ";
                  begin 
                    match read_line () with
                    | str -> ANSITerminal.erase Screen; 
                      let act = execute str h st in 
                      match act with
                      | (st', raised) -> 
                        if raised then continued_betting (st' |> active_players |> place_last h) st' 
                        else betting_aux false t st'
                  end
              end
          end

    end

(** [betting_aux players] prints the private information of each players to 
    the terminal, clearing the terminal between each player*)
and betting_aux b players st =
  if b then everyone_gets_a_turn players st else continued_betting players st

(** [first_round_betting st] sets up the correct player list rotation and
    proceeds with the first round of betting. *)
let first_round_betting st =
  if only_one_player st then st
  else betting_aux true (st |> active_players |> rotate_first_round) st

(** [betting st] prints the private information of each players to 
    the terminal, clearing the terminal between each player*)
let rec betting st =
  if only_one_player st then st
  else betting_aux true (st |> active_players) st

(** [flop st] draws 3 cards and places them on the table. It prints out the
    table information to all players. *)
let flop st = 
  if only_one_player st then st
  else
    let d = draw_card 3 (rem_deck st) in
    let st' = change_table (change_rem_deck (snd d) st) (fst d) in 
    let st'' = change_active_players st' (List.map (fun p -> reset_last_move p) (active_players st')) in
    show_flop st''; st''

(** [turn st] draws 1 card and places it on the table. It prints out the
    table information to all players. *)
let turn st = 
  if only_one_player st then st
  else
    let d = draw_card 1 (rem_deck st) in
    let st' = change_table (change_rem_deck (snd d) st) (fst d) in 
    let st'' = change_active_players st' (List.map (fun p -> reset_last_move p) (active_players st')) in
    show_turn st''; st''

(** [river st] draws the final card and places it on the table. It prints out 
    the table information to all players. *)
let river st = 
  if only_one_player st then st
  else
    let d = draw_card 1 (rem_deck st) in
    let st' = change_table (change_rem_deck (snd d) st) (fst d) in
    let st'' = change_active_players st' (List.map (fun p -> reset_last_move p) (active_players st')) in 
    show_river st''; st''

(** [show_down_aux st players high acc] is a list of players which have the
    best hand taking into account cards in the hole and on the table. *)
let rec show_down_aux st players high acc =
  match players with 
  | [] -> List.rev acc
  | h :: t -> let h_value = add (h |> hand) (st |> table) |> hand_value in 
    if h_value > high then show_down_aux st t h_value ([h])
    else if h_value = high then show_down_aux st t high (h :: acc)
    else show_down_aux st t high (acc)

(** names_to_string more_than_one players acc] prints out the names of 
    [players]. *)
let rec names_to_string more_than_one players acc =
  match players with 
  | [] -> acc
  | h1 :: h2 :: [] -> names_to_string more_than_one (h2 :: []) (acc ^ name h1 ^ " ")
  | h1 :: h2 :: t -> names_to_string more_than_one (h2 :: t) (acc ^ name h1 ^ ", ")
  | h :: [] -> 
    begin
      if more_than_one 
      then names_to_string more_than_one ([]) (acc ^ "and " ^ name h) 
      else names_to_string more_than_one ([]) (name h)
    end
(* | h :: t -> names_to_string t (acc ^ "," ^ name h) *)

(** [split_pot st aps winners won_money acc] is a list of [winners] to reflect
    the earnings of [winners] and their newly_won [won_money]. *)
let rec split_pot st aps winners won_money acc =
  match aps with
  | [] -> List.rev acc
  | h :: t -> if List.mem (name h) (List.map (fun p -> name p) winners) 
    then split_pot st t winners won_money ((change_money h won_money) :: acc)
    else split_pot st t winners won_money (h :: acc)

(** [show_win_info st winners won_money aps] is an updated [st] which reflects
    the winning status of [winners]. *)
let rec show_win_info st winners won_money aps=
  print_endline ((names_to_string (List.length winners > 1) winners "") ^ " won this round of poker!"); 
  change_active_players st (split_pot st (st |> active_players) winners won_money [])

(** [show_down st] is the state after comparing all the players' hands to determine the winner(s)*)
let show_down st =
  if only_one_player st
  then show_win_info st (active_players st) (betting_pool st) (active_players st) 
  else
    begin 
      let winners = show_down_aux st (active_players st) 0 [] in
      show_win_info st winners (betting_pool st / List.length winners) 
        (active_players st)
    end

(** [player_continuing str] is true if [str] parses to continue and false if [str] parses to quit*)
let rec player_continuing str =
  match parse str with
  | Quit -> false
  | Continue -> true
  | exception Empty -> 
    begin
      print_endline "You cannot enter an empty command. Please enter quit or continue!";
      print_string "> ";
      match read_line () with
      | str -> player_continuing str
    end
  | exception Malformed -> 
    begin
      print_endline "We did not recognize that. Please enter quit or continue!";
      print_string "> ";
      match read_line () with
      | str -> player_continuing str
    end
  | _ -> 
    begin
      print_endline "Please either quit or continue";
      print_string "> ";
      match read_line () with
      | str -> player_continuing str
    end

(** [quit_or_cont players acc] is the players left at the end of the game who have not quit and have enough money to continue the game*)
let rec quit_or_cont players acc =
  match players with 
  | [] -> List.rev acc
  | h :: t -> 
    let m = money h in
    print_endline ("\n" ^ name h ^ ", you now have $" ^ (string_of_int m));
    if m <= 0 
    then (print_endline "You are out of money and are now invited to leave the table."; 
          quit_or_cont t acc)
    else if m < 50 
    then (print_endline "You do not have the $50 necessary to continue playing at this table. You now are invited to leave."; 
          quit_or_cont t acc)
    else (
      if is_ai h then quit_or_cont t (h :: acc) 
      else (
        print_endline "Would you like to quit or continue?";
        print_string "> ";
        match read_line () with
        | str -> if player_continuing str then quit_or_cont t (h :: acc)
          else quit_or_cont t acc ))

(**[game_over players winner] displays the final win message to [winner] and losing messages to the losing players*)
let rec game_over players winner =
  match players with
  | [] -> print_endline ("\n\n\n\n" ^ winner ^ ", congratulations! YOU WON THE GAME!!");
  |  h :: t -> 
    begin
      let m = money h in
      if m <= 0 
      then 
        begin
          print_endline ("\n\n" ^ name h ^ ", you are out of money and are now invited to leave the table.");
          game_over t winner
        end
      else if m < 50
      then 
        begin
          print_endline ("\n\n" ^ name h ^ ", you only have " ^ string_of_int (money h) ^". You do not have the $50 necessary to continue playing at this table. You now are invited to leave.");
          game_over t winner
        end
      else 
        begin
          game_over t (name h)
        end
    end

(* let rec all_ps_begin_equal_all_ps_end all_ps act_ps =
   match (all_ps, act_ps) with 
   | [], [] -> print_endline "WOO all players in beginning and all players at the end are same length and in same order in the beginning"
   | _, [] -> failwith "more all players in beginnning than all players in the end"
   | [], _ -> failwith "more all players in end than all players in the beginning"
   | h1 :: t1, h2 :: t2 -> if name h1 = name h2 then all_ps_begin_equal_all_ps_end t1 t2
    else failwith "all players in beginning and all players in end are not in the same order" *)

(** [all_but_one_cant_play players acc] is true if only one player can continue playing in the game*)
let rec all_but_one_cant_play players acc =
  match players with
  | [] -> acc = 1
  | h :: t -> 
    begin
      if money h >= 50 then all_but_one_cant_play t (acc + 1)
      else all_but_one_cant_play t acc
    end

(** [end_game players st] is [st] updated to reflect the end of the game*)
let end_game st =
  let st' = remove_all_active_players st (active_players st) in
  change_all_players st' (st' |> all_players) 
(* |> rotate_game *)

(* in
   quit_or_cont (all_players st'') [] *)

(* let rec all_ps_equal_act_ps all_ps act_ps =
   match (all_ps, act_ps) with 
   | [], [] -> print_endline "WOO active players and all players same length and in same order in the beginning"
   | _, [] -> failwith "more all players than active players in the begining"
   | [], _ -> failwith "more active players than all players in the begining"
   | h1 :: t1, h2 :: t2 -> if name h1 = name h2 then all_ps_equal_act_ps t1 t2
    else failwith "all players and active players are not in the same order" *)

(** [add_ai st] is an updated [st] with an AI player in the last position of
    both player lists. *)
let add_ai st =
  let st' = change_active_players st (active_players st @ [create_ai_player]) in
  change_all_players st' (all_players st' @ [create_ai_player])

(** [next_game players] is the state of the next game with [players] in the game *)
let rec next_game players = 
  if List.length players <= 1 then ()
  else players |> rotate |> new_round |> start_game

(** [start_game st] plays a game starting in [st]. *)
and start_game st =
  let st' = if List.length (active_players st) = 1 then add_ai st else st in
  let st'' = st' |> set_blinds |> take_blind_money in
  show_blind_info (active_players st''); 
  let st_at_end = st'' 
                  |> first_round_betting 
                  |> flop 
                  |> betting
                  |> turn 
                  |> betting 
                  |> river 
                  |> betting
                  |> show_down  
                  |> end_game
  in 
  if all_but_one_cant_play (all_players st_at_end) 0
  then game_over (all_players st_at_end) ""
  else 
    begin
      quit_or_cont (all_players st_at_end) [] 
      |> next_game
    end

(** [get_players] asks for the number of players and their names, and then 
    starts a game with that many human players. *)
let rec get_players () = 
  print_endline "\nPlease enter the number of players in your game.";
  print_endline "Note there may only be up to 10 players in your game";
  print_string  "> ";
  match int_of_string(read_line ()) with
  | exception End_of_file -> ()
  | exception e -> print_endline "You must enter a number."; get_players ()
  | x -> 
    if (x > 10 || x < 1) 
    then (print_endline "You must enter a number between 1 and 10."; 
      get_players () )
    else start_game (create_player_list x [] |> new_round)

(** [main ()] clears the screen and prompts for the game to play, then begins 
    to collect information over the players. *)
let main () =
  ANSITerminal.erase Screen;
  (print_string "\n\nWelcome to Texas Hold 'Em!"); get_players () 

let () = main ()


