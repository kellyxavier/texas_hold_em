open Deck
open Player
open State
open Command
open Hands
open Rank_hand
open Ai

(** [wait ()] pauses the game and returns when a player hits enter. *)
let wait () = 
  print_endline "\nPress enter when everyone is ready to continue.";
  print_string "> ";
  match read_line () with
  | _ -> ()

(** [get_valid_name lst i] is a name prompted by the [i]th user that is neither 
    empty nor already in [lst]. *)
let rec get_valid_name lst i =
  print_endline ("Player " ^ (string_of_int i) ^
                 ": enter your name\nNote: your name must be unique.");
  print_string "> ";
  match String.trim (read_line ()) with
  | "" ->
    begin 
      print_endline "You cannot have an empty name. Please try again.";
      get_valid_name lst i
    end
  | n -> 
    if List.mem n lst then 
      begin
        print_endline 
          ("You cannot choose a name that someone has already selected. " ^
           "Please try again.");
        get_valid_name lst i
      end
    else if is_ai_name n then
      begin
        print_endline 
          ("You cannot choose a name that is reserved for the AI. " ^ 
           "Please try again.");
        get_valid_name lst i
      end
    else n

(** [create_player_list x names acc] is the player list built off of [acc] 
    consisting of [x] players with names listed in [names]. *)
let rec create_player_list x names acc =
  if x = 0 then List.rev acc
  else 
    let n = get_valid_name names (List.length acc + 1) in
    create_player_list (x - 1) (n :: names) ((create_player n) :: acc) 

(** [rotate lst] is [lst] rotated left once. *)
let rotate lst =
  match lst with
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

(** [remove_blinds_aux lst acc] is [lst] without any blinds on the players. *)
let rec remove_blinds_aux lst acc =
  match lst with 
  | h :: t -> remove_blinds_aux t (change_blind h None :: acc)
  | _ -> List.rev acc

(** [remove_blinds lst] is [lst] with all blinds removed from players. *)
let rec remove_blinds lst =
  remove_blinds_aux lst []

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
  | Small -> 
    begin
      print_endline ("\n" ^ name p ^ 
                     ", you are small blind, so you automatically bet $25.");
      print_endline ("You now have $" ^ string_of_int (money p) ^ " left.")
    end
  | Big -> 
    begin
      print_endline ("\n" ^ name p ^ 
                     ", you are big blind, so you automatically bet $50.");
      print_endline ("You now have $" ^ string_of_int (money p) ^ " left.")
    end
  | None -> print_string ""

(** [show_blind_info players] prints out the blind information of each
    player in [players]. *)
let rec show_blind_info players=
  match players with 
  | [] -> ()
  | h :: t -> display_blind_money h; show_blind_info t 

(** [blinds st] is the state at which blinds are set in [st] and 
    betting is about to commence. *)
let blinds st =
  let st' = st |> set_blinds |> take_blind_money in
  show_blind_info (active_players st');
  wait ();
  st'

(** [print_prev_moves players] prints out the last moves of eacher player in
    [players]. *)
let rec print_prev_moves players =
  match players with
  | [] -> ()
  | h :: t -> print_endline (name h ^ ": " ^ move_to_string (last_move h));
    print_prev_moves t

(** [show_info p st] prints the private information of player [p]*)
let show_info p st = 
  let info_p = get_info st p in 
  let wallet = info_p.wallet |> string_of_int in 
  let t = info_p.t_cards |> to_string in 
  let h = info_p.h_cards |> to_string in
  let bp = info_p.b_pool |> string_of_int in
  print_endline
    ("There is currently $" ^ bp ^ " in the betting pool.");
  print_endline
    ("\nThese are the last moves of each player currently at the table:");
  print_prev_moves (active_players st);
  print_endline
    ("\nYou have $" ^ wallet ^ " and your cards are:\n" ^ h);
  if t = "" then print_endline "There are no cards currently on the table."
  else print_endline ("The table currently has the cards: \n" ^ t);
  print_endline ("\nYou must bet at least $" ^ 
                 string_of_int (current_bet st - money_betted p) ^
                 " to stay in the game.\n")

(** [show_flop st] reveals the table with the newly-added flop to all 
    players. *)
let show_flop st =
  if one_no_money (active_players st) then ()
  else
    begin
      ANSITerminal.erase Above;
      print_endline "We will now reveal the flop.";
      print_endline ("The table currently has the cards: \n" ^ 
                     (st |> table |> to_string));
      wait ()
    end

(** [show_river st] reveals the table with the newly-added river to all 
    players. *)
let show_river st =
  if one_no_money (active_players st) then ()
  else
    begin
      ANSITerminal.erase Above;
      print_endline "We will now reveal the river.";
      print_endline ("The table currently has the cards: \n" ^ 
                     (st |> table |> to_string));
      wait ()
    end

(** [show_turn st] reveals the table with the newly-added turn to all 
    players. *)
let show_turn st =
  if one_no_money (active_players st) then ()
  else
    begin
      ANSITerminal.erase Above;
      print_endline "We will now reveal the turn.";
      print_endline ("The table currently has the cards: \n" ^ 
                     (st |> table |> to_string));
      wait ()
    end

(** [execute_again p st] prompts [p] to take a betting decision and then
    executes the command again in [st]. *)
let rec execute_again p st = 
  print_string "> ";
  match read_line () with
  | str -> execute str p st

(** [execute str p st] inteprets [p]'s [str] in the context of a regular betting
    round and returns a pair of an updated [st] which reflects the changes made
    and whether it was a raise or not. *)
and execute str p st =
  match parse str with
  | Quit -> print_endline quit_error; execute_again p st
  | Continue -> print_endline continue_error; execute_again p st
  | Fold -> (fold st p, false)
  | Check -> 
    begin
      match check st p with 
      | st' -> (st', false)
      | exception InvalidBet -> print_endline check_error; execute_again p st
    end
  | Call -> (call st p, false)
  | Allin -> 
    begin
      match all_in st p with 
      | st' -> (st', true)
      | exception InvalidBet -> print_endline raise_error; execute_again p st
    end
  | Raise i -> 
    begin
      match raise i st p with 
      | st' -> (st', true)
      | exception InvalidBet -> print_endline raise_error; execute_again p st
    end
  | exception Empty -> print_endline empty_error; execute_again p st
  | exception Malformed -> print_endline malformed_error; execute_again p st
  | Default -> failwith "player entered command should never parse to Default"

(** [everyone_gets_a_turn players st] performs a betting round that will
    terminate if no one decides to raise a bet, and will go to
    [continued_betting players st] if someone does. *)
let rec everyone_gets_a_turn players st =
  if all_no_money players || only_one_player st then st 
  else
    begin
      match players with
      | [] -> st
      | h::t -> 
        ANSITerminal.erase Above;
        if money h = 0 then everyone_gets_a_turn t st 
        else if one_no_money players then st
        else if is_ai h 
        then ai_turn st h t true
        else human_turn st h t true
    end

(** [continued_betting players st] performs as many rounds of betting as
    necessary until all players have called or folded on the current bet. *)
and continued_betting players st = 
  if all_no_money players || only_one_player st then st
  else
    begin
      match players with
      | [] -> failwith "betting without players"
      | h :: [] -> st
      | h::t -> 
        begin
          ANSITerminal.erase Above;
          if money h = 0 then continued_betting t st  
          else if is_ai h then ai_turn st h t false
          else human_turn st h t false
        end
    end

(** [ai_turn st ai rest_of_ps] performs the turn for [ai] and then continues 
    with the betting round for [rest_of_ps]. *)
and ai_turn st ai rest_of_ps b = 
  let act = 
    if name ai = "Easy AI" then 
      execute (make_easy_move st (get_info st ai)) ai st 
    else if name ai = "Hard AI" then
      execute (make_hard_move st (get_info st ai)) ai st
    else execute (make_med_move st (get_info st ai)) ai st in 
  match act with
  | (st', raised) ->
    begin 
      if raised 
      then continued_betting (st' |> active_players |> place_last ai) st' 
      else betting_aux b rest_of_ps st'
    end

(** [human_turn st human rest_of_ps] prints out info to the human player and 
    is the state after it performs the turn for [human] and then continues 
    with the betting round for [rest_of_ps]. *)
and human_turn st human rest_of_ps b =
  print_endline ("\n\n"^(name human)^", press enter when you are alone");
  print_string "> ";
  match read_line () with 
  | _ -> 
    begin
      show_info human st; 
      if current_bet st - money_betted human = 0 
      then
        print_endline ("Your options are 'fold', 'check', 'call', 'allin', " ^
                       "'raise x',\nwhere x is a positive integer.")
      else
        print_endline ("Your options are 'fold', 'call', 'allin', 'raise x', " ^ 
                       "\nwhere x is a positive integer.\n");
      print_endline "What would you like to do?";         
      print_string "> ";
      execute_human_turn st human rest_of_ps b
    end

(** [human_turn st act str human rest_of_ps b] is the state after it performs 
    the turn for [human] and then continues with the betting round for 
    [rest_of_ps]. *)
and execute_human_turn st human rest_of_ps b =
  match read_line () with
  | str -> 
    let act = execute str human st in 
    begin
      match act with
      | (st', raised) -> 
        if raised then continued_betting (st' |> active_players |> 
                                          place_last human) st' 
        else betting_aux b rest_of_ps st'
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
  if only_one_player st || one_no_money (active_players st) then st
  else betting_aux true (st |> active_players) st

(** [flop st] draws 3 cards and places them on the table. It prints out the
    table information to all players. *)
let flop st = 
  if only_one_player st then st
  else
    begin
      let d = draw_card 3 (rem_deck st) in
      let st' = change_table (change_rem_deck (snd d) st) (fst d) in 
      let st'' = change_active_players st' (List.map (fun p -> reset_last_move p) 
                                              (active_players st')) in
      show_flop st''; st''
    end

(** [turn st] draws 1 card and places it on the table. It prints out the
    table information to all players. *)
let turn st = 
  if only_one_player st then st
  else
    begin
      let d = draw_card 1 (rem_deck st) in
      let st' = change_table (change_rem_deck (snd d) st) (fst d) in 
      let st'' = change_active_players st' 
          (List.map (fun p -> reset_last_move p) (active_players st')) in
      show_turn st''; st''
    end

(** [river st] draws the final card and places it on the table. It prints out 
    the table information to all players. *)
let river st = 
  if only_one_player st then st
  else
    begin
      let d = draw_card 1 (rem_deck st) in
      let st' = change_table (change_rem_deck (snd d) st) (fst d) in
      let st'' = change_active_players st' 
          (List.map (fun p -> reset_last_move p) (active_players st')) in 
      show_river st''; st''
    end

(** [show_down_aux st players high acc] is a list of players which have the
    best hand taking into account cards in the hole and on the table. *)
let rec show_down_aux st players high acc =
  match players with 
  | [] -> List.rev acc
  | h :: t -> 
    begin 
      let h_value = add (h |> hand) (st |> table) |> hand_value in 
      if h_value > high then show_down_aux st t h_value ([h])
      else if h_value = high then show_down_aux st t high (h :: acc)
      else show_down_aux st t high (acc)
    end

(** [names_to_string more_than_one players acc] prints out the names of 
    [players]. *)
let rec names_to_string more_than_one players acc =
  match players with 
  | [] -> acc
  | h1 :: h2 :: [] -> names_to_string more_than_one (h2 :: []) (acc ^ 
                                                                name h1 ^ " ")
  | h1 :: h2 :: t -> names_to_string more_than_one (h2 :: t) (acc 
                                                              ^ name h1 ^ ", ")
  | h :: [] -> 
    begin
      if more_than_one 
      then names_to_string more_than_one ([]) (acc ^ "and " ^ name h) 
      else names_to_string more_than_one ([]) (name h)
    end

(** [split_pot st aps winners won_money acc] is a list of [winners] to reflect
    the earnings of [winners] and their newly_won [won_money]. *)
let rec split_pot st aps winners won_money acc =
  match aps with
  | [] -> List.rev acc
  | h :: t -> if List.mem (name h) (List.map (fun p -> name p) winners) 
    then split_pot st t winners won_money ((change_money h won_money) :: acc)
    else split_pot st t winners won_money (h :: acc)

(** [print_cards lst acc] prints the cards of all players in [lst]. *)
let rec print_cards lst acc =
  match lst with
  | [] -> acc
  | h :: t -> print_cards t (acc ^ "\n" ^ name h ^ " had the following cards:\n"
                             ^ (h |> hand |> to_string))

(** [net_winnings winners money] is the net amount of [money] won be each 
    [winner] since the beginning of the current game. *)
let net_winnings winners money =
  match winners with
  | [] -> failwith "no winner"
  | h :: _ -> money - (money_betted h)

(** [folded_players st] is a player list consisting only of the players who 
    folded in [st] *)
let folded_players st =
  let act_ps_names = List.map (fun p -> name p) (active_players st) in
  List.filter (fun p -> not (List.mem (name p) act_ps_names)) (all_players st)

(** [show_win_info st winners won_money_total won_money_net aps] is an updated 
    [st] which reflects the winning status of [winners]. *)
let rec show_win_info st winners won_money aps =
  let f_players = folded_players st in
  let f_names = names_to_string (List.length f_players > 1) f_players "" in
  let net = net_winnings winners won_money in
  if List.length f_players <> 0 then print_endline (f_names ^ " folded in " ^ 
                                                    "this round of poker.");
  print_endline ((names_to_string (List.length winners > 1) winners "") ^ 
                 " won $" ^ string_of_int net ^ " in this round of poker!"); 
  change_active_players st (split_pot st (st |> active_players) winners 
                              won_money [])

(** [show_down st] is the state after comparing all the players' hands to 
    determine the winner(s)*)
let show_down st =
  ANSITerminal.erase Above;
  if only_one_player st
  then show_win_info st (active_players st) (betting_pool st) (active_players 
                                                                 st) 
  else
    begin 
      let winners = show_down_aux st (active_players st) 0 [] in
      print_endline ("These were the final cards on the table:\n" ^ 
                     (st |> table |> to_string));
      print_endline (print_cards (active_players st) "");
      show_win_info st winners (betting_pool st / List.length winners) 
        (active_players st)
    end

(** [player_continuing str] is true if [str] parses to continue and false if 
    [str] parses to quit*)
let rec player_continuing str =
  match parse str with
  | Quit -> false
  | Continue -> true
  | exception Empty -> 
    begin
      print_endline ("You cannot enter an empty command. " ^ 
                     "\nPlease enter quit or continue!");
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

(** [quit_or_cont_aux p rest_of_ps acc] is the players left at the end of the 
    game who have not quit and have enough money to continue the game when the 
    current player [p] has enough money to continue playing*)
let rec quit_or_cont_aux p rest_of_ps acc =
  if is_ai p then quit_or_cont rest_of_ps (p :: acc) 
  else (
    print_endline "Would you like to quit or continue?";
    print_string "> ";
    match read_line () with
    | str -> if player_continuing str then quit_or_cont rest_of_ps (p :: acc)
      else quit_or_cont rest_of_ps acc )

(** [quit_or_cont players acc] is the players left at the end of the game who 
    have not quit and have enough money to continue the game*)
and quit_or_cont players acc =
  match players with 
  | [] -> List.rev acc
  | h :: t -> 
    let m = money h in
    print_endline ("\n" ^ name h ^ ", you now have $" ^ (string_of_int m));
    if m <= 0 
    then (print_endline ("You are out of money and are now invited to leave " ^ 
                         "the table."); 
          quit_or_cont t acc)
    else if m < 50 
    then (print_endline ("You do not have the $50 necessary to continue playing"
                         ^ " at this table. You now are invited to leave."); 
          quit_or_cont t acc)
    else quit_or_cont_aux h t acc

(**[game_over players winner] displays the lose bankrupt message to [p] and 
   appropriate end messages to [rest of ps]*)
let rec game_over_broke p rest_of_ps winner =
  print_endline ("\n\n" ^ name p ^ ", you are out of money " ^ 
                 "and are now invited to leave the table.");
  game_over rest_of_ps winner

(**[game_over players winner] displays the lose not enough money message to [p]
   and appropriate end messages to [rest of ps]*)
and game_over_not_enough p rest_of_ps winner =
  print_endline ("\n\n" ^ name p ^ ", you only have " ^ 
                 string_of_int (money p) ^ ". You do not have the " ^ 
                 "$50 necessary to continue " ^ "playing at this table." 
                 ^ " You now are invited to leave.");
  game_over rest_of_ps winner

(**[game_over players winner] displays the final win message to [winner] and 
   losing messages to the losing players*)
and game_over players winner =
  match players with
  | [] -> print_endline ("\n\n\n\n" ^ winner ^ 
                         ", congratulations! YOU WON THE GAME!!");
  |  h :: t -> 
    begin
      let m = money h in
      if m <= 0 then game_over_broke h t winner
      else if m < 50 then game_over_not_enough h t winner
      else game_over t (name h)
    end

(** [all_but_one_cant_play players acc] is true if only one player can continue 
    playing in the game*)
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

(** [add_ai st diff] is an updated [st] with an AI player in the last position 
    of both player lists and difficulty set to [diff].
    Requires: [diff] must match either "Easy AI" or "Hard AI" or "Medium AI" *)
let add_ai st diff =
  create_ai_player diff :: [] |> List.append (all_players st) |> new_round

(** [pick_diff st] asks the player for the level of AI difficulty desired,
    then returns a state with such an AI. *)
let rec pick_diff st =
  let only_p_name = names_to_string false (active_players st) "" in
  print_endline (only_p_name ^ ", what level difficulty AI do you want? " ^
                 "'easy', 'medium', or 'hard'?");
  print_string "> ";
  match read_line () with
  | str -> 
    match diff str with
    | exception Empty -> 
      pick_diff st
    | exception Malformed ->
      begin
        print_endline "Sorry, we didn't understand that.";
        pick_diff st
      end
    | _ -> add_ai st (diff str)

(** [next_game players] is the state of the next game with [players] in the 
    game *)
let rec next_game players = 
  if List.length players = 0 || only_ais players 
  then print_endline "Thanks for playing!"
  else players |> remove_blinds |> rotate |> new_round |> start_game

(** [start_game st] plays a game starting in [st]. *)
and start_game st =
  let st' = 
    if List.length (active_players st) = 1 
    then pick_diff st 
    else st in
  let st_at_end = st'
                  |> blinds
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
    else start_game (create_player_list x [] [] |> new_round)

(** [main ()] clears the screen and prompts for the game to play, then begins 
    to collect information over the players. *)
let main () =
  ANSITerminal.erase Above;
  print_endline "\n\nWelcome to Texas Hold 'Em!";
  print_endline ("House Rules: You cannot bet more than the wallet of the " ^ 
                 "poorest player.");
  print_endline ("Honor System: Please do not scroll up to see your opponents' "
                 ^ "private information.");
  print_endline "Make sure to play this game in full screen!";
  get_players () 

let () = 
  try main ()
  with e -> print_endline "You have ended the game. Thanks for playing!"; ()
