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
        | "" ->
          begin 
            print_endline "You cannot have an empty name. Please try again.";
            create_player_list x acc
          end
        | n -> 
          if List.mem n ((List.map (fun p -> name p)) acc) then 
            begin
              print_endline "You cannot choose a name that someone has already selected. Please try again";
              create_player_list x acc
            end
          else create_player_list (x-1) (create_player n::acc))

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
  | Small -> print_endline ("\n\n"^name p ^ ", you are small blind, so you automatically bet 25.
You now only have $" ^ string_of_int (money p) ^ " left.")
  | Big -> print_endline ("\n\n"^name p ^ ", you are big blind, so you automatically bet 50.
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
  print_endline ("You have $" ^ (string_of_int money) ^ " and your cards are\n" ^ hand);
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
  | Continue -> 
    begin
      print_endline "Please enter a betting action.";
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
          print_endline "You cannot bet more than the wallet of the poorest player. Please try again!";
          print_string "> ";
          match read_line () with
          | str -> execute str p st
        end
      | st' -> (st', false)
    end
  (* begin
     print_endline "Allin is currently in development. Please try another betting command!";
     print_string "> ";
     match read_line () with
     | str -> execute str p st
     end *)
  | Raise i -> 
    begin
      match raise i st p with 
      | exception InvalidBet -> 
        begin
          print_endline "You cannot bet more than the wallet of the poorest player. Please try again!";
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
  if only_one_player st then st 
  else
    begin
      match players with
      | [] -> st
      | h::t -> print_endline ("\n\n"^(name h)^", press enter when you are alone");
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
          begin 
            match read_line () with
            | str -> ANSITerminal.erase Screen; 
              let act = execute str h st in 
              match act with
              | (st', b) -> 
                if b then continued_betting (st' |> active_players |> cb_lst h) st' 
                else betting_aux true t st'
          end
    end

and continued_betting players st = 
  if only_one_player st then st 
  else
    begin
      match players with
      | [] -> failwith "betting without players"
      | h :: [] -> st
      | h::t -> print_endline ("\n\n"^(name h)^", press enter when you are alone");
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
              | (st', b) -> 
                if b then continued_betting (st' |> active_players |> cb_lst h) st' 
                else betting_aux false t st'
          end
    end

(** [betting_aux players] prints the private information of each players to 
    the terminal, clearing the terminal between each player*)
and betting_aux b players st =
  if b then everyone_gets_a_turn players st else continued_betting players st

let first_round_betting st =
  if only_one_player st then st 
  else betting_aux true (st |> active_players |> rotate_first_round) st

(** [betting st] prints the private information of each players to 
    the terminal, clearing the terminal between each player*)
let rec betting st =
  if only_one_player st then st
  else betting_aux true (st |> active_players) st

let flop st = 
  if only_one_player st then st
  else
    let d = draw_card 3 (rem_deck st) in
    let st' = change_table (change_rem_deck (snd d) st) (fst d) in 
    show_flop st'; st'

let turn st = 
  if only_one_player st then st
  else
    let d = draw_card 1 (rem_deck st) in
    let st' = change_table (change_rem_deck (snd d) st) (fst d) in 
    show_turn st'; st'

let river st = 
  if only_one_player st then st
  else
    let d = draw_card 1 (rem_deck st) in
    let st' = change_table (change_rem_deck (snd d) st) (fst d) in 
    show_river st'; st'
let finish st =
  print_endline "That's all for the demo game folks!";
  st

let rec show_down_aux st players high acc =
  match players with 
  | [] -> List.rev acc
  | h :: t -> let h_value = add (h |> hand) (st |> table) |> hand_value in 
    if h_value > high then show_down_aux st t h_value ([h])
    else if h_value = high then show_down_aux st t high (h :: acc)
    else show_down_aux st t high (acc)

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

let rec split_pot st aps winners won_money acc =
  match aps with
  | [] -> List.rev acc
  | h :: t -> if List.mem (name h) (List.map (fun p -> name p) winners) 
    then split_pot st t winners won_money ((change_money h won_money) :: acc)
    else split_pot st t winners won_money (h :: acc)

let rec show_win_info st winners won_money aps=
  print_endline ((names_to_string (List.length winners > 1) winners "") ^ " won the game!"); 
  change_active_players st (split_pot st (st |> active_players) winners won_money [])

let show_down st =
  if only_one_player st
  then show_win_info st (active_players st) (betting_pool st) (active_players st) 
  else
    begin 
      let winners = show_down_aux st (active_players st) 0 [] in
      show_win_info st winners (betting_pool st / List.length winners) 
        (active_players st)
    end

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

let rec display_money players acc =
  match players with 
  | [] -> List.rev acc
  | h :: t -> print_endline ("\n" ^ name h ^ ", you now have $" ^ (string_of_int (money h)));
    print_endline "Would you like to continue the game or quit?"; 
    match read_line () with
    | str -> if player_continuing str then display_money t (h :: acc)
      else display_money t acc

let rec all_ps_begin_equal_all_ps_end all_ps act_ps =
  match (all_ps, act_ps) with 
  | [], [] -> print_endline "WOO all players in beginning and all players at the end are same length and in same order in the beginning"
  | _, [] -> failwith "more all players in beginnning than all players in the end"
  | [], _ -> failwith "more all players in end than all players in the beginning"
  | h1 :: t1, h2 :: t2 -> if name h1 = name h2 then all_ps_begin_equal_all_ps_end t1 t2
    else failwith "all players in beginning and all players in end are not in the same order"

let end_game st =
  let st' = remove_all_active_players st (active_players st) in
  let st'' = change_all_players st' (st' |> all_players 
  (* |> rotate_game *)
                                    ) 
  in
  display_money (all_players st'') []

(* let rec all_ps_equal_act_ps all_ps act_ps =
   match (all_ps, act_ps) with 
   | [], [] -> print_endline "WOO active players and all players same length and in same order in the beginning"
   | _, [] -> failwith "more all players than active players in the begining"
   | [], _ -> failwith "more active players than all players in the begining"
   | h1 :: t1, h2 :: t2 -> if name h1 = name h2 then all_ps_equal_act_ps t1 t2
    else failwith "all players and active players are not in the same order" *)



let rec next_game players = 
  if List.length players <= 1 then ()
  else players |> rotate_game |> new_round |> start_game

(** [start_game st] plays a game starting in [st]. *)
and start_game st =
  let st' = st |> set_blinds |> take_blind_money in
  show_blind_info st' (active_players st'); 
  st' 
  |> first_round_betting 
  |> flop 
  |> betting
  |> turn 
  |> betting 
  |> river 
  |> betting
  |> show_down  
  |> end_game
  |> next_game

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.erase Screen;
  (print_string
     "\n\nWelcome to Texas Hold 'Em.\n");
  print_endline "Please enter the number of players in your game.\nNote there may only be 2-10 players in your game";
  print_string  "> ";
  match int_of_string(read_line ()) with
  | exception End_of_file -> ()
  | exception e -> print_endline "You must enter a number.\nStart the game engine again to play"
  | x -> if (x > 10 || x <2 ) then print_endline "You must enter a number between 2 and 10. \n Start the game engine again to play";
    start_game (create_player_list x [] |> new_round)

let () = main ()


