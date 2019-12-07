open OUnit2
open Deck
open Player
open State
open Command


let d = sorted_deck
let pair = fst (draw_card 2 d)
let p1 = create_player "kelly"
let p2 = change_hand p1 pair
let p3 = change_money p1 500
let p4 = change_money p1 (-500)
let p5 = change_status p1 AllIn
let p6 = change_status p1 Folded
let p7 = change_status p1 Out
let p8 = change_blind p1 Big
let p9 = change_blind p1 Small

let player_tests =
  [
    "player's name is set correctly " >:: (fun _ -> 
        assert_equal "kelly" (p1 |> name));
    "player's initial hand is empty " >:: (fun _ -> 
        assert_equal empty (p1 |> hand));
    "player's inital money is 5000 " >:: (fun _ -> 
        assert_equal 5000 (p1 |> money));
    "player's initial status is Active " >:: (fun _ -> 
        assert_equal Betting (p1 |> status));
    "player's initial blind is None " >:: (fun _ -> 
        assert_equal None (p1 |> blind));
    "player's updated hand is appropriate size " >:: (fun _ -> 
        assert_equal 2 (p2 |> hand |> to_list |> List.length));
    "player's updated hand is expected cards" >:: (fun _ -> 
        assert_equal [(Clubs, 2); (Clubs, 1)] (p2 |> hand |> to_list));
    "player's money can be increased " >:: (fun _ -> 
        assert_equal 5500 (p3 |> money));
    "player's money can be decreased " >:: (fun _ -> 
        assert_equal 4500 (p4 |> money));
    "player's status can be changed to AllIn " >:: (fun _ -> 
        assert_equal AllIn (p5 |> status));
    "player's status can be changed to Folded " >:: (fun _ -> 
        assert_equal Folded (p6 |> status));
    "player's status can be changed to Out " >:: (fun _ -> 
        assert_equal Out (p7 |> status));
    "player's blind can be changed to Big " >:: (fun _ -> 
        assert_equal Big (p8 |> blind));
    "player's blind can be changed to Little " >:: (fun _ -> 
        assert_equal Small (p9 |> blind));
  ]

let rec get_player i lst =
  match lst with 
  | [] -> failwith "empty list"
  | h :: t -> if i = 1 then h else get_player (i-1) t

let rec get_money p lst =
  match lst with
  | [] -> failwith "empty list"
  | h :: t -> if name h = name p then money h else get_money p t

let rec get_last_move p lst =
  match lst with
  | [] -> failwith "empty list"
  | h :: t -> if name h = name p then last_move h else get_last_move p t

let rec get_money_betted p lst =
  match lst with
  | [] -> failwith "empty list"
  | h :: t -> if name h = name p then money_betted h else get_money_betted p t

let rec player_names  acc lst=
  match lst with 
  | [] -> List.rev acc
  | h :: t -> player_names (name h :: acc) t

let player1 = create_player "valeria"
let player2 = create_player "david"
let player3 = create_player "bobby"
let player4 = create_player "frank"
let player5 = create_player "alice"
let player6 = create_player "carl"
let player7 = create_player "robin"
let player8 = create_player "franny"
let player9 = create_player "kathy"
let player10 = create_player "rachel"
let player11 = create_player "josh"
let invalid_player_list_1 = [player1; player2; player3; player4; player5; 
                             player6; player7; player8; player9; player10; 
                             player11]
let invalid_player_list_2 = []
let valid_player_list_1 = [player1; player2; player3; player4]
let valid_player_list_2 = [player1; player2; player3; player4; player5; 
                           player6; player7; player8; player9; player10]
let valid_player_list_3 = [player1; player2]
let st = new_round valid_player_list_1
let og_act_players = st |> active_players
let player_delete = List.nth (st |> active_players) 0
let players_left = [List.nth (st |> active_players) 1; 
                    List.nth (st |> active_players) 2;
                    List.nth (st |> active_players) 3]
let dk = empty |> insert Clubs 2 |> insert Diamonds 7 |> insert Diamonds 6
let p_i1 = create_player "A"
let p_i2 = create_player "B"
let p_i3 = create_player "C"
let p_lst1 = [p_i1; p_i2; p_i3]
let test_state1 = new_round p_lst1
let p1 = get_player 1 (active_players test_state1)
let p2 = get_player 2 (active_players test_state1)
let p3 = get_player 3 (active_players test_state1)
let test_state_cb_20 = let s' = change_current_bet test_state1 20 
  in change_betting_pool s' 20
let test_state_p2_quit = quit test_state1 p2
let test_state_p2_fold = fold test_state1 p2
let test_state_p1_check = check test_state1 p1
let test_state_p2_fold_on_check = fold test_state_p1_check p2
let test_state_p2_call = call test_state_cb_20 p2
let test_state_p2_allin = all_in test_state1 p2
let test_state_p1_raise_2000 = raise 2000 test_state1 p1
let test_state_p2_call_2000 = call test_state_p1_raise_2000 p2
let test_state_p3_allin = all_in test_state_p2_call_2000 p3
let test_state_p2_raise_1000 = raise 1000 test_state1 p2

let state_tests =
  [
    "new_round with input of player list with length strictly 
       greater than 10 should raise InvalidPlayerList" >::
    (fun _ -> 
       assert_raises (InvalidPlayerList) (fun () -> 
           new_round invalid_player_list_1));
    "new_round with input of empty player list should raise 
       InvalidPlayerList" >::
    (fun _ -> 
       assert_raises (InvalidPlayerList) (fun () -> 
           new_round invalid_player_list_2));
    "new round with input of list of 4 players results in state with 
    4 players" >:: (fun _ -> 
        assert_equal 4 (valid_player_list_1 |> new_round |> active_players 
                        |> List.length));
    "new round with input of list of 10 players results in state with 
    10 players (ie border case)" >:: (fun _ -> 
        assert_equal 10 (valid_player_list_2 |> new_round |> active_players 
                         |> List.length));
    "new round with input of list of 2 players results in state with 
    2 players (ie border case)" >:: (fun _ -> 
        assert_equal 2 (valid_player_list_3 |> new_round |> active_players 
                        |> List.length));
    "new round with input list of 4 players assigns each player in resulting 
    state 2 cards" >:: (fun _ -> 
        assert_equal 4 (valid_player_list_1 |> new_round |> active_players |> 
                        List.map (fun x -> to_list(hand x)) |> 
                        List.filter (fun x -> List.length x = 2) |> 
                        List.length));
    "new round with input list of 10 players assigns each player in resulting 
    state 2 cards (ie border case)" >:: (fun _ -> 
        assert_equal 10 (valid_player_list_2 |> new_round |> active_players |> 
                         List.map (fun x -> to_list(hand x)) |> 
                         List.filter (fun x -> List.length x = 2)|> 
                         List.length));
    "new round with input list of 2 players assigns each player in resulting 
    state 2 cards (ie border case)" >:: (fun _ -> 
        assert_equal 2 (valid_player_list_3 |> new_round |> active_players |> 
                        List.map (fun x -> to_list(hand x)) |> 
                        List.filter (fun x -> List.length x = 2)|> 
                        List.length));
    "new round with input list of 4 players assigns each player in resulting 
    state 2 unique cards" >:: (fun _ -> 
        assert_equal 8 (valid_player_list_1 |> new_round |> active_players |> 
                        List.map (fun x -> to_list(hand x)) |> List.flatten |> 
                        List.sort_uniq compare |> List.length));
    "new round with input list of 10 players assigns each player in resulting 
    state 2 unique cards (ie border case)" >:: (fun _ -> 
        assert_equal 20 (valid_player_list_2 |> new_round |> active_players |> 
                         List.map (fun x -> to_list(hand x)) |> List.flatten |> 
                         List.sort_uniq compare |> List.length));
    "new round with input list of 2 players assigns each player in resulting 
    state 2 unique cards (ie border case)" >:: (fun _ -> 
        assert_equal 4 (valid_player_list_3 |> new_round |> active_players |> 
                        List.map (fun x -> to_list(hand x)) |> List.flatten |> 
                        List.sort_uniq compare |> List.length));
    "new round with input of list of 4 players results in state with 
    empty table" >:: (fun _ -> 
        assert_equal empty (valid_player_list_1 |> new_round |> table));
    "new round with input of list of 10 players results in state with 
    empty table (ie border case)" >:: (fun _ -> 
        assert_equal empty (valid_player_list_2 |> new_round |> table));
    "new round with input of list of 2 players results in state with 
    empty table (ie border case)" >:: (fun _ -> 
        assert_equal empty (valid_player_list_3 |> new_round |> table));
    "new round with input of list of 4 players results in state with 
    betting pool of 0" >:: (fun _ -> 
        assert_equal 0 (valid_player_list_1 |> new_round |> betting_pool));
    "new round with input of list of 10 players results in state with 
    betting pool of 0 (ie border case)" >:: (fun _ -> 
        assert_equal 0 (valid_player_list_2 |> new_round |> betting_pool));
    "new round with input of list of 2 players results in state with 
    betting pool of 0 (ie border case)" >:: (fun _ -> 
        assert_equal 0 (valid_player_list_3 |> new_round |> betting_pool));
    "remove_player results in state with active players without player that
    was removed" 
    >:: (fun _ -> 
        assert_equal players_left (remove_active_player st player_delete |> 
                                   active_players));
    "remove_all_active_players results in state with active players set to an
    empty list" 
    >:: (fun _ -> 
        assert_equal [] (remove_all_active_players st (st |> active_players) |> 
                         active_players));
    "remove_all_active_players results in state with all players set to the
    original active players" 
    >:: (fun _ -> 
        assert_equal og_act_players (remove_all_active_players st 
                                       og_act_players |> all_players));
    "state's all_players can be updated" >:: (fun _ -> 
        assert_equal valid_player_list_2 
          (change_all_players st valid_player_list_2 |> all_players));
    "state's active_players can be updated" >:: (fun _ -> 
        assert_equal valid_player_list_2 
          (change_active_players st valid_player_list_2 |> active_players));
    "state's table can be updated" >:: (fun _ -> 
        assert_equal dk (change_table st dk |> table));
    "state's betting pool can be updated" >:: (fun _ -> 
        assert_equal 30 (change_betting_pool st 30 |> betting_pool));
    "state's current bet can be updated" >:: (fun _ -> 
        assert_equal 30 (change_current_bet st 30 |> current_bet));
    "state's max bet can be updated" >:: (fun _ -> 
        assert_equal 400 (change_max_bet st 400 |> max_bet));
    "find_max bet evaluates correctly when players have varying amounts of money
    in their wallets" >:: (fun _ -> 
        assert_equal 4000 (find_max_bet test_state_p2_raise_1000));
    "state's rem_deck can be updated" >:: (fun _ -> 
        assert_equal dk (change_rem_deck dk st |> rem_deck));
    "has right number of active players after one quits" >:: (fun _ -> 
        assert_equal 2
          (test_state_p2_quit |> active_players |> List.length));
    "successfully removes a player from active list when quit" >:: (fun _ -> 
        assert_equal [name p1; name p3]
          (test_state_p2_quit |> active_players |> player_names []));
    "sucessfully removes a player from all players when quit" >:: (fun _ -> 
        assert_equal [name p1; name p3] 
          (test_state_p2_quit |> all_players |> player_names []));
    "check when current bet not equal to money betted should raise InvalidBet" 
    >:: (fun _ -> 
        assert_raises (InvalidBet) (fun () -> check test_state_cb_20 p2));
    "player's money is changed correctly when player calls" >:: (fun _ ->
        assert_equal 4980 (test_state_p2_call |> active_players 
                           |> get_money p2));
    "player's money betted is changed correctly when player calls" >:: (fun _ ->
        assert_equal 20 (test_state_p2_call |> active_players 
                         |> get_money_betted p2)); 
    "betting pool is changed correctly when player calls" >:: (fun _ ->
        assert_equal 40 (test_state_p2_call |> betting_pool));
    "last move of player is Default when player has not made a move yet" >:: 
    (fun _ -> assert_equal Default
        (test_state1 |> active_players |> get_last_move p2)); 
    "last move of player is Fold after the player folds" >:: (fun _ -> 
        assert_equal Fold
          (test_state_p2_fold |> all_players |> get_last_move p2));
    "last move of player is Call after the player calls" >:: (fun _ -> 
        assert_equal Check
          (test_state_p1_check |> active_players |> get_last_move p1));
    "last move of player is Call after the player calls" >:: (fun _ -> 
        assert_equal Call
          (test_state_p2_call |> active_players |> get_last_move p2)); 
    "last move of player is Allin after the player goes allin" >:: (fun _ -> 
        assert_equal Allin
          (test_state_p2_allin |> active_players |> get_last_move p2));
    "last move of player is Raise 1000 after the player raise 1000" >:: 
    (fun _ -> assert_equal (Raise 1000)
        (test_state_p2_raise_1000 |> active_players |> get_last_move p2));
    "get_info evaluates to the appropriate info record for p1 when p1 checks 
       when p2 and p3 have not made a move yet" >:: (fun _ -> 
        assert_equal (let i = get_info test_state1 p1 in 
                      {i with old_moves = [name p1, Check; name p2, Default; 
                                           name p3, Default]})
          (get_info test_state_p1_check p1));
    "get_info evaluates to the appropriate info record for p2 when p2 folds 
       when p1 has checked and p3 has not made a move yet" >:: (fun _ -> 
        assert_equal (let i = get_info test_state_p1_check 
                          (get_player 2 (active_players test_state_p1_check)) in
                      {i with old_moves = [name p1, Check; name p3, Default]})
          (get_info test_state_p2_fold_on_check 
             (get_player 2 (all_players test_state_p2_fold_on_check))));
    "get_info evaluates to the appropriate info record for p1 when 
       p1 raises 2000 when p2 and p3 have not made a move yet" >:: (fun _ -> 
        assert_equal (let i = get_info test_state1 p1 in 
                      {i with wallet = 3000; c_bet = 2000; m_betted = 2000; 
                              b_pool = 2000; 
                              old_moves = [name p1, (Raise 2000); 
                                           name p2, Default; 
                                           name p3, Default]})
          (get_info test_state_p1_raise_2000 
             (get_player 1 (active_players test_state_p1_raise_2000))));
    "get_info evaluates to the appropriate info record for p2 when 
       p2 calls when p1 has raised 2000 and p3 have not made a move yet" >:: 
    (fun _ -> assert_equal (let i = get_info test_state_p1_raise_2000 
                                (get_player 2 (active_players 
                                                 test_state_p1_raise_2000)) in 
                            {i with wallet = 3000; m_betted = 2000; 
                                    b_pool = 4000; 
                                    old_moves = [name p1, (Raise 2000); 
                                                 name p2, Call; 
                                                 name p3, Default]})
        (get_info test_state_p2_call_2000 
           (get_player 2 (active_players test_state_p2_call_2000))));
    "get_info evaluates to the appropriate info record for p3 when 
       p3 goes allin when p1 has raised 2000 and p2 called p1's bet" >:: 
    (fun _ -> assert_equal (let i = get_info test_state_p2_call_2000 
                                (get_player 3 (active_players 
                                                 test_state_p2_call_2000)) in 
                            {i with wallet = 0; c_bet = 5000; m_betted = 5000; 
                                    b_pool = 9000; 
                                    old_moves = [name p1, (Raise 2000); 
                                                 name p2, Call; 
                                                 name p3, Allin]})
        (get_info test_state_p3_allin 
           (get_player 3 (active_players test_state_p3_allin))));
  ]

let tests =
  List.flatten [
    player_tests;
    state_tests
  ]