open OUnit2
open Deck
open Player
open State


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
let player_delete = List.nth (st |> active_players) 0
let players_left = [List.nth (st |> active_players) 1; 
                    List.nth (st |> active_players) 2;
                    List.nth (st |> active_players) 3]
let t = empty |> insert Clubs 2 |> insert Diamonds 7 |> insert Diamonds 6

let p1 = create_player "A"
let p2 = create_player "B"
let p3 = create_player "C"
let p_lst1 = [p1; p2; p3]
let test_state1 = new_round p_lst1
let test_state_cb_20 = let s' = change_current_bet test_state1 20 
  in change_betting_pool s' 20
let test_state_p2_quit = quit test_state1 p2
let test_state_p1_check = check test_state1 p1
let test_state_p2_call = call test_state_cb_20 p2

let rec get_money p lst =
  match lst with
  | [] -> failwith "empty list"
  | h :: t -> if name h = name p then money h else get_money p t

let rec get_money_betted p lst =
  match lst with
  | [] -> failwith "empty list"
  | h :: t -> if name h = name p then money_betted h else get_money_betted p t

let rec player_names  acc lst=
  match lst with 
  | [] -> List.rev acc
  | h :: t -> player_names (name h :: acc) t

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
                        List.filter (fun x -> List.length x = 2)|> List.length));
    "new round with input list of 10 players assigns each player in resulting 
    state 2 cards (ie border case)" >:: (fun _ -> 
        assert_equal 10 (valid_player_list_2 |> new_round |> active_players |> 
                         List.map (fun x -> to_list(hand x)) |> 
                         List.filter (fun x -> List.length x = 2)|> List.length));
    "new round with input list of 2 players assigns each player in resulting 
    state 2 cards (ie border case)" >:: (fun _ -> 
        assert_equal 2 (valid_player_list_3 |> new_round |> active_players |> 
                        List.map (fun x -> to_list(hand x)) |> 
                        List.filter (fun x -> List.length x = 2)|> List.length));
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
        assert_equal players_left (remove_active_player st player_delete|> 
                                   active_players));
    "state's table can be updated" >:: (fun _ -> 
        assert_equal t (change_table st t |> table));
    "state's betting pool can be updated" >:: (fun _ -> 
        assert_equal 30 (change_betting_pool st 30 |> betting_pool));
    "has right number of active players after one quits" >:: (fun _ -> 
        assert_equal 2
          (test_state_p2_quit |> active_players |> List.length));
    "successfully removes a player from active list when quit" >:: (fun _ -> 
        assert_equal [name p1; name p3]
          (test_state_p2_quit |> active_players |> player_names []));
    "removes a player from all players when quit" >:: (fun _ -> 
        assert_equal [name p1; name p3] 
          (fold test_state_p2_quit player2 |> all_players |> player_names []));
    "check when current bet not equal to money betted should raise InvalidBet" 
    >:: (fun _ -> 
        assert_raises (InvalidBet) (fun () -> check test_state_cb_20 p2 ));
    "player's money is changed correctly when player calls" >:: (fun _ ->
        assert_equal 4980 (test_state_p2_call |> active_players 
                           |> get_money p2));
    "player's money betted is changed correctly when player calls" >:: (fun _ ->
        assert_equal 20 (test_state_p2_call |> active_players 
                         |> get_money_betted p2)); 
    "betting pool is changed correctly when player calls" >:: (fun _ ->
        assert_equal 40 (test_state_p2_call |> betting_pool)) 
  ]

let tests =
  List.flatten [
    player_tests;
    state_tests
  ]