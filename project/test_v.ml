open OUnit2
open Deck
open Command
open Player
open State

let test_deck_1 = 
  empty
  |> insert Diamonds 8 
  |> insert Clubs 5

let test_deck_2 = 
  empty 
  |> insert Clubs 11
  |> insert Spades 1

let test_deck_2_rev = 
  empty
  |> insert Spades 1
  |> insert Clubs 11

let test_deck = 
  test_deck_1 
  |> insert Clubs 11 
  |> insert Spades 1

let test_deck_rev =
  empty
  |> insert Spades 1
  |> insert Clubs 11
  |> insert Clubs 5
  |> insert Diamonds 8

let deck_tests = [

  "the empty deck is empty" >:: (fun _ -> 
      assert_equal true (is_empty empty));
  "the sorted deck is not" >:: (fun _ -> 
      assert_equal false (is_empty sorted_deck));

  "will not draw any cards when asked" >:: (fun _ -> 
      assert_equal (empty, test_deck) 
        (draw_card 0 test_deck));
  "draws 2 cards as asked" >:: (fun _ -> 
      assert_equal (test_deck_2_rev, test_deck_1) 
        (draw_card 2 test_deck));

  "can draw the entire deck, leaving it empty" >:: (fun _ -> 
      assert_equal (test_deck_rev, empty) (draw_card 4 test_deck));
  "cannot draw more cards than there are cards in the deck" >:: (fun _ ->
      assert_raises EmptyDeck (fun _ -> draw_card 1 empty));
  "cannot draw negative cards" >:: (fun _ -> 
      assert_raises InvalidArgument (fun _ -> 
          draw_card (-1) test_deck));

  "adding one deck on top of another" >:: (fun _ -> assert_equal test_deck
                                              (add test_deck_2 test_deck_1));
  "adding an empty deck on top does not change the deck" >:: (fun _ -> 
      assert_equal test_deck (add empty test_deck));
  "adding a deck on top of an empty one returns the original deck" >:: (fun _ ->
      assert_equal test_deck (add test_deck empty));
]

let command_tests = [

  "parses quit" >:: (fun _ -> assert_equal Quit (parse " quit "));
  "parses continue" >:: (fun _ -> assert_equal Continue (parse " continue "));
  "parses fold" >:: (fun _ -> assert_equal Fold (parse " fold "));
  "parses call" >:: (fun _ -> assert_equal Call (parse " call "));
  "parses check" >:: (fun _ -> assert_equal Check (parse " check "));
  "parses allin" >:: (fun _ -> assert_equal Allin (parse " allin "));
  "parses raise i" >:: (fun _ -> assert_equal (Raise 100) (parse "raise 100 "));

  "correctly requires raise to be followed by something" >:: (fun _ -> 
      assert_raises Malformed (fun _ -> parse " raise "));
  "correctly requires raise to be followed by an int" >:: (fun _ -> 
      assert_raises Malformed (fun _ -> parse " raise something"));
  "raises malformed when there's too many arguments" >:: (fun _ -> 
      assert_raises Malformed (fun _ -> parse " call 5 "));
  "raises malformed when the command cannot be parsed" >:: (fun _ -> 
      assert_raises Malformed (fun _ -> parse " fail "));

]

let player1 = create_player "A"
let player2 = create_player "B"
let player3 = create_player "C"
let player4 = change_money (create_player "D") (-50)

let player_lst1 = [player1; player2; player3]
let player_lst2 = [player1; player2; player3; player4]

let test_state_empty = new_round [player1]
let test_state1 = new_round player_lst1
let test_state2 = change_current_bet 
    (change_max_bet (new_round player_lst2) 500) 100

let rec get_money p lst =
  match lst with
  | [] -> failwith "player not in list"
  | h :: t -> if name h = name p then money h else get_money p t

let rec get_betted p lst =
  match lst with
  | [] -> failwith "player not in list"
  | h :: t -> if name h = name p then money_betted h else get_betted p t

let rec player_names  acc lst=
  match lst with 
  | [] -> List.rev acc
  | h :: t -> player_names (name h :: acc) t

let state_round_v_tests = [

  "successfully removes a player from active list when folding" >:: (fun _ -> 
      assert_equal [name player1; name player3]
        (fold test_state1 player2 |> active_players |> player_names []));
  "doesn't remove a player from all players when folding" >:: (fun _ -> 
      assert_equal [player1; change_last_move player2 Fold; player3] 
        (fold test_state1 player2 |> all_players)); 
  "all_in removes all of the player's money" >:: (fun _ -> assert_equal 0
                                                     (all_in test_state1 player1
                                                      |> active_players 
                                                      |> get_money player1)); 
  "all_in correctly increases the better pool" >:: (fun _ -> assert_equal 5000
                                                       (all_in test_state1 
                                                          player1 
                                                        |> betting_pool));
  "all_in correctly increase how much the player has betted" >:: (fun _ -> 
      assert_equal 5000
        (all_in test_state1 player1 |> active_players |> get_betted player1));
  "cannot all_in more than the max bet" >:: (fun _ -> assert_raises InvalidBet
                                                (fun _ -> all_in test_state2 
                                                    player1));

  "raise i correctly updates player wallet" >:: (fun _ -> 
      assert_equal 4875
        (raise 25 test_state2 player4 |> active_players |> get_money player4));
  "raise i correctly updates amount player has betted" >:: (fun _ -> 
      assert_equal 125 
        (raise 25 test_state2 player4 |> active_players |> get_betted player4));
  "raise i correctly updates the betting pool" >:: (fun _ -> assert_equal 75
                                                       (raise 25 test_state2 
                                                          player4 |> 
                                                        betting_pool));
  "cannot raise more than the max bet" >:: (fun _ -> assert_raises InvalidBet
                                               (fun _ -> raise 476 test_state2 
                                                   player4));

  "correctly identifies an active player list of one or fewer players" >::
  (fun _ -> assert_equal true (only_one_player test_state_empty));
  "correctly identifies an active player list of more than 1 players" >::
  (fun _ -> assert_equal false (only_one_player test_state1));
]

let p1 = create_player "tester"
let p2 = change_money (create_player "bankrupt") (-5000)
let ai = create_ai_player Med

let player_tests = [
  "player's initial money betted is 0 " >:: (fun _ -> 
      assert_equal 0 (p1 |> money_betted));
  "can increment money betted" >:: (fun _ -> assert_equal 50 
                                       (change_money_betted p1 50 
                                        |> money_betted));
  "can reset money betted" >:: (fun _ -> assert_equal 0
                                   (reset_money_betted p1 |> money_betted));

  "player's initial last move is default" >:: (fun _ -> 
      assert_equal Default (p1 |> last_move));
  "player's last move can be changed to quit" >:: (fun _ -> 
      assert_equal Quit (change_last_move p1 Quit |> last_move));
  "player's last move can be changed to continue" >:: (fun _ -> 
      assert_equal Continue (change_last_move p1 Continue |> last_move));
  "player's last move can be changed to fold" >:: (fun _ -> 
      assert_equal Fold (change_last_move p1 Fold |> last_move));
  "player's last move can be changed to call" >:: (fun _ -> 
      assert_equal Call (change_last_move p1 Call |> last_move));
  "player's last move can be changed to check" >:: (fun _ -> 
      assert_equal Check (change_last_move p1 Check |> last_move));
  "player's last move can be changed to raise" >:: (fun _ -> 
      assert_equal (Raise 50) (change_last_move p1 (Raise 50) |> last_move));
  "player's last move can be changed to all in" >:: (fun _ -> 
      assert_equal Allin (change_last_move p1 Allin |> last_move));
  "can reset player's last move" >:: (fun _ -> assert_equal Default
                                         (reset_last_move p1 |> last_move));

  "AI's name is set correctly " >:: (fun _ -> 
      assert_equal "Medium AI" (ai |> name)); 
  "AI's initial hand is empty " >:: (fun _ -> 
      assert_equal empty (ai |> hand));
  "AI's inital money is 5000 " >:: (fun _ -> 
      assert_equal 5000 (ai |> money));
  "AI's initial status is Active " >:: (fun _ -> 
      assert_equal Betting (ai |> status));
  "AI's initial blind is None " >:: (fun _ -> 
      assert_equal None (ai |> blind));
  "AI's initial money betted is 0 " >:: (fun _ -> 
      assert_equal 0 (ai |> money_betted));
  "AI's initial last move is default" >:: (fun _ -> 
      assert_equal Default (ai |> last_move));

  "human player is not an AI" >:: (fun _ -> assert_equal false (is_ai p1));
  "AI player is an AI" >:: (fun _ -> assert_equal true (is_ai ai));

  "\"Easy AI\" is an AI name" >:: (fun _ -> assert_equal true (is_ai_name 
                                                                 "Easy AI"));
  "\"Medium AI\" is an AI name" >:: (fun _ -> 
      assert_equal true (is_ai_name "Medium AI"));
  "\"Hard AI\" is an AI name" >:: (fun _ -> assert_equal true (is_ai_name 
                                                                 "Hard AI"));
  "names that aren't reserved for the AI correctly identified" >:: (fun _ -> 
      assert_equal false (is_ai_name "Test"));
  "names that aren't reserved for the AI correctly identified" >:: (fun _ -> 
      assert_equal false (is_ai_name ""));

  "correctly identifies a list of AIs" >:: (fun _ -> assert_equal true
                                               (only_ais [ai]));
  "correctly identifies a list of non-AIs" >:: (fun _ -> assert_equal false
                                                   (only_ais [p1]));
  "correctly identifies when one player is broke" >:: (fun _ -> 
      assert_equal true (one_no_money [p2]));
  "correctly identifies when more than one player is broke" >:: (fun _ -> 
      assert_equal true (one_no_money [p2; p2]));
  "correctly identifies when one player is broke" >:: (fun _ -> 
      assert_equal false (one_no_money [p1]));
  "the empty list does not have one player without money" >:: (fun _ -> 
      assert_equal false (one_no_money []));

  "correctly identifies when all players are broke" >:: (fun _ -> 
      assert_equal true (one_no_money [p2; p2]));
  "correctly identifies when some players are broke" >:: (fun _ -> 
      assert_equal true (one_no_money [p1; p2]));
  "correctly identifies when no players are broke" >:: (fun _ -> 
      assert_equal false (one_no_money [p1]));
  "the empty list does not have players without money" >:: (fun _ -> 
      assert_equal false (one_no_money []));
]

let tests =
  List.flatten [
    deck_tests;
    command_tests;
    state_round_v_tests;
    player_tests;
  ]
