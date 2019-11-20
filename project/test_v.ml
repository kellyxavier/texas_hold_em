open OUnit2
open Deck
open Command
open Player
open State

let test_draw_deck_1 = 
  empty
  |> insert Diamonds 8 
  |> insert Clubs 5

let test_draw_deck_2 = 
  empty 
  |> insert Clubs 11
  |> insert Spades 1

let test_draw_deck_2_rev = 
  empty
  |> insert Spades 1
  |> insert Clubs 11

let test_draw_deck = 
  test_draw_deck_1 
  |> insert Clubs 11 
  |> insert Spades 1

let test_draw_deck_rev =
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
      assert_equal (empty, test_draw_deck) 
        (draw_card 0 test_draw_deck));
  "draws 2 cards as asked" >:: (fun _ -> 
      assert_equal (test_draw_deck_2_rev, test_draw_deck_1) 
        (draw_card 2 test_draw_deck));

  "can draw the entire deck, leaving it empty" >:: (fun _ -> 
      assert_equal (test_draw_deck_rev, empty) (draw_card 4 test_draw_deck));
  "cannot draw more cards than there are cards in the deck" >:: (fun _ ->
      assert_raises EmptyDeck (fun _ -> draw_card 1 empty));
  "cannot draw negative cards" >:: (fun _ -> 
      assert_raises InvalidArgument (fun _ -> 
          draw_card (-1) test_draw_deck));
]

let command_tests = [

  "parses quit" >:: (fun _ -> assert_equal Quit (parse " quit "));
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

let player_lst = [player1; player2; player3; player4]

let test_state1 = new_round player_lst
let test_state2 = change_current_bet (change_max_bet test_state1 500) 100

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

let state_round__v_tests = [

  "successfully removes a player from active list when folding" >:: (fun _ -> 
      assert_equal [name player1; name player3; name player4]
        (fold test_state1 player2 |> active_players |> player_names []));
  "doesn't remove remove a player from all players when folding" >:: (fun _ -> 
      assert_equal [player1; player2; player3; player4] 
        (fold test_state1 player2 |> all_players)); 

  (* "all_in removes all of the player's money" >:: (fun _ -> assert_equal 0
                                                     (all_in test_state1 player1 |> active_players |> get_money player1)~printer:string_of_int); 
     "all_in correctly increases the better pool" >:: (fun _ -> assert_equal 5000
                                                       (all_in test_state1 player1 |> betting_pool)~printer:string_of_int);
     "all_in correctly increase how much the player has betted" >:: (fun _ -> 
      assert_equal 5000
        (all_in test_state1 player1 |> active_players |> get_betted player1)~printer:string_of_int);
     "cannot all_in more than the max bet" >:: (fun _ -> assert_raises InvalidBet
                                                (fun _ -> all_in test_state2 player1)); *)

  "raise i correctly updates player wallet" >:: (fun _ -> 
      assert_equal 4875
        (raise 25 test_state2 player4 |> active_players |> get_money player4));
  "raise i correctly updates amount player has betted" >:: (fun _ -> 
      assert_equal 125 
        (raise 25 test_state2 player4 |> active_players |> get_betted player4));
  "raise i correctly updates the betting pool" >:: (fun _ -> assert_equal 75
                                                       (raise 25 test_state2 player4 |> betting_pool));
  "cannot raise more than the max bet" >:: (fun _ -> assert_raises InvalidBet
                                               (fun _ -> raise 476 test_state2 player4));

]

let tests =
  List.flatten [
    deck_tests;
    command_tests;
    state_round__v_tests
  ]
