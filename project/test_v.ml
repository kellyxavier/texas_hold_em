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
  "parses raise i" >:: (fun _ -> assert_equal (Raise 100) 
                           (parse " raise 100 "));
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
let player_lst1 = [player1; player2; player3]
let test_state1 = new_round player_lst1
let test_state2 = test_state1

let rec get_money p lst =
  match lst with
  | [] -> failwith "empty list"
  | h :: t -> if name h = name p then money h else get_money p t

let rec player_names  acc lst=
  match lst with 
  | [] -> List.rev acc
  | h :: t -> player_names (name h :: acc) t


let state_round__v_tests = [
  "has right number of active players after one folds" >:: (fun _ -> 
      assert_equal 2
        (fold test_state1 player2 |> active_players |> List.length));
  "successfully removes a player from active list when folding" >:: (fun _ -> 
      assert_equal [name player1; name player3]
        (fold test_state1 player2 |> active_players |> player_names []));
  "doesn't remove remove a player from all players when folding" >:: (fun _ -> 
      assert_equal [player1; player2; player3] 
        (fold test_state1 player2 |> all_players)); 
  "all_in removes all of the player's money" >:: (fun _ -> assert_equal 0
                                                     (all_in test_state1 player1
                                                      |> active_players 
                                                      |> get_money player1)); 
]

let tests =
  List.flatten [
    deck_tests;
    command_tests;
    state_round__v_tests
  ]