open OUnit2
open Deck
open Player


let p1 = create_player "kelly"

let deck_tests =
  [

  ]

let player_tests =
  [
    "tests player's hand is empty " >:: (fun _ -> 
        assert_equal [] (p1 |> hand));
  ]

let suite =
  "test suite for A2"  >::: List.flatten [
    deck_tests;
    player_tests;
  ]

let _ = run_test_tt_main suite
