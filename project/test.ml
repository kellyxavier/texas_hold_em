open OUnit2
open Deck
open Player

open Test_d
open Test_k
open Test_v

let p1 = create_player "kelly"

let deck_tests =
  [

  ]

let player_tests =
  [
    "tests player's hand is empty " >:: (fun _ -> 
        assert_equal empty (p1 |> hand));

  ]

let suite =
  "test suite for A2"  >::: List.flatten [
    deck_tests;
    player_tests;
  ]

let tests_merged = 
  List.flatten [
    Test_d.tests;
    Test_k.tests;
    Test_v.tests
  ]

let _ = run_test_tt_main suite
