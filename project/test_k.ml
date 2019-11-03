open OUnit2
open Deck
open Player


let p1 = create_player "kelly"

let player_tests =
  [
    "tests player's hand is empty " >:: (fun _ -> 
        assert_equal empty (p1 |> hand));

  ]

let tests =
  List.flatten [
    player_tests;
  ]