open OUnit2
open Deck
open Player


let player p1 = create_player "kelly"

let deck_tests =
  [

  ]

let player_tests =
  [
    "tests player's hand is empty " >:: (fun _ -> 
        assert_equal [] (p1 |> hand));
  ]