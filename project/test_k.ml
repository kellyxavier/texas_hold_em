open OUnit2
open Deck
open Player


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
let p9 = change_blind p1 Little

let player_tests =
  [
    "tests player's name is set correctly " >:: (fun _ -> 
        assert_equal "kelly" (p1 |> name));
    "tests player's initial hand is empty " >:: (fun _ -> 
        assert_equal empty (p1 |> hand));
    "tests player's inital money is 5000 " >:: (fun _ -> 
        assert_equal 5000 (p1 |> money));
    "tests player's initial status is Active " >:: (fun _ -> 
        assert_equal Active (p1 |> status));
    "tests player's initial blind is None " >:: (fun _ -> 
        assert_equal None (p1 |> blind));
    "tests player's updated hand is appropriate size " >:: (fun _ -> 
        assert_equal 2 (p2 |> hand |> to_list |> List.length));
    "tests player's updated hand is expected cards" >:: (fun _ -> 
        assert_equal [(Clubs, 2); (Clubs, 1)] (p2 |> hand |> to_list));
    "tests player's money can be increased " >:: (fun _ -> 
        assert_equal 5500 (p3 |> money));
    "tests player's money can be decreased " >:: (fun _ -> 
        assert_equal 4500 (p4 |> money));
    "tests player's status can be changed to AllIn " >:: (fun _ -> 
        assert_equal AllIn (p5 |> status));
    "tests player's status can be changed to Folded " >:: (fun _ -> 
        assert_equal Folded (p6 |> status));
    "tests player's status can be changed to Out " >:: (fun _ -> 
        assert_equal Out (p7 |> status));
    "tests player's blind can be changed to Big " >:: (fun _ -> 
        assert_equal Big (p8 |> blind));
    "tests player's blind can be changed to Little " >:: (fun _ -> 
        assert_equal Little (p9 |> blind));
  ]

let tests =
  List.flatten [
    player_tests;
  ]