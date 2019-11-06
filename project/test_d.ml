open OUnit2
open Rank_hand
open Hands

let tests = [
  "flush with a king beats a flush with a lower high card" >:: (fun _ -> 
      assert_equal true (hand_value flush_queen < hand_value flush_king));
<<<<<<< HEAD
  "flush with an ace beats a flush with a lower high card" >:: (fun _ -> 
      assert_equal true (hand_value flush_king < hand_value flush_ace));
=======
(*  "flush with an ace beats a flush with a lower high card" >:: (fun _ -> 
     assert_equal true (hand_value flush_king < hand_value flush_ace)); *)
>>>>>>> c3f9ad95e67ba2de4ed8c62629aad3bd893d5877
]
