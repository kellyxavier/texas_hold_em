open OUnit2
open Rank_hand
open Hands

let tests = [
  (* 
  (*================HIGH VALUE TESTS================*)

  "a higher-card hand beats a lower one" >:: (fun _ -> 
      assert_equal true (hand_value hv_low < hand_value hv_high));

  "an ace beats all high-value" >:: (fun _ -> 
      assert_equal true (hand_value hv_high < hand_value hv_ace));

  (*================PAIR TESTS================*)

  "a higher pair beats a lower pair" >:: (fun _ ->
      assert_equal true (hand_value pair_low < hand_value pair_high));

  "an ace beats all pairs" >:: (fun _ ->
      assert_equal true (hand_value pair_high < hand_value pair_ace));

  "the same pair will go to the higher-valued hand" >:: (fun _ ->
      assert_equal true 
      (hand_value pair_equal_low < hand_value pair_equal_high));

  "pair beats high-valued hand" >:: (fun _ ->
      assert_equal true (hand_value hv_ace < hand_value pair_low));

  (*================TWO PAIR TESTS================*)

  "a higher two pair beats a lower two pair" >:: (fun _ ->
      assert_equal true (hand_value tpair_low < hand_value tpair_high));

  "an ace beats all two pairs" >:: (fun _ ->
      assert_equal true (hand_value tpair_high < hand_value tpair_ace));

  "two pair beats high-valued hand" >:: (fun _ ->
      assert_equal true (hand_value hv_ace < hand_value tpair_low));

  "two pair beats pair hand" >:: (fun _ ->
      assert_equal true (hand_value pair_ace < hand_value tpair_low));

  (*================FLUSH TESTS================*)

  "flush with a king beats a flush with a lower high card" >:: (fun _ -> 
      assert_equal true (hand_value flush_low < hand_value flush_high));

  "flush with an ace beats a flush with a lower high card" >:: (fun _ -> 
      assert_equal true (hand_value flush_high < hand_value flush_ace));

  "flush beats high-valued hand" >:: (fun _ ->
      assert_equal true (hand_value hv_ace < hand_value flush_low));

  (*================ROYAL FLUSH TESTS================*)

  "royal flush beats all" >:: (fun _ ->
      assert_equal true (hand_value flush_ace < hand_value royal_flush)); *)

  "two pair beats pair hand" >:: (fun _ ->
      assert_equal true (hand_value pair_ace < hand_value tpair_low));
]
