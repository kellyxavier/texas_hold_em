open OUnit2
open OUnit2
open Rank_hand
open Hands

let tests = [

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

(*  "two pair beats pair hand" >:: (fun _ ->
      assert_equal true (hand_value pair_ace < hand_value tpair_low)); *)

  (*================THREE OF A KIND TESTS================*)

  "a higher toak beats a lower toak" >:: (fun _ ->
      assert_equal true (hand_value toak_low < hand_value toak_high));

  "an ace beats all toak" >:: (fun _ ->
      assert_equal true (hand_value toak_high < hand_value toak_ace));

  "toak beats high-valued hand" >:: (fun _ ->
      assert_equal true (hand_value hv_ace < hand_value toak_low));

  "toak beats pair hand" >:: (fun _ ->
      assert_equal true (hand_value pair_ace < hand_value toak_low));

  "toak beats two pair hand" >:: (fun _ ->
      assert_equal true (hand_value tpair_ace < hand_value toak_low));

  (*================STRAIGHT TESTS================*)
  
  "a higher stra beats a lower stra" >:: (fun _ ->
      assert_equal true (hand_value stra_low < hand_value stra_high));

  "an ace beats all stra" >:: (fun _ ->
      assert_equal true (hand_value stra_high < hand_value stra_ace));

  "stra beats high-valued hand" >:: (fun _ ->
      assert_equal true (hand_value hv_ace < hand_value stra_low));

  "stra beats pair hand" >:: (fun _ ->
      assert_equal true (hand_value pair_ace < hand_value stra_low));

  "stra beats two pair hand" >:: (fun _ ->
      assert_equal true (hand_value tpair_ace < hand_value stra_low));

  "stra beats toak" >:: (fun _ ->
      assert_equal true (hand_value toak_ace < hand_value stra_low));

  (*================FLUSH TESTS================*)

  "flush with a king beats a flush with a lower high card" >:: (fun _ -> 
      assert_equal true (hand_value flush_low < hand_value flush_high));

  "flush with an ace beats a flush with a lower high card" >:: (fun _ -> 
      assert_equal true (hand_value flush_high < hand_value flush_ace));
  
  "flush beats high-valued hand" >:: (fun _ ->
      assert_equal true (hand_value hv_ace < hand_value flush_low));

  "flush beats pair hand" >:: (fun _ ->
      assert_equal true (hand_value pair_ace < hand_value flush_low));

  "flush beats two pair hand" >:: (fun _ ->
      assert_equal true (hand_value tpair_ace < hand_value flush_low));

  "flush beats toak" >:: (fun _ ->
      assert_equal true (hand_value toak_ace < hand_value flush_low));

  "flush beats stra" >:: (fun _ ->
      assert_equal true (hand_value stra_ace < hand_value flush_low));

  (*================FULL HOUSE TESTS================*)

  "full house with a high card beats a full house with a low" >:: (fun _ -> 
      assert_equal true (hand_value fh_low < hand_value fh_high));

  "full house with an ace beats all other full house" >:: (fun _ -> 
      assert_equal true (hand_value fh_high < hand_value fh_ace));
  
  "full house beats high-valued hand" >:: (fun _ ->
      assert_equal true (hand_value hv_ace < hand_value fh_low));

  "full house beats pair hand" >:: (fun _ ->
      assert_equal true (hand_value pair_ace < hand_value fh_low));

  "full house beats two pair hand" >:: (fun _ ->
      assert_equal true (hand_value tpair_ace < hand_value fh_low));

  "full house beats toak" >:: (fun _ ->
      assert_equal true (hand_value toak_ace < hand_value fh_low));

  "full house beats stra" >:: (fun _ ->
      assert_equal true (hand_value stra_ace < hand_value fh_low));

  "full house beats flush" >:: (fun _ ->
      assert_equal true (hand_value flush_ace < hand_value fh_low));

  (*================FOUR OF A KIND TESTS================*)

  "foak with a high card beats a foak with a low" >:: (fun _ -> 
      assert_equal true (hand_value foak_low < hand_value foak_high));

  "foak with an ace beats all other foak" >:: (fun _ -> 
      assert_equal true (hand_value foak_high < hand_value foak_ace));
  
  "foak beats high-valued hand" >:: (fun _ ->
      assert_equal true (hand_value hv_ace < hand_value foak_low));

  "foak beats pair hand" >:: (fun _ ->
      assert_equal true (hand_value pair_ace < hand_value foak_low));

  "foak beats two pair hand" >:: (fun _ ->
      assert_equal true (hand_value tpair_ace < hand_value foak_low));

  "foak beats toak" >:: (fun _ ->
      assert_equal true (hand_value toak_ace < hand_value foak_low));

  "foak beats stra" >:: (fun _ ->
      assert_equal true (hand_value stra_ace < hand_value foak_low));

  "foak beats flush" >:: (fun _ ->
      assert_equal true (hand_value flush_ace < hand_value foak_low));

  "foak beats full house" >:: (fun _ ->
      assert_equal true (hand_value fh_ace < hand_value foak_low));

  (*================ROYAL FLUSH TESTS================*)

  "royal flush beats all" >:: (fun _ ->
      assert_equal true (hand_value flush_ace < hand_value royal_flush));

]
