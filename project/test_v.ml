open OUnit2
open Deck

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

let tests = [
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
