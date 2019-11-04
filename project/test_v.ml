open OUnit2
open Deck

let tests = [
    "the empty deck is empty" >:: (fun _ -> assert_equal true (Deck.is_empty
                                                                 Deck.empty));
    "the sorted deck is not" >:: (fun _ -> assert_equal false (Deck.is_empty
                                                                 Deck.sorted_deck));

    "will not draw any cards when asked" >:: (fun _ -> assert_equal
                                                 (Deck.empty, Deck.sorted_deck) (Deck.draw_card 0 Deck.sorted_deck));
    "draws the three cards as asked" >:: (fun _ -> assert_equal
                                             ([(Clubs, 3); (Clubs, 2); (Clubs, 1)], 
                                              [(Clubs, 4); (Clubs, 5); (Clubs, 6); (Clubs, 7); (Clubs, 8); (Clubs, 9); 
                                               (Clubs, 10); (Clubs, 11); (Clubs, 12); (Clubs, 13); (Diamonds, 1); 
                                               (Diamonds, 2); (Diamonds, 3); (Diamonds, 4); (Diamonds, 5); 
                                               (Diamonds, 6); (Diamonds, 7); (Diamonds, 8); (Diamonds, 9); 
                                               (Diamonds, 10); (Diamonds, 11); (Diamonds, 12); (Diamonds, 13); 
                                               (Hearts, 1); (Hearts, 2); (Hearts, 3); (Hearts, 4); (Hearts, 5); 
                                               (Hearts, 6); (Hearts, 7); (Hearts, 8); (Hearts, 9); (Hearts, 10); 
                                               (Hearts, 11); (Hearts, 12); (Hearts, 13); (Spades, 1); (Spades, 2); 
                                               (Spades, 3); (Spades, 4); (Spades, 5); (Spades, 6); (Spades, 7); 
                                               (Spades, 8); (Spades, 9); (Spades, 10); (Spades, 11); (Spades, 12); 
                                               (Spades, 13)])
                                             (match Deck.draw_card 3 Deck.sorted_deck with
                                              | (c, r) -> (Deck.to_list c, Deck.to_list r)));
    "can draw the entire deck, leaving it empty" >:: (fun _ -> assert_equal
                                                         (List.rev (Deck.to_list Deck.sorted_deck), [])
                                                         (match Deck.draw_card 52 Deck.sorted_deck with
                                                          | (c, r) -> (Deck.to_list c, Deck.to_list r)));
    "cannot draw more cards than there are cards in the deck" >:: (fun _ ->
        assert_raises EmptyDeck (fun _ -> Deck.draw_card 1 Deck.empty));
    "cannot draw negative cards" >:: (fun _ -> assert_raises InvalidArgument
                                         (fun _ -> Deck.draw_card (-1) Deck.sorted_deck));
]
