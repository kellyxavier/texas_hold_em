module type RankHand = sig
  include Deck
  val hand_value : deck -> int
end

module RankHand(D : Deck) : RankHand = struct
  include D

  type sorted_hand = {
    clubs : card list;
    diamonds : card list;
    hearts : card list;
    spades : card list;
    ranks : card list;

  }

  let rec sort_hand c d h s r = function
    | [] -> {
        clubs = (List.sort compare c); 
        diamonds = (List.sort compare d); 
        hearts = (List.sort compare h); 
        spades = (List.sort compare s); 
        ranks = (List.sort compare r)
      }
    | (suit, rank) :: t -> begin
        match suit with
        | Clubs -> sort_hand (c :: rank) d h s (r :: rank)
        | Diamonds -> sort_hand c (d :: rank) h s (r :: rank)
        | Hearts -> sort_hand c d (h :: rank) s (r :: rank)
        | Spades -> sort_hand c d h (s :: rank) (r :: rank)
      end

  let hand_value hand =

end