(** 
    Representation of a deck of cards.
*)

(** The type of a card suit. *)
type suit = Clubs | Diamonds | Hearts | Spades

(** The type of a card. *)
type card = (suit * int)

(** The type of a deck. *)
type deck = card list

(** [shuffle] is a randomized deck of 52 cards without duplicates. *)
val shuffle : deck

(** [draw_card n d] is a list of [n] cards drawn from [d]. 
    Raises [EmptyDeck] if [n] is greater than the number of cards in [d]. *)
val draw_card : int -> deck -> card list
