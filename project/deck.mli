(** 
    Representation of a deck of cards.
*)

(** The type of a card suit. *)
type suit = Clubs | Diamonds | Hearts | Spades

(** The type of a card. *)
type card

(** The type of a deck. *)
type deck

(** [empty] is the empty deck. *)
val empty : deck

(** [sorted_deck] is an ordered list of all 52 cards, going from clubs
    between Ace and King, then Diamonds, then Hearts, and then Spades. *)
val sorted_deck : deck

(** [shuffle] is a randomized deck of 52 cards without duplicates. *)
val shuffle : deck

(** [is_empty d] is true if [d] is empty and false otherwise. *)
val is_empty : deck -> bool

(** [draw_card n d] is a tuple of a list of [n] cards that are not in [d] and
    the deck which remains after the cards are drawn. 
    Raises [EmptyDeck] if [n] is greater than the number of cards in [d]. *)
val draw_card : int -> deck -> deck * deck

(** [to_list d] is a list of deck. The cards are a tuple of a suit and rank. *)
val to_list : deck -> (suit * int) list
