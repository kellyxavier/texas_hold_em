(** 
    Representation of a deck of cards.
*)

(** The type of a card suit. *)
type suit

(** The type of a card. *)
type card

(** The type of a deck. *)
type deck

(** [shuffle] is a randomized deck of 52 cards without duplicates. *)
val shuffle : deck

(** [shuffle d] is [d] shuffled into a random order. *)
val shuffle : deck -> deck

(** [draw_card n d] is a tuple of a list of [n] cards that are not in [d] and
    the deck which remains after the cards are drawn. 
    Raises [EmptyDeck] if [n] is greater than the number of cards in [d]. *)
val draw_card : int -> deck -> deck
