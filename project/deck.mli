(** 

    Representation of a deck of cards.
*)

(** [EmptyDeck] is the exception raised when actions are performed on a deck
    without enough cards to complete it. *)
exception EmptyDeck

(** [InvalidArgument] is the exception raise when invalid parameters are used
    on a function. *)
exception InvalidArgument

(** The type of a card suit. *)
type suit = Clubs | Diamonds | Hearts | Spades

(** The abstract type of a card. *)
type card

(** The abstract type of a deck. *)
type deck

(** [empty] is the empty deck. *)
val empty : deck

(** [sorted_deck] is an ordered list of all 52 cards, going from Clubs
    between Ace and King, then Diamonds, then Hearts, and then Spades. *)
val sorted_deck : deck

(** [shuffle] is a randomized deck of 52 cards without duplicates. *)
val shuffle : unit -> deck

(** [is_empty d] is true if [d] is empty and false otherwise. *)
val is_empty : deck -> bool

(** [insert s r d] is a deck [d] with a card of suit [s] and rank [r] added
    on the top of the deck. 
    Requires: [r] is between 1 and 13, inclusive. *)
val insert : suit -> int -> deck -> deck

(** [draw_card n d] is a tuple of a list of [n] cards that are not in [d] and
    the deck which remains after the cards are drawn. 
    Raises [EmptyDeck] if [n] is greater than the number of cards in [d]. 
    Raises [InvalidArgument] if [n] is less than 0. *)
val draw_card : int -> deck -> deck * deck

(** [add d1 d2] is [d1] placed on top of [d2]. *)
val add : deck -> deck -> deck

(** [to_list d] is a list of deck. The cards are a tuple of a suit and rank. *)
val to_list : deck -> (suit * int) list

(** [to_string d] is a string of the deck in the following format:
    Ace of Clubs
    5 of Diamonds *)
val to_string : deck -> string
