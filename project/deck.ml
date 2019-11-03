(** [EmptyDeck] is the exception raised when actions are performed on a deck
    without enough cards to complete it. *)
exception EmptyDeck

type suit = Clubs | Diamonds | Hearts | Spades

(** The second element in the tuple is the rank of the card. Jack is 11, Queen
    is 12, King is 13, and Ace is 1. *)
type card = (suit * int)

type deck = card list

(** [standard_deck] is an ordered list of all 52 cards, going from clubs
    between Ace and King, then Diamonds, then Hearts, and then Spades. *)
let standard_deck = [
  (Clubs, 1); (Clubs, 2); (Clubs, 3); (Clubs, 4); (Clubs, 5); (Clubs, 6);
  (Clubs, 7); (Clubs, 8); (Clubs, 9); (Clubs, 10); (Clubs, 11); (Clubs, 12);
  (Clubs, 13); (Diamonds, 1); (Diamonds, 2); (Diamonds, 3); (Diamonds, 4);
  (Diamonds, 5); (Diamonds, 6); (Diamonds, 7); (Diamonds, 8); (Diamonds, 9); 
  (Diamonds, 10); (Diamonds, 11); (Diamonds, 12); (Diamonds, 13); (Hearts, 1);
  (Hearts, 2); (Hearts, 3); (Hearts, 4); (Hearts, 5); (Hearts, 6); (Hearts, 7); 
  (Hearts, 8); (Hearts, 9); (Hearts, 10); (Hearts, 11); (Hearts, 12); 
  (Hearts, 13); (Spades, 1); (Spades, 2); (Spades, 3); (Spades, 4); (Spades, 5);
  (Spades, 6); (Spades, 7); (Spades, 8); (Spades, 9); (Spades, 10); 
  (Spades, 11); (Spades, 12); (Spades, 13)
  ]

(** [get_card n d r] returns a tuple of the [n]th card in [d] and [r] which are
    all cards in [d] preceeding the [n]th card. [r] might not be in same order 
    as [d] was.
    Raises [EmptyDeck] is [n] is greater than the length of [d]. *)
let rec get_card n d r =
  match d with
  | [] -> raise EmptyDeck
  | h :: t ->
    if n > 0 then get_card (n - 1) t (h:: r)
    else (h, List.rev_append r t)

(** [shuffle_help n d lst] is a tail-recursive implmenentation of shuffle
    which returns [lst]. *)
let rec shuffle_help n d lst =
  if n > 0
  then
    match get_card (Random.int n) d [] with
    | (c, r) -> shuffle_help (n - 1) r (c :: lst)
  else lst

let shuffle =
  shuffle_help 52 standard_deck []

let shuffle d = failwith "Unimplemented"

(** [draw_card_help n d lst] is a tail-recursive implementation of draw_card
    which returns [lst]. *)
let rec draw_card_help n d lst =
  if n > 0 
  then match d with
  | [] -> raise EmptyDeck
  | h :: t -> draw_card_help (n - 1) t (h :: lst)
  else (lst, d)

let draw_card n d =
  draw_card_help n d []
