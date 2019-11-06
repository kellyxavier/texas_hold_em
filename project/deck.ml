exception EmptyDeck

exception InvalidArgument

type suit = Clubs | Diamonds | Hearts | Spades

(** The second element in the tuple is the rank of the card. Jack is 11, Queen
    is 12, King is 13, and Ace is 1. *)
type card = (suit * int)

type deck = card list

let empty = []

let sorted_deck = [
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

let is_empty d =
  match d with
  | [] -> true
  | _ -> false

let insert s r d =
  (s, r) :: d

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

let shuffle u =
  shuffle_help 52 sorted_deck []

(** [draw_card_help n d lst] is a tail-recursive implementation of draw_card
    which returns [lst]. *)
let rec draw_card_help n d lst =
  if n < 0 then raise InvalidArgument
  else if n > 0 
  then match d with
  | [] -> raise EmptyDeck
  | h :: t -> draw_card_help (n - 1) t (h :: lst)
  else (lst, d)

let draw_card n d =
  draw_card_help n d []

let to_list d = d

(** [print_rank r] is the string of the rank [r]. *)
let print_rank r =
  match r with
  | 1 -> "Ace"
  | 11 -> "Jack"
  | 12 -> "Queen"
  | 13 -> "King"
  | n -> string_of_int n

(** [print_suit s] is the string of the suit [s]. *)
let print_suit s =
  match s with
  | Clubs -> "Clubs"
  | Diamonds -> "Diamonds"
  | Hearts -> "Hearts"
  | Spades -> "Spades"

(** [print_card c str] is the string [str] of card [c]. *)
let print_card c str =
  match c with
  | (s, r) -> str ^ (print_rank r) ^ " of " ^ (print_suit s) ^ "\n"

(**[to_string_helper d str] is a tail-recursive implementation of to_string
    which returns [str]. *)
let rec to_string_helper d str =
  match d with
  | [] -> str
  | h :: t -> to_string_helper t ((print_card h "") ^ str)

let to_string d =
  to_string_helper d ""
