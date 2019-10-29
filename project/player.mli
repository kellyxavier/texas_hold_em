open Deck

(** 
    Representation of a player in the game.
*)

(** The type [status] represents the status of a player*)
type status =
  | Active
  | AllIn
  | Folded
  | Out

(** The type [hand] represents the hand of a player*)
type hand = card list
(** The abstract type of values representing players. *)
type player =
  {
    name: string;
    (* h : hand; *)
    money : int;
    s : status
  }

(**[create_player n] is a player with name n, no hand, starting money of 5000,
   and status Active*)
val create_player : string -> player

(**[hand p] is a deck of two cards representing the hand of player [p]. *)
val hand : player -> hand

(**[money p] is the amount of money player [p] currently has. *)
val money : player -> int

(**[change_money p m] is the player [p] with [m] added to the money player 
   [p] currently has. *)
val change_money : player -> int -> player


(**[status p] is the current status of player [p]. *)
val status : player -> status

(**[change_status p s] is the player [p] with status [s] *)
val change_status : player -> status -> player

(**[new_round p] is the player [p] with status updated for new round 
   and a new hand *)
val new_round : player -> status -> player








