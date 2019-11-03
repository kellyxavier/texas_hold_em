open Deck

(** 
    Representation of a player in the game.
*)

(** The abstract type [status] represents the status of a player*)
type s

(** The abstract type [blind] represents the blind of a player*)
type b

(** The abstract type of values representing players. *)
type player

(**[create_player n] is a player with name n, no hand, starting money of 5000,
   and status Active*)
val create_player : string -> player

(**[hand p] is a deck of two cards representing the hand of player [p]. *)
val hand : player -> deck

(**[change_hand p h] is the player [p] with hand [h]. *)
val change_hand : player -> deck-> player

(**[money p] is the amount of money player [p] currently has. *)
val money : player -> int

(**[change_money p m] is the player [p] with [m] added to the money player 
   [p] currently has. *)
val change_money : player -> int -> player

(**[status p] is the current status of player [p]. *)
val status : player -> s

(**[change_status p s] is the player [p] with status [s] *)
val change_status : player -> s -> player









