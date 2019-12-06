open Deck
open Command

(** 
    Representation of a player in the game.
*)

(** The type [status] represents the status of a player*)
type s =
  | Betting
  | AllIn
  | Folded
  | Out

(** The type [blind] represents the blind of a player*)
type b =
  | Big
  | Small
  | None

(** The abstract type of values representing players. *)
type player

(** [create_player n] is a player with name n, no hand, starting money of 5000,
   and status Active. This is not an AI player*)
val create_player : string -> player

(** [create_ai_player] is an AI player. They start with all the same fields as
   a human player, with a name of "The AI."*)
val create_ai_player : string -> player

(** [name p] is the name of player [p]. *)
val name : player -> string

(** [hand p] is a deck of two cards representing the hand of player [p]. *)
val hand : player -> deck

(** [change_hand p h] is the player [p] with hand [h]. *)
val change_hand : player -> deck-> player

(** [money p] is the amount of money player [p] currently has. *)
val money : player -> int

(** [change_money p m] is the player [p] with [m] added to the money player 
    [p] currently has. *)
val change_money : player -> int -> player

(** [status p] is the current status of player [p]. *)
val status : player -> s

(** [change_status p st] is the player [p] with status [st] *)
val change_status : player -> s -> player

(** [blind p] is the blind of player [p]. *)
val blind : player -> b

(** [change_status p bl] is the player [p] with blind [bl] *)
val change_blind : player -> b -> player

(** [money_betted p] is the amount of money player [p] currently has betted. *)
val money_betted : player -> int

(** [change_money_betted p mb] is the player [p] with [mb] added to the money 
   player [p] currently has betted. *)
val change_money_betted : player -> int -> player

(** [reset_money_betted p] sets [p]'s money back to 0. *)
val reset_money_betted : player -> player

(** [last_move p] is the last move player [p] made*)
val last_move : player -> command

(** [change_last_move p str] is the player [p] with [str] as its last move. *)
val change_last_move : player -> command -> player

(** [reset_last_move p] resets [p]'s last move to Default *)
val reset_last_move : player -> player

(** [is_ai p] is the AI status of [p]. *)
val is_ai : player -> bool
