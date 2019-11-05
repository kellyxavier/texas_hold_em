open Deck
open Player

(** 
   Representation of dynamic game state.

   This module represents the state of a game as it is being played,
   including the game's current list of players, the deck on the table,
   the betting pool, and functions that cause the state to change.
*)
exception InvalidPlayerList
(** The abstract type of values representing the game state. *)
type state 

(** [new_round lst] is the state of the game at the start of a new round. 
    In that state the current list of active players is [lst], the table is 
    an empty deck, and the betting pool is 0.
    Requires: length of [lst] is greater than or equal to 2 and
    less than or equal to 10*)
val new_round : player list -> state

(** [active_players st] is the list of players that are still playing 
    (ie not out or folded) in state [st]. *)
val active_players : state -> player list

(**[remove_active_player st p] is state [st] with active_players without
   player [p]. *)
val remove_active_player : state -> player -> state

(** [table st] is the deck currently on the table in state [st]. *)
val table : state -> deck

(**[change_table st d] is state [st] with table [d]. *)
val change_table : state -> deck -> state

(** [betting_pool st] is the current betting pool in state [st]. *)
val betting_pool : state -> int

(**[change_betting_pool st m] is state [st] with [m] added to the betting_pool 
   currently in state [st]. *)
val change_betting_pool : state -> int -> state