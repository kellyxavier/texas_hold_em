(** 
   Representation of dynamic game state.
   This module represents the state of a game as it is being played,
   including the game's current list of players, the deck on the table,
   the betting pool, and functions that cause the state to change.
*)

open Deck
open Player
open Command

(** [InvalidPlayerList] raises when the client tries to start a game with 
      less than 2 or more than 10 players. *)
exception InvalidPlayerList

(** [InvalidBet] raises when a player tries to make an illegal betting action *)
exception InvalidBet

(** The abstract type of values representing the game state. *)
type state 

(** The record of values applicable for a betting player's decision
    making. *)
type info =
  {
    wallet : int;
    o_wallets : (string * int) list;
    m_bet : int;
    c_bet : int;
    m_betted : int;
    b_pool : int;
    old_moves : (string * command) list;
    h_cards : deck;
    t_cards : deck;
  }

(** [new_round lst] is the state of the game at the start of a new round. 
    In that state the current list of active players is [lst], the table is 
    an empty deck, and the betting pool is 0.
    Requires: length of [lst] is greater than or equal to 2 and
    less than or equal to 10*)
val new_round : player list -> state

(** [all_players st] is the list of all players in state [st]. *)
val all_players : state -> player list

(** [change_all_players st lst] is the [st] with all players set to [lst].*)
val change_all_players : state -> player list -> state

(** [active_players st] is the list of players that are still playing 
    (ie not out or folded) in state [st]. *)
val active_players : state -> player list

(** [change_active_players st lst] is the [st] with active players set to 
    [lst].*)
val change_active_players : state -> player list -> state

(**[remove_active_player st p] is state [st] with active_players without
   player [p] and all_players with the player with the same name as [p] replaced
   by [p]. *)
val remove_active_player : state -> player -> state

(**[remove__all_active_players st act_players] is state [st] with no 
   active_players and all_players replaced by [act_players]. *)
val remove_all_active_players: state -> player list -> state 

(** [table st] is the deck currently on the table in state [st]. *)
val table : state -> deck

(**[change_table st d] is state [st] with table [d]. *)
val change_table : state -> deck -> state

(** [betting_pool st] is the current betting pool in state [st]. *)
val betting_pool : state -> int

(**[change_betting_pool st m] is state [st] with [m] added to the betting_pool 
   currently in state [st]. *)
val change_betting_pool : state -> int -> state

(** [current_bet st] is the current bet in state [st]. *)
val current_bet : state -> int

(**[change_current_bet st m] is state [st] with [m] added to the current_bet 
   currently in state [st]. *)
val change_current_bet : state -> int -> state

(** [max_bet st] is the current max bet in state [st]. *)
val max_bet : state -> int

(**[change_max_bet st m] is state [st] with max_bet set to [m]*)
val change_max_bet : state -> int -> state

(** [find_max_bet st] is the amout of money the player with least amount of 
    money currently has.*)
val find_max_bet : state -> int

(** [rem_deck st] is the current remaining deck in [st]. *)
val rem_deck : state -> deck

(** [change_rem_deck st] is [st] with rem_deck set to [rd]. *)
val change_rem_deck : deck -> state -> state

(** [update_player_money lst p m move acc] is the list of players [lst] with the 
    money of [p] increased by [m] and the last move of [p] changed to [move]*)
val update_player_money : player list -> player -> int -> command -> player list 
  -> player list

(**[quit st p] is state [st] with active_players without player [p]. *)
val quit : state -> player -> state

(**[fold st p] is state [st] with active_players without player [p]. *)
val fold : state -> player -> state

(**[check st p] is state [st] if the player [p]'s money betted is the same as 
   the current bet.
   Raises: [InvalidCheck] if not. *)
val check : state -> player -> state

(**[call st p] is state [st] with active players changed to reflect the change 
   in player [p]'s money (ie decreases the player's money by the difference 
   between the amount of money the player has previously betted and the
   current bet in state [st]) and the betting pool increased by the difference 
   between the amount of money the player has previously betted and the
   current bet in state [st]. *)
val call : state -> player -> state

(**[raise r st p] is state [st] with active players changed to reflect the 
   change in player [p]'s money (ie decreases the player's money by the 
   difference between the amount of money the player has previously betted and 
   the current bet in state [st], plus [r]) and the betting pool increased by 
   same difference plus r.
   Raises [InvalidBet] if the current bet in [st] + [r] is greater than the 
   max bet in [st]. *)
val raise : int -> state -> player -> state

(**[all_in st p] is state [st] with active players changed to reflect the change 
   in player [p]'s money (ie sets [p]'s money to 0) and the betting pool 
   increased by the total amount of money [p] previously had. 
   Raises: [InvalidBet] is [p]'s money and money betted is greated than the
      maximum bet in [st]. *)
val all_in : state -> player -> state

(** [only_one_player st] is true if there's more than one active player
    in the game and false otherwise. *)
val only_one_player : state -> bool

(** [get_info st p] is info for [p] to help [p] make a better betting 
    decision in [st]. *)
val get_info : state -> player -> info
