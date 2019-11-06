open Deck
open Player

module type DeckSig = sig 
  type suit
  type card
  type deck
  val empty : deck
  val shuffle : deck
  val is_empty : deck -> bool
  val draw_card : int -> deck -> deck * deck
end

module DeckCheck : DeckSig = Deck

module type PlayerSig = sig
  type s
  type b
  type player
  val create_player : string -> player
  val name : player -> string
  val hand : player -> deck
  val change_hand : player -> deck -> player
  val money : player -> int
  val change_money : player -> int -> player
  val status : player -> s
  val change_status : player -> s -> player
  val blind : player -> b
  val change_blind : player -> b -> player
end

module PlayerCheck : PlayerSig = Player

module type StateSig = sig 
  exception InvalidPlayerList
  type state
  val new_round : player list -> state
  val active_players : state -> player list
  val remove_active_player : state -> player -> state
  val table : state -> deck
  val change_table : state -> deck -> state
  val betting_pool : state -> int
  val change_betting_pool : state -> int -> state
end

module StateCheck : StateSig = State