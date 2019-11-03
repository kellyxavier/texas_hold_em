open Deck

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
  val hand : player -> deck
  val change_hand : player -> deck -> player
  val money : player -> int
  val change_money : player -> int -> player
  val status : player -> s
  val change_status : player -> s -> player
end

module PlayerCheck : PlayerSig = Player
