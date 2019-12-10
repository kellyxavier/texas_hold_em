open Deck
open Player
open State
open Command
open Test_d
open Test_k
open Test_v

module type DeckSig = sig 
  exception EmptyDeck
  exception InvalidArgument
  type suit = Clubs | Diamonds | Hearts | Spades
  type card
  type deck
  val empty : deck
  val sorted_deck : deck
  val shuffle : unit -> deck
  val is_empty : deck -> bool
  val insert : suit -> int -> deck -> deck
  val draw_card : int -> deck -> deck * deck
  val add : deck -> deck -> deck
  val to_list : deck -> (suit * int) list
  val to_string : deck -> string
end

module DeckCheck : DeckSig = Deck

module type PlayerSig = sig
  type s =
    | Betting
    | AllIn
    | Folded
    | Out
  type b =
    | Big
    | Small
    | None
  type player
  val create_player : string -> player
  val create_ai_player : Command.difficulty -> player
  val name : player -> string
  val hand : player -> deck
  val change_hand : player -> deck-> player
  val money : player -> int
  val change_money : player -> int -> player
  val status : player -> s
  val change_status : player -> s -> player
  val blind : player -> b
  val change_blind : player -> b -> player
  val money_betted : player -> int
  val change_money_betted : player -> int -> player
  val reset_money_betted : player -> player
  val last_move : player -> command
  val change_last_move : player -> command -> player
  val reset_last_move : player -> player
  val is_ai : player -> bool
end

module PlayerCheck : PlayerSig = Player

module type StateSig = sig 
  exception InvalidPlayerList
  exception InvalidBet
  type state 
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
  val new_round : player list -> state
  val all_players : state -> player list
  val change_all_players : state -> player list -> state
  val active_players : state -> player list
  val change_active_players : state -> player list -> state
  val remove_active_player : state -> player -> state
  val remove_all_active_players: state -> player list -> state 
  val table : state -> deck
  val change_table : state -> deck -> state
  val betting_pool : state -> int
  val change_betting_pool : state -> int -> state
  val current_bet : state -> int
  val change_current_bet : state -> int -> state
  val max_bet : state -> int
  val change_max_bet : state -> int -> state
  val find_max_bet : state -> int
  val rem_deck : state -> deck
  val change_rem_deck : deck -> state -> state
  val update_player_money : player list -> player -> int -> command -> 
    player list -> player list
  val quit : state -> player -> state
  val fold : state -> player -> state
  val check : state -> player -> state
  val call : state -> player -> state
  val raise : int -> state -> player -> state
  val all_in : state -> player -> state
  val get_info : state -> player -> info
end

module StateCheck : StateSig = State

module type CommandSig = sig 
  type amount = int
  type command = 
    | Default
    | Quit
    | Continue
    | Fold
    | Call
    | Check
    | Allin
    | Raise of amount
  exception Empty
  exception Malformed
  val parse : string -> command
  val move_to_string : command -> string
end

module CommandCheck : CommandSig = Command

module type HandsSig = sig 
  val hv_table : deck
  val hv_low : deck
  val hv_high : deck
  val hv_ace : deck
  val pair_table : deck
  val pair_low : deck
  val pair_high : deck
  val pair_ace : deck
  val pair_equal_low : deck
  val pair_equal_high : deck
  val tpair_table : deck
  val tpair_low : deck
  val tpair_high : deck
  val tpair_ace : deck
  val toak_table : deck
  val toak_low : deck
  val toak_high : deck
  val toak_ace : deck
  val stra_table : deck
  val stra_low : deck
  val stra_high : deck
  val stra_ace : deck
  val flush_table : deck
  val flush_low : deck
  val flush_high : deck
  val flush_ace : deck
  val fh_low : deck
  val fh_high : deck
  val fh_ace : deck
  val foak_low : deck
  val foak_high : deck
  val foak_ace : deck
  val sf_low : deck
  val sf_high : deck
  val royal_flush : deck
end

module HandsCheck : HandsSig = Hands

module type RankHandSig = sig 
  val hand_value : deck -> int
end

module RankHandCheck : RankHandSig = Rank_hand

module type AiSig = sig 
  val make_easy_move : state -> info -> string
  val make_med_move : state -> info -> string
  val make_hard_move : state -> info -> string
end

module AiCheck : AiSig = Ai

module type TestSig = sig 
  val tests : OUnit2.test list
end

module TestDCheck : TestSig = Test_d

module TestKCheck : TestSig = Test_k

module TestVCheck : TestSig = Test_v