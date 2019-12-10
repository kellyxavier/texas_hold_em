(** 
    Premade hands for use in the automated testing of Rank_hand
*)

open Deck

(*================PARTIAL HANDS (for AI)==========*)
(*================HIGH CARD HANDS=================*)
(** [hc_high] is the hand containing only a high card (King) *)
val hc_high : deck

(** [hc_ace] is the hand containing only an ace *)
val hc_ace : deck

(** [hc_low] is the hand containing only a low card (Two) *)
val hc_low : deck

(*===================PAIR HANDS===================*)
(** [low_pair] is the hand containing only a low pair (Pair of Twos) *)
val low_pair : deck

(** [high_pair] is the hand containing only a high pair (Pair of Kings) *)
val high_pair : deck

(** [ace_pair] is the hand containing only an ace pair *)
val ace_pair : deck

(*==================TRIO HANDS====================*)
(** [low_trio] is the hand containing only a low trio (Trio of Twos) *)
val low_trio : deck

(** [high_trio] is the hand containing only a high trio (Trio of Kings) *)
val high_trio : deck

(** [ace_trio] is the hand containing only an ace trio *)
val ace_trio : deck

(*================FULL HANDS BELOW================*)
(*================HIGH VALUE HANDS================*)
(** [hv_table] is the base table to test high value hands*)
val hv_table : deck

(** [hv_low] is the hand consisting of low cards (Ten and Eleven) added to 
    the base table*)
val hv_low : deck

(** [hv_high] is the hand consisting of high cards (Queen and King) added to 
    the base table*)
val hv_high : deck

(** [hv_ace] is the hand consisting of an ace and other card (Ace and Nine) 
    added to the base table*)
val hv_ace : deck

(*================PAIR HANDS================*)
(** [pair_table] is the base table to test pair hands*)
val pair_table : deck

(** [pair_low] is the hand consisting of an additional 2 (creating a pair)
    and an extra ten which is counted as a high card added to the base table. *)
val pair_low : deck

(** [pair_high] is the hand consisting of two kings (creating a pair)
    added to the base table. *)
val pair_high : deck

(** [pair_ace] is the hand consisting of an ace (creating a pair) and an
    additional 9 which is counted as a high card added to the base table. *)
val pair_ace : deck

(** [pair_equal_low] is the hand consisting of a 6 (creating a pair) and a low
    card (Four) added to the base table. *)
val pair_equal_low : deck

(** [pair_equal_high] is the hand consisting of a 6 (creating a pair) and a high
    card (King) added to the base table. *)
val pair_equal_high : deck

(*================TWO PAIR HANDS================*)
(** [tpair_table] is the base table to test two pair hands *)
val tpair_table : deck

(** [tpair_low] is the hand consisting of a 2 and a 3 added to the base table
    (creating two low pairs) *)
val tpair_low : deck

(** [tpair_high] is the hand consisting of an 8 and a 6 added to the base table
    (creating two high pairs) *)
val tpair_high : deck

(** [tpair_ace] is the hand consisting of an ace and a 2 added to the base table
    (creating two pairs one of which is an ace pair) *)
val tpair_ace : deck

(*================THREE OF A KIND HANDS================*)
(** [toak_table] is the base table to test three of a kind hands *)
val toak_table : deck

(** [toak_low] is the hand consisting of two 2's added to the base table
    (creating a three of a kind of twos) *)
val toak_low : deck

(** [toak_high] is the hand consisting of two 8's added to the base table
    (creating a three of a kind of eights) *)
val toak_high : deck

(** [toak_ace] is the hand consisting of two aces added to the base table
    (creating a three of a kind of aces) *)
val toak_ace : deck

(*================STRAIGHT HANDS================*)
(** [stra_table] is the base table to test straight hands *)
val stra_table : deck

(** [stra_low] is the hand consisting of a 4 and a 5 added to the base table, 
    resulting in the straight: 2, 3, 4, 5, 6 *)
val stra_low : deck

(** [stra_high] is the hand consisting of a 7 and an 8 added to the base table,
    resulting in the straight: 6, 7, 8, 9, 10 *)
val stra_high : deck

(** [stra_ace] is a completely manufactured hand that contains the straight:
    10, Jack, Queen, King, Ace  *)
val stra_ace : deck

(*================FLUSH HANDS================*)
(** [flush_table] is the base table to test flush hands *)
val flush_table : deck

(** [flush_low] is the hand consisting of two additional heart cards added to 
    the base table, resulting in a heart flush with a high card of a Queen *)
val flush_low : deck

(** [flush_high] is the hand consisting of two additional heart cards added to
    the base table, resulting in a heart flush with a high card of a King *)
val flush_high : deck

(** [flush_ace] is the hand consisting of two additional heart cards added to
    the base table, resulting in a heart flush with a high card of an Ace *)
val flush_ace : deck

(*================FULL HOUSE HANDS================*)
(** [fh_low] is a completely manufactured hand that contains a three of a kind
    of twos and a pair of threes *)
val fh_low : deck

(** [fh_high] is a completely manufactured hand that contains a three of a kind
    of Kings and a pair of tens *)
val fh_high : deck

(** [fh_ace] is a completely manufactured hand that contains a three of a kind
    of aces and a pair of eights *)
val fh_ace : deck

(*================FOUR OF A KIND HANDS================*)
(** [foak_low] is a completely manufactured hand that contains a four of a kind
    of twos *)
val foak_low : deck

(** [foak_high] is a completely manufactured hand that contains a four of a kind
    of kings *)
val foak_high : deck

(** [foak_ace] is a completely manufactured hand that contains a four of a kind
    of aces *)
val foak_ace : deck

(*================STRAIGHT FLUSH HANDS================*)
(** [sf_low] is a completely manufactured hand that contains a straight:
    2, 3, 4, 5, 6 where all cards are clubs *)
val sf_low : deck

(** [sf_high] is a completely manufactured hand that contains a straight:
    9, 10, Jack, Queen, King where all cards are clubs *)
val sf_high : deck

(*================ROYAL FLUSH HANDS================*)
(** [royal_flush] is a completely manufactured hand that contains a royal flush
    of spades *)
val royal_flush : deck

