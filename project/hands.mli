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


val tpair_low : deck
val tpair_high : deck
val tpair_ace : deck

(*================THREE OF A KIND HANDS================*)

val toak_table : deck
val toak_low : deck
val toak_high : deck
val toak_ace : deck

(*================STRAIGHT HANDS================*)

val stra_table : deck
val stra_low : deck
val stra_high : deck
val stra_ace : deck

(*================FLUSH HANDS================*)

val flush_table : deck
val flush_low : deck
val flush_high : deck
val flush_ace : deck

(*================FULL HOUSE HANDS================*)

val fh_low : deck
val fh_high : deck
val fh_ace : deck

(*================FOUR OF A KIND HANDS================*)

val foak_low : deck
val foak_high : deck
val foak_ace : deck

(*================STRAIGHT FLUSH HANDS================*)

val sf_low : deck
val sf_high : deck

(*================ROYAL FLUSH HANDS================*)

val royal_flush : deck

