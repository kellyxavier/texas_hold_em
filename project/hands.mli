open Deck

(*================HIGH VALUE HANDS================*)
(** [hv_table] is the table upon which we will test high value hands*)
val hv_table : deck
val hv_low : deck
val hv_high : deck
val hv_ace : deck

(*================PAIR HANDS================*)

val pair_table : deck
val pair_low : deck
val pair_high : deck
val pair_ace : deck
val pair_equal_low : deck
val pair_equal_high : deck

(*================TWO PAIR HANDS================*)

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

