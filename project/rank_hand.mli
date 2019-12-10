(**
    All of the functions used to determine the value of a hand (or partial hand)
*)

open Deck

(** [hand_value hand] is the value of a given [hand] based on the cards
    contained, and standard poker scoring. Scores are int values ranging from
    1 - (20 * 13) *)
val hand_value : deck -> int
