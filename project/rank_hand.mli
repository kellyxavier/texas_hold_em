open Deck

(** [hand_value hand] is the value of a given [hand] based on the cards
    contained, and standard poker scoring. Scores are int values ranging from
    1 - (20 * 13)
    "Requires" (not sure if actually necessary): exactly 7 cards in a hand *)
val hand_value : deck -> int
