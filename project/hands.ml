open Deck

(*================HIGH VALUE HANDS================*)

let hv_table =
  empty 
  |> insert Clubs 2
  |> insert Clubs 3
  |> insert Diamonds 5
  |> insert Spades 6
  |> insert Hearts 8

let hv_low =
  hv_table
  |> insert Spades 10
  |> insert Spades 11

let hv_high =
  hv_table
  |> insert Spades 12
  |> insert Spades 13

let hv_ace =
  hv_table
  |> insert Spades 9
  |> insert Spades 1

(*================PAIR HANDS================*)

let pair_table =
  empty 
  |> insert Clubs 2
  |> insert Clubs 3
  |> insert Diamonds 1
  |> insert Spades 6
  |> insert Hearts 8

let pair_low =
  pair_table
  |> insert Diamonds 2
  |> insert Hearts 10

let pair_high =
  pair_table
  |> insert Diamonds 13
  |> insert Clubs 13

let pair_ace =
  pair_table
  |> insert Spades 1
  |> insert Clubs 1

let pair_equal_low =
  pair_table
  |> insert Hearts 6
  |> insert Spades 2

let pair_equal_high =
  pair_table
  |> insert Spades 6
  |> insert Hearts 13

(*================TWO PAIR TESTS================*)

let tpair_table = pair_table

let tpair_low =
  tpair_table
  |> insert Diamonds 2
  |> insert Hearts 3

let tpair_high =
  tpair_table
  |> insert Diamonds 8
  |> insert Clubs 6

let tpair_ace =
  tpair_table
  |> insert Diamonds 1
  |> insert Spades 2

(*================FLUSH HANDS================*)

let flush_table =
  empty
  |> insert Hearts 2
  |> insert Hearts 3
  |> insert Hearts 6
  |> insert Clubs 7
  |> insert Clubs 10

let flush_low =
  flush_table
  |> insert Hearts 12
  |> insert Hearts 5

let flush_high =
  flush_table
  |> insert Hearts 13
  |> insert Hearts 11

let flush_ace =
  flush_table 
  |> insert Hearts 1
  |> insert Hearts 12

(*================ROYAL FLUSH HANDS================*)

let royal_flush =
  empty
  |> insert Spades 8
  |> insert Spades 9
  |> insert Spades 10
  |> insert Spades 11
  |> insert Spades 12
  |> insert Spades 13
  |> insert Spades 1
