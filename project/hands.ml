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
  |> insert Clubs 9

let pair_equal_low =
  pair_table
  |> insert Hearts 3
  |> insert Spades 2

let pair_equal_high =
  pair_table
  |> insert Spades 6
  |> insert Hearts 13

(*================TWO PAIR HANDS================*)

let tpair_table = 
  empty 
  |> insert Clubs 2
  |> insert Clubs 3
  |> insert Diamonds 1
  |> insert Spades 6
  |> insert Hearts 8

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

(*================THREE OF A KIND HANDS================*)

let toak_table =
  empty 
  |> insert Clubs 2
  |> insert Clubs 3
  |> insert Diamonds 1
  |> insert Spades 6
  |> insert Hearts 8

let toak_low =
  toak_table
  |> insert Diamonds 2
  |> insert Hearts 2

let toak_high =
  toak_table
  |> insert Diamonds 8
  |> insert Spades 8

let toak_ace =
  toak_table
  |> insert Spades 1
  |> insert Hearts 1

(*================STRAIGHT HANDS================*)

let stra_table =
  empty 
  |> insert Clubs 2
  |> insert Clubs 3
  |> insert Diamonds 10
  |> insert Spades 6
  |> insert Hearts 8

let stra_low =
  stra_table
  |> insert Diamonds 4
  |> insert Hearts 5

let stra_high =
  stra_table
  |> insert Hearts 7
  |> insert Diamonds 9

let stra_ace =
  empty
  |> insert Clubs 10
  |> insert Diamonds 11
  |> insert Clubs 12
  |> insert Hearts 13
  |> insert Spades 1
  |> insert Diamonds 7
  |> insert Spades 5

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

(*================FULL HOUSE HANDS================*)

let fh_low =
  empty
  |> insert Clubs 2
  |> insert Diamonds 2
  |> insert Hearts 2
  |> insert Clubs 3
  |> insert Diamonds 3
  |> insert Hearts 5
  |> insert Clubs 9

let fh_high =
  empty
  |> insert Clubs 13
  |> insert Diamonds 13
  |> insert Hearts 13
  |> insert Clubs 10
  |> insert Diamonds 10
  |> insert Hearts 5
  |> insert Clubs 9

let fh_ace =
  empty
  |> insert Clubs 1
  |> insert Diamonds 1
  |> insert Hearts 1
  |> insert Clubs 8
  |> insert Diamonds 8
  |> insert Hearts 5
  |> insert Clubs 9

(*================FOUR OF A KIND HANDS================*)

let foak_low =
  empty
  |> insert Clubs 2
  |> insert Diamonds 2
  |> insert Hearts 2
  |> insert Spades 2
  |> insert Clubs 4
  |> insert Diamonds 6
  |> insert Hearts 8

let foak_high =
  empty
  |> insert Clubs 13
  |> insert Diamonds 13
  |> insert Hearts 13
  |> insert Spades 13
  |> insert Clubs 4
  |> insert Diamonds 6
  |> insert Hearts 8

let foak_ace =
  empty
  |> insert Clubs 1
  |> insert Diamonds 1
  |> insert Hearts 1
  |> insert Spades 1
  |> insert Clubs 4
  |> insert Diamonds 6
  |> insert Hearts 8

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


  |> insert Diamonds 13
  |> insert Clubs 13

let pair_ace =
  pair_table
  |> insert Spades 1
  |> insert Clubs 9

let pair_equal_low =
  pair_table
  |> insert Hearts 3
  |> insert Spades 2

let pair_equal_high =
  pair_table
  |> insert Spades 6
  |> insert Hearts 13


  |> insert Hearts 2

let toak_high =
  toak_table
  |> insert Diamonds 8
  |> insert Spades 8

let toak_ace =
  toak_table
  |> insert Spades 1
  |> insert Hearts 1


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
