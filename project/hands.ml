open Deck

let flush_table =
  empty
  |> insert Hearts 2
  |> insert Hearts 3
  |> insert Hearts 6
  |> insert Clubs 7
  |> insert Clubs 10

let flush_ace =
  flush_table 
  |> insert Hearts 1
  |> insert Hearts 12

let flush_king =
  flush_table
  |> insert Hearts 13
  |> insert Hearts 11

let flush_queen =
  flush_table
  |> insert Hearts 12
  |> insert Hearts 5
