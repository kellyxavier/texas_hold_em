open Deck

type status =
  | Active
  | AllIn
  | Folded
  | Out

type hand = card list

type player =
  {
    name: string;
    h : hand;
    money : int;
    s : status
  }

let create_player n = 
  failwith "unimplemented"

let hand p =
  failwith "unimplemented"

let money p =
  failwith "unimplemented"

let change_money p m =
  failwith "unimplemented"

let status p =
  failwith "unimplemented"

let change_status p s =
  failwith "unimplmented"

let new_round p =
  failwith "unimplemented"