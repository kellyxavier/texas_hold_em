open Deck

type s =
  | Active
  | AllIn
  | Folded
  | Out

type b =
  | Big
  | Little
  | None

type player =
  {
    name: string;
    hand : deck;
    money : int;
    status : s;
    blind : b
  }

let create_player n = 
  {name = n; hand = empty; money = 5000; status = Active; blind = None}

let hand p =
  p.hand

let change_hand p h =
  {p with hand = h}

let money p =
  p.money

let change_money p m =
  {p with money = p.money + m}

let status p =
  p.status

let change_status p st =
  {p with status = st}