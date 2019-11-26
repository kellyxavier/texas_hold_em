open Deck
open Command

type s =
  | Betting
  | AllIn
  | Folded
  | Out

type b =
  | Big
  | Small
  | None

type player =
  {
    name: string;
    hand : deck;
    money : int;
    status : s;
    blind : b;
    money_betted : int;
    last_move : command;
    is_ai : bool
  }

let create_player n = 
  {name = n; hand = empty; money = 5000; status = Betting; blind = None;
   money_betted = 0; last_move = Default; is_ai = false}

let create_ai_player = 
  {name = "The AI"; hand = empty; money = 5000; status = Betting; blind = None;
   money_betted = 0; last_move = Default; is_ai = true}

let name p =
  p.name

let hand p =
  p.hand

let change_hand p h =
  {p with hand = h}

let money p =
  p.money

let change_money p m =
  {p with money = p.money + m; money_betted = p.money_betted - m}

let status p =
  p.status

let change_status p st =
  {p with status = st}

let blind p =
  p.blind

let change_blind p bl =
  {p with blind = bl}

let money_betted p =
  p.money_betted

let change_money_betted p mb =
  {p with money_betted = p.money_betted + mb}

let reset_money_betted p =
  {p with money_betted = 0}

let last_move p =
  p.last_move

let change_last_move p c =
  {p with last_move = c}

let reset_last_move p =
  {p with last_move = Default}

let is_ai p =
  p.is_ai