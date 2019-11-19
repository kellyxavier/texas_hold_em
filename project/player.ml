open Deck

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
    money_betted : int
  }

let create_player n = 
  {name = n; hand = empty; money = 5000; status = Betting; blind = None;
   money_betted = 0}

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


