open Deck
open Player

exception InvalidPlayerList
exception InvalidBet

type state =
  {
    all_players : player list;
    active_players : player list;
    table : deck;
    betting_pool : int;
    current_bet : int;
    max_bet : int
  }

let rec deal_players d lst acc=
  match lst with 
  | [] -> List.rev acc
  | h::t -> let cards_deck_pair = draw_card 2 d in 
    let cards = fst cards_deck_pair in let rem_deck = snd cards_deck_pair in 
    deal_players rem_deck t (change_hand h cards :: acc)

let new_round lst=
  if List.length lst < 2  || List.length lst > 10 then raise InvalidPlayerList 
  else {all_players = lst; active_players = deal_players (shuffle ()) lst []; 
        table = empty; betting_pool = 0; current_bet = 0; max_bet = 5000}

let all_players st = 
  st.all_players

let change_all_players st lst =
  {st with all_players = lst}

let active_players st =
  st.active_players

let rec remove_player lst p acc =
  match lst with
  | [] -> List.rev acc
  | h::t -> if name p = name h then remove_player t p acc 
    else remove_player t p (h::acc)

let remove_active_player st p =
  let players_left = remove_player st.active_players p [] in 
  {st with active_players = players_left}

let table st =
  st.table

(* let rec add_to_table_helper st d acc : deck=
   match d with
   | [] -> acc
   | h::t -> add_to_table_helper st t (h::acc) *)

let change_table st d =
  (* {st with table = add_to_table_helper st d st.table} *)
  {st with table = d}

let betting_pool st =
  st.betting_pool

let change_betting_pool st m=
  {st with betting_pool = st.betting_pool + m}

let current_bet st =
  st.current_bet

let change_current_bet st m=
  {st with current_bet = st.current_bet + m}

let max_bet st =
  st.max_bet

let change_max_bet st m =
  {st with max_bet = m}

let rec find_max_bet_aux lst mb =
  match lst with 
  | [] -> mb
  | h :: t -> if money h < mb then find_max_bet_aux t (money h) 
    else find_max_bet_aux t mb

let find_max_bet st =
  find_max_bet_aux st.active_players 0

let rec update_player_money lst p m acc =
  match lst with 
  | [] -> List.rev acc
  | h :: t -> if name p = name h then let updated_player = change_money p m 
      in update_player_money t p m (updated_player :: acc)
    else update_player_money t p m (h :: acc)

let quit st p =
  let s' = remove_active_player st p in 
  {s' with all_players = remove_player (s'.all_players) p []}

let fold st p =
  remove_active_player st p

let check st p =
  if st.current_bet = money_betted p then st else raise InvalidBet

let call st p =
  let diff = (st.current_bet - money_betted p) 
  in let active_players' = update_player_money st.active_players p (-diff) []
  in {st with active_players = active_players'; 
              betting_pool = st.betting_pool + diff}

let all_in st p =
  if max_bet st >= money p + money_betted p
  then 
    let m = money p in
    let active_players' = update_player_money st.active_players p (-m) [] in  
    {st with active_players = active_players';
             betting_pool = st.betting_pool + m; 
             current_bet = st.current_bet + (st.current_bet - m)}
  else raise InvalidBet

let raise r st p =
  let m = (st.current_bet - money_betted p + r) in
  if max_bet st >=  st.current_bet + r
  then 
    let active_players' = update_player_money st.active_players p (-m) []
    in  {st with active_players = active_players';
                 betting_pool = st.betting_pool + m; 
                 current_bet = st.current_bet + r}
  else raise InvalidBet