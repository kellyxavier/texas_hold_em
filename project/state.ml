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
    max_bet : int;
    rem_deck : deck
  }
(**[deal_players d lst acc] is [lst] with the hand of each player in [lst] set
   to a deck of 2 cards dealt from [d]*) 
let rec deal_players d lst acc=
  match lst with 
  | [] -> (List.rev acc, d)
  | h::t -> let cards_deck_pair = draw_card 2 d in 
    let cards = fst cards_deck_pair in let rem_deck = snd cards_deck_pair in 
    deal_players rem_deck t ((change_hand h cards |> reset_money_betted ):: acc)

let rec min_money min players =
  match players with 
  | [] -> min
  | h :: t -> if money h < min then min_money (money h) t else min_money min t


let new_round lst=
  if List.length lst < 2  || List.length lst > 10 then raise InvalidPlayerList 
  else 
    let deal = deal_players (shuffle ()) lst [] in
    {all_players = lst; active_players = fst deal; 
     table = empty; betting_pool = 0; current_bet = 0; max_bet = lst 
                                                                 |> min_money 
                                                                   max_int;
     rem_deck = snd deal}

let all_players st = 
  st.all_players

let change_all_players st lst =
  {st with all_players = lst}

let active_players st =
  st.active_players

let change_active_players st lst =
  {st with active_players = lst}

let rec remove_player lst p acc =
  match lst with
  | [] -> List.rev acc
  | h::t -> if name p = name h then remove_player t p acc 
    else remove_player t p (h::acc)

let rec change_player lst p acc =
  match lst with
  | [] -> List.rev acc 
  | h :: t -> if name h = name p then change_player t p (p :: acc)
    else change_player t p (h :: acc)

(* let rec update st p =
   match all_players st with
   | [] -> failwith "player not in all players list"
   | h :: t -> change_all_players st (change_player (all_players st) p [])

   let rec asdf st =
   match active_players st with
   | [] -> st
   | h :: t -> asdf (update st p) *)



let remove_active_player st p =
  let act_players_left = remove_player st.active_players p [] in
  let all_ps' = change_player st.all_players p [] in
  {st with active_players = act_players_left; all_players = all_ps'}

(* let act_players_left = remove_player st.active_players p [] in
   {st with active_players = act_players_left} *)

let rec remove_all_active_players st act_players =
  match act_players with
  | [] -> st 
  | h :: t -> remove_all_active_players (remove_active_player st h) t

let table st =
  st.table

(* let rec add_to_table_helper st d acc : deck=
   match d with
   | [] -> acc
   | h::t -> add_to_table_helper st t (h::acc) *)

let change_table st d =
  (* {st with table = add_to_table_helper st d st.table} *)
  {st with table = (add d st.table)}

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

let rem_deck st =
  st.rem_deck

let change_rem_deck rd st =
  {st with rem_deck = rd}

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