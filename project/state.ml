open Deck
open Player

exception InvalidPlayerList
type state =
  {
    active_players : player list;
    table : deck;
    betting_pool : int
  }

let rec deal_players d lst acc=
  match lst with 
  | [] -> List.rev acc
  | h::t -> let cards_deck_pair = draw_card 2 d in 
    let cards = fst cards_deck_pair in let rem_deck = snd cards_deck_pair in 
    deal_players rem_deck t (change_hand h cards :: acc)

let new_round lst=
  if List.length lst < 2  || List.length lst > 10 then raise InvalidPlayerList 
  else {active_players = deal_players shuffle lst []; table = empty; 
        betting_pool = 0}

let active_players st =
  st.active_players

let rec remove_player lst p acc =
  match lst with
  | [] -> List.rev acc
  | h::t -> if p = h then remove_player t p acc 
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
