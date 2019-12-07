open Deck
open Player
open Command

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

type info =
  {
    wallet : int;
    o_wallets : (string * int) list;
    m_bet : int;
    c_bet : int;
    m_betted : int;
    b_pool : int;
    old_moves : (string * command) list;
    h_cards : deck;
    t_cards : deck;
  }

(**[deal_players d lst acc] is [lst] with the hand of each player in [lst] set
   to a deck of 2 cards dealt from [d]*) 
let rec deal_players d lst acc=
  match lst with 
  | [] -> (List.rev acc, d)
  | h::t -> 
    begin
      let cards_deck_pair = draw_card 2 d in 
      let cards = fst cards_deck_pair in 
      let rem_deck = snd cards_deck_pair in 
      deal_players rem_deck t ((change_hand h cards 
                                |> reset_money_betted ):: acc)
    end

(**[min_money min players] is the minimum anount of money held by a player
   in [players]*) 
let rec min_money min players =
  match players with 
  | [] -> min
  | h :: t -> if money h < min then min_money (money h) t else min_money min t

let new_round lst=
  if List.length lst < 1  || List.length lst > 10 then raise InvalidPlayerList 
  else
    begin 
      let lst' = List.map (fun p -> reset_last_move p) lst in
      let deal = deal_players (shuffle ()) lst' [] in
      {all_players = lst'; active_players = fst deal; 
       table = empty; betting_pool = 0; current_bet = 0; 
       max_bet = lst' |> min_money max_int; rem_deck = snd deal}
    end

let all_players st = 
  st.all_players

let change_all_players st lst =
  {st with all_players = lst}

let active_players st =
  st.active_players

let change_active_players st lst =
  {st with active_players = lst}

(**[remove_player lst p acc] is [lst] without the player with same name as [p]*)
let rec remove_player lst p acc =
  match lst with
  | [] -> List.rev acc
  | h::t -> 
    begin
      if name p = name h then remove_player t p acc 
      else remove_player t p (h::acc)
    end

(**[change_player lst p acc] is [lst] with the player with same name as [p] 
   replaced by [p]*)
let rec change_player lst p acc =
  match lst with
  | [] -> List.rev acc 
  | h :: t -> 
    begin 
      if name h = name p then change_player t p (p :: acc)
      else change_player t p (h :: acc)
    end

let remove_active_player st p =
  let act_players_left = remove_player st.active_players p [] in
  let all_ps' = change_player st.all_players p [] in
  {st with active_players = act_players_left; all_players = all_ps'}

let rec remove_all_active_players st act_players =
  match act_players with
  | [] -> st 
  | h :: t -> remove_all_active_players (remove_active_player st h) t

let table st =
  st.table

let change_table st d =
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

(**[find_max_bet_aux lst mb] is a tail recursive implementation of 
   [find_max_bet st]*)
let rec find_max_bet_aux lst mb =
  match lst with 
  | [] -> mb
  | h :: t -> 
    begin
      if money h < mb then find_max_bet_aux t (money h) 
      else find_max_bet_aux t mb
    end

let find_max_bet st =
  find_max_bet_aux st.active_players 5000

let rem_deck st =
  st.rem_deck

let change_rem_deck rd st =
  {st with rem_deck = rd}

(**[update_player_last_move lst p acc] is [lst] with the player with same name 
   as [p] replaced by [p] with last_move set to [move]*)
let rec update_player_last_move lst p move acc =
  match lst with 
  | [] -> List.rev acc
  | h :: t -> 
    begin
      if name p = name h then 
        begin
          let updated_player = change_last_move p move in 
          update_player_last_move t p move (updated_player :: acc)
        end
      else update_player_last_move t p move (h :: acc)
    end

let rec update_player_money lst p m move acc =
  match lst with 
  | [] -> List.rev acc
  | h :: t -> 
    begin
      if name p = name h then 
        begin
          let updated_player = change_money p m in 
          let updated_player' = change_last_move updated_player move in
          update_player_money t p m move (updated_player' :: acc)
        end
      else update_player_money t p m move (h :: acc)
    end

let quit st p =
  let s' = remove_active_player st p in 
  let s'' = {s' with all_players = remove_player (s'.all_players) p []} in 
  let all_players' = update_player_last_move (all_players s'') p Quit [] in 
  {s'' with all_players = all_players'} 

let fold st p =
  let s' = remove_active_player st p in 
  let all_players' = update_player_last_move (s'.all_players) p Fold [] in
  {s' with all_players = all_players'} 

let check st p =
  if st.current_bet = money_betted p then
    begin 
      let active_players' = update_player_last_move (st.active_players) p 
          Check [] in 
      {st with active_players = active_players'} 
    end
  else raise InvalidBet

let call st p =
  let diff = (st.current_bet - money_betted p) in 
  let active_players' = update_player_money st.active_players p (-diff) 
      Call [] in 
  {st with active_players = active_players'; 
           betting_pool = st.betting_pool + diff}

let raise r st p =
  let m = (st.current_bet - money_betted p + r) in
  if max_bet st >=  st.current_bet + r && r >= 0
  then
    begin 
      let cmd = if m = money p then Allin else (Raise r) in
      let active_players' = update_player_money st.active_players p (-m) 
          cmd [] in  
      {st with active_players = active_players'; 
               betting_pool = st.betting_pool + m; 
               current_bet = st.current_bet + r}
    end
  else raise InvalidBet

let all_in st p =
  let r = money p - (st.current_bet - money_betted p) in raise r st p

(** [other_wallets lst n acc] is a list of tuples of a player in [lst] that is
    not [n] and how much money they have left. *)
let rec other_wallets lst n acc =
  match lst with  
  | [] -> List.rev acc
  | h :: t -> 
    begin
      if name h = n then other_wallets t n acc
      else other_wallets t n ((name h, money h) :: acc)
    end

(** [last_moves lst acc] is a list of tuples of a player in [lst] and their
    last move. Their last move is default if they haven't done anything yet. *)
let rec last_moves lst acc =
  match lst with
  | [] -> List.rev acc
  | h :: t -> last_moves t ((name h, last_move h) :: acc)

let rec string_wallets wallets acc =
  match wallets with 
  | [] -> acc
  | (n, m) :: t -> string_wallets t (acc ^ n ^ "'s walllet: " ^ string_of_int (m) 
                                     ^ " ")

let rec string_last_moves last_moves acc =
  match last_moves with
  | [] -> acc
  | (n, Default) :: t -> string_last_moves t (acc ^ n ^ "'s last move: Default")
  | (n, Fold) :: t -> string_last_moves t (acc ^ n ^ "'s last move: Fold")
  | (n, Check) :: t -> string_last_moves t (acc ^ n ^ "'s last move: Check")
  | (n, Call) :: t -> string_last_moves t (acc ^ n ^ "'s last move: Call")
  | (n, (Raise r)) :: t -> string_last_moves t (acc ^ n ^ "'s last move: Raise " ^ string_of_int r)
  | (n, Allin) :: t -> string_last_moves t (acc ^ n ^ "'s last move: Alllin ")
  | (n, Quit) :: t -> string_last_moves t (acc ^ n ^ "'s last move: Quit ")
  | (n, Continue) :: t -> string_last_moves t (acc ^ n ^ "'s last move: Continue ")

let get_info st p =
  (* print_endline ("m_bet: " ^ string_of_int (max_bet st));
     print_endline ("c_bet: " ^ string_of_int (current_bet st));
     print_endline ("m_betted: " ^ string_of_int (money_betted p));
     print_endline ("b_pool: " ^ string_of_int (betting_pool st));
     let l_moves = last_moves (active_players st) [] in
     print_endline ("old_moves: " ^ string_last_moves l_moves "");
     print_endline ("h_cards: " ^ to_string (hand p));
     print_endline("t_cards: " ^ to_string (table st)); *)
  {
    wallet = money p;
    o_wallets = other_wallets (active_players st) (name p) [];
    m_bet = max_bet st;
    c_bet = current_bet st;
    m_betted = money_betted p;
    b_pool = betting_pool st;
    old_moves = last_moves (active_players st) [];
    h_cards = hand p;
    t_cards = table st;
  }