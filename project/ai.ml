open Deck
open State
open Rank_hand

let make_easy_move st info =
  if info.wallet - (info.c_bet - info.m_betted) > 0 then "call" else "fold"

(** [check_worth_med info rank] is the command the medium AI will choose based
    on the ranking it gave to the point value of its hand [rank] and the current
    betting information of the game given by [info] *)
let check_worth_med info rank =
  let max_risk = info.m_bet * rank / 5 in
  if info.c_bet = 50 && info.m_betted < 50 then "call" else
  if info.c_bet < max_risk then 
    "raise " ^ (max_risk - info.c_bet |> string_of_int) else
  if info.m_betted = info.c_bet then "call" else "fold"

let make_med_move st info = 
  Random.self_init ();
  let points = hand_value (add info.h_cards info.t_cards) in
  let offset = (Random.int 300) - 150 in 
  if points >= 327421 + offset then 
    "raise " ^ (info.m_bet - info.c_bet |> string_of_int) else
  if points >= 327063 + offset then 
    check_worth_med info 4 else
  if points >= 327045 + offset then
    check_worth_med info 3 else
  if points >= 322260 + offset then
    check_worth_med info 2 else
  if points >= 288000 + offset then
    check_worth_med info 1
  else
    check_worth_med info 0

(** [check_worth_hard info rank] is the command the hard AI will choose based
    on the ranking it gave to the point value of its hand [rank] and the current
    betting information of the game given by [info] *)
let check_worth_hard info rank = 
  Random.self_init ();
  let max_risk = info.m_bet * rank / 10 in
  if info.c_bet = 50 && info.m_betted < 50 then "call" else
  if info.c_bet < max_risk then
    if Random.bool () then "raise " ^ (max_risk - info.c_bet |> string_of_int)
    else "call" else
  if info.m_betted = info.c_bet then "call" else 
  if Random.bool () then "call" else "fold"

let make_hard_move st info =
  Random.self_init ();
  let points = hand_value (add info.h_cards info.t_cards) in
  let base_points = hand_value info.t_cards in
  let good_hand = if points > base_points then -1 else 0 in
  let tier_adjust = if info.m_betted > 50 then good_hand + 1 else good_hand in
  if points = 327430 then
    if Random.bool () then
      "raise " ^ (info.m_bet - info.c_bet |> string_of_int) else "call"
  else if points >= 327421 then
    check_worth_hard info (9 + tier_adjust)
  else if points >= 327242 then
    check_worth_hard info (8 + tier_adjust)
  else if points >= 327063 then
    check_worth_hard info (7 + tier_adjust)
  else if points >= 327054 then
    check_worth_hard info (6 + tier_adjust)
  else if points >= 327045 then
    check_worth_hard info (5 + tier_adjust)
  else if points >= 324569 then
    check_worth_hard info (4 + tier_adjust)
  else if points >= 322260 then
    check_worth_hard info (3 + tier_adjust)
  else if points >= 288000 then
    check_worth_hard info (2 + tier_adjust)
  else if points >= 139726 then
    check_worth_hard info (1 + tier_adjust)
  else 
    check_worth_hard info 0