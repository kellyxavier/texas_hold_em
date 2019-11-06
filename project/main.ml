open Deck
open Player
open State
open Hands
open Rank_hand

(** [create_player_list x acc] is the player list consisting of [x] players*)
let rec create_player_list x acc =
  if x = 0 then List.rev acc
  else (print_endline ("Player "^(string_of_int((List.length acc)+1))^ ": enter your name");
        print_string "> ";
        match read_line () with
        | n -> create_player_list (x-1) (create_player n::acc))

(** [show_info_helper p] prints the private information of player [p]*)
let show_info_helper p =
  let money = money p in let hand = hand p |> to_string in 
  print_endline ("You have " ^ (string_of_int money) ^ " dollars and your cards are\n" ^ hand)

(** [show_info players] prints the private information of each players to 
    the terminal, clearing the terminal between each player*)
let rec show_info players =
  match players with
  | [] -> ()
  | h::t -> print_endline ((name h)^", press enter when you are alone");
    print_string "> ";
    match read_line () with 
    | _ -> show_info_helper h; print_endline 
        ("Press enter when you are done viewing your private info");
      print_string "> ";
      begin match read_line () with 
        |_ -> ANSITerminal.erase Screen; show_info t end

let comparing_hands_demo unit =
  print_endline "Now, let's compare some hands to see who wins!";
  print_endline "Let's start off by comparing ";
  (*First should win*)
  print_endline (to_string hv_ace);
  print_endline (to_string hv_high);
  if (hand_value hv_ace > hand_value hv_high)
  then print_endline "First hand wins\n\n"
  else (print_endline "Second hand wins\n\n");

  (*Second should win*)
  print_endline (to_string pair_ace);
  print_endline (to_string tpair_low);
  if (hand_value pair_ace > hand_value tpair_low)
  then print_endline "First hand wins\n\n"
  else (print_endline "Second hand wins\n\n");

  (*First should win*)
  print_endline (to_string foak_high);
  print_endline (to_string foak_low);
  if (hand_value foak_high > hand_value foak_low)
  then print_endline "First hand wins\n\n"
  else (print_endline "Second hand wins\n\n");

  (*Second should win*)
  print_endline (to_string toak_ace);
  print_endline (to_string sf_low);
  if (hand_value toak_ace > hand_value sf_low)
  then print_endline "First hand wins\n\n"
  else (print_endline "Second hand wins\n\n");

  (*Second should win*)
  print_endline (to_string toak_ace);
  print_endline (to_string royal_flush);
  if (hand_value toak_ace > hand_value royal_flush)
  then print_endline "First hand wins\n\n"
  else (print_endline "Second hand wins\n\n");
  print_endline "\n\nThanks for playing our demo! See ya next week!"

(** [start_game x] initializes a game with [x] players*)
let start_game x =
  let player_list = create_player_list x [] in
  let st = new_round player_list in let active_players = active_players st in 
  show_info active_players; print_endline "That's all for the demo game folks!";
  comparing_hands_demo ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  (print_string
     "\n\nWelcome to Texas Hold 'Em.\n");
  print_endline "Please enter the number of players in your game.
Note there may only be 2-10 players in your game";
  print_string  "> ";
  match int_of_string(read_line ()) with
  | exception e -> print_endline "You must enter a number. 
  Start the game engine again to play"
  | x -> if (x > 10 || x <2 ) then print_endline "You must enter a number between 1 and 10.
  Start the game engine again to play" else start_game x

let () = main ()


