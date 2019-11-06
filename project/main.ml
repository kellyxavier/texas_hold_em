open Deck
open Player
open State
(** [main ()] prompts for the game to play, then starts it. *)
let rec create_player_list x acc =
  if x = 0 then List.rev acc
  else (print_endline ("Player "^(string_of_int((List.length acc)+1))^ ": enter your name");
        print_string "> ";
        match read_line () with
        | n -> create_player_list (x-1) (create_player n::acc))

let rec print_list lst = match lst with
  | [] -> ""
  | h::t -> h^", "^print_list t

let show_info_helper p =
  let money = money p in let hand = hand p |> to_string in 
  print_endline ("You have " ^ (string_of_int money) ^ " dollars and your cards are\n" ^ hand)

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


let start_game x =
  let player_list = create_player_list x [] in
  let st = new_round player_list in let active_players = active_players st in 
  show_info active_players; print_endline "That's all folks!"

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


