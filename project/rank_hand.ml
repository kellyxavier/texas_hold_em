include Deck

(** Type of a sorted hand (used for finding value of a hand) *)
type sorted_hand = {
  clubs : int list;
  diamonds : int list;
  hearts : int list;
  spades : int list;
  ranks : int list;

}

(** [inc rank] increments [rank] by one, cycles back to start if necessary.
    Ex. K (13) increments to A (1) *)
let inc rank = 
  let rank' = rank + 1 in
  if rank' = 14 then 1 else rank'

(** [sort_hand c d h s r hand] sorts the given [hand] into a [sorted_hand] *)
let rec sort_hand c d h s r = function
  | [] -> {
      clubs = (List.sort compare c); 
      diamonds = (List.sort compare d); 
      hearts = (List.sort compare h); 
      spades = (List.sort compare s); 
      ranks = (List.sort compare r)
    }
  | (suit, rank) :: t -> begin
      match suit with
      | Clubs -> sort_hand (rank :: c) d h s (rank :: r) t
      | Diamonds -> sort_hand c (rank :: d) h s (rank :: r) t
      | Hearts -> sort_hand c d (rank :: h) s (rank :: r) t
      | Spades -> sort_hand c d h (rank :: s) (rank :: r) t
    end

(** [append_first_four suit] the list of ranks of a suit with the first four
    ranks appended to the end of the list again
    Allows for checking of cycled straights.  *)
let append_first_four = function
  | (a :: b :: c :: d :: t) as lst -> lst @ (a :: b :: c :: d :: [])
  | _ -> failwith "Not allowed"

(** [check_royal_straight suit] is the value of the given flush *)
let rec check_royal_straight suit = 
  match suit with
  | [] -> failwith "Should not happen"  
  | a :: b :: c :: d :: e :: t -> 
    if e = inc d && d = inc c && c = inc b && b = inc a then
      if e = 1 then 50 * 10 else (* Royal Flush [500] *)
        e + (50 * 9) (* Straight Flush [455-463] *)
    else check_royal_straight (b :: c :: d :: e :: t)
  | h :: t -> (List.fold_left max 0 suit) + (50 * 5) (* Flush [255-263] *)

(** [check_flush suit] is the point value of a flush with the given cards of
    the same suit [suit]. If this is 0 then there is not a flush. *)
let check_flush suit =
  if List.length suit >= 5 then check_royal_straight suit else 0 

let rec check_foak highcard = function
  | a :: b :: c :: d :: t -> 
    if a = b && b = c && c = d then 
      (2 * a) + (50 * 8) + highcard (* FoaK [403-438] *)
    else check_foak highcard (b :: c :: d :: t)
  | _ -> 0

let hand_value hand =
  let sort = sort_hand [] [] [] [] [] hand in

  (* Checking for flushes *)
  if check_flush sort.clubs > 0 then check_flush sort.clubs else
  if check_flush sort.diamonds > 0 then check_flush sort.diamonds else
  if check_flush sort.hearts > 0 then check_flush sort.hearts else
  if check_flush sort.spades > 0 then check_flush sort.spades else

    failwith "Unimplemented"