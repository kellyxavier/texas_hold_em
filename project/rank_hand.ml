include Deck

let debug = true  

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

(** [dec rank] decreases [rank] by one, cycles forward to end if necessary.
    Ex. A (1) decreases to K (13) *)
let dec rank =
  let rank' = rank - 1 in
  if rank' = 0 then 13 else rank'

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
let check_royal_straight suit = 
  let rec helper suit' =
    match suit' with
    | [] -> failwith "Should not happen"  
    | a :: b :: c :: d :: e :: t -> 
      if e = inc d && d = inc c && c = inc b && b = inc a then
        if e = 1 then 173000 else (* Royal Flush [173000] *)
          e + 172431 (* Straight Flush [172436-172444] ; Actual (5-13)*)
      else helper (b :: c :: d :: e :: t)
    | h :: t -> 
      ((List.fold_left max 0 (List.map dec suit)) + 170656) 
      (* Flush [170661-170669] ; Actual (5-13) *) in
  helper (append_first_four suit)

(** [check_flush suit] is the point value of a flush with the given cards of
    the same suit [suit]. If this is 0 then there is not a flush. *)
let check_flush suit =
  if List.length suit >= 5 then check_royal_straight suit else 0 

(** [check_foak ranks] is the point value of a four of a kind (FoaK)
    given the list of all ranks in a hand. If this is 0 then there is no FoaK *)
let rec check_foak ranks = 
  match ranks with
  | a :: b :: c :: d :: t -> 
    if a = b && b = c && c = d then 
      let highc = List.fold_left max 0 (List.filter (fun x -> x <> a) ranks) in
      (12 * (dec a)) + (dec highc) + 172263 (* FoaK [172277-172431]
                                               Actual (14-168) *)
    else check_foak (b :: c :: d :: t)
  | _ -> 0

(** [check_trio fh ranks] is the point value of a trio given the list of all
    ranks, however this becomes a helper function for full house
    when [fh] is true and returns only an intermediate point value 
    Requires: [ranks] must be a list of all ranks in ASCENDING order *)
let rec check_trio fh ranks =
  match List.rev ranks with 
  | a :: b :: c :: t -> 
    if a = b && b = c then begin
      if fh then (dec a) (* rank of trio part of full house *)
      else let highcrds = begin
          match List.filter (fun x -> x <> a) ranks with
          | x :: y :: t -> 12 * (dec x) + (dec y)
          | _ -> failwith "Shouldn't happen" end in
        ((dec a) * 133) + highcrds + 168759 (* Trio [168930-170643] ; 
                                               Actual (171-1884) *)
    end
    else check_trio fh (b :: c :: t)
  | _ -> 0

(** [check_pair fh ranks] is the point value of a pair fiven the list of all
    ranks, however this becomes a helper function for full house when [fh] is 
    true and returns only an intermediate point value
    Requires: [ranks] must be a list of all ranks in ASCENDING order *)
let rec check_pair fh ranks = 
  match List.rev ranks with
  | a :: b :: t ->  
    if a = b then begin
      if fh then dec a
      else let highcrds = begin
          match List.filter (fun x -> x <> a) ranks with 
          | x :: y :: z :: t -> 133 * (dec x) + 12 * (dec y) + (dec z)
          | _ -> failwith "Shouldn't happen" end in
        ((dec a) * 1464) + highcrds + 146104 (* Pair [149748-166875]
                                                Actual (3644-20771)*)
    end
    else check_pair fh (b :: t)
  | _ -> 0

(** [check_fh ranks] is the point value of a full house given the list of 
    all ranks. If this is 0 then there is no full house. *)
let check_fh ranks = 
  let trio = check_trio true ranks in
  if trio <> 0 then 
    let pair = check_pair true (List.filter (fun x -> x <> trio) ranks) in
    begin 
      if pair <> 0 then (12 * trio) + pair + 170669 (* FH [170683-172263] 
                                                       Actual (14-168)*)
      else 0 - trio
    end
  else 0

(** [check_straight ranks] is the point value of a straight given the list of
    all ranks. If this is 0 then there is no straight. *)
let check_straight ranks = 
  let rec helper ranks' = 
    match ranks' with
    | a :: b :: c :: d :: e :: t -> 
      if e = inc d && d = inc c && c = inc b && b = inc a then
        e + 170643 (* Straight [170648-170656] ; Actual (5-13) *)
      else helper (b :: c :: d :: e :: t)
    | _ -> 0 in
  helper (append_first_four ranks)

(** [check_twopair ranks] is the point value of two pairs given the list of all
    ranks. If this is 0 then there is no two pairs. *)
let check_twopair ranks =
  let pair1 = check_pair true ranks in
  if pair1 > 0 then 
    let pair2 = check_pair true (List.filter (fun x -> x <> pair1) ranks) in
    begin
      if pair2 > 0 then 
        let highcrd = List.fold_left max 0 
            (List.filter (fun x -> x <> pair1 && x <> pair2) ranks) in
        begin
          if pair1 > pair2 then (pair1 * 133) + (pair2 * 12) + highcrd else
            (* TwoPair [167156-168759] ; Actual (281-1884)] *)
            (pair2 * 133) + (pair1 * 12) + highcrd + 166875
        end
      else 0
    end
  else 0

let get_highcard ranks = 
  match List.rev ranks with
  | a :: b :: c :: d :: e :: t ->
    (* HighCard [1-146104 (120472-266575)] *)
    ((a * 19032) + (b * 1464) + (c * 133) + (d * 12) + e) - 120471
  | _ -> failwith "Something is terribly wrong"


let hand_value hand =
  let sort = sort_hand [] [] [] [] [] (to_list hand) in

  (* Checking for flushes *)
  let clubs_flush = check_flush sort.clubs in
  if clubs_flush > 0 then clubs_flush else
    let diamonds_flush = check_flush sort.diamonds in
    if diamonds_flush > 0 then diamonds_flush else
      let hearts_flush = check_flush sort.hearts in
      if hearts_flush > 0 then hearts_flush else
        let spades_flush = check_flush sort.spades in
        if spades_flush > 0 then spades_flush else

          (* Checking for FoaK *)
          let foak = check_foak sort.ranks in
          if foak > 0 then foak else

            (* Checking for Full House (and by extension finding out if trio exists) *)
            let fh = check_fh sort.ranks in
            if fh > 0 then fh else
              (* Rank of trio if trio exists (chance for optimization exists) *)
              (* let trio = if fh < 0 then 0 - fh else 0 in *)

              (* Checking for a straight *)
              let straight = check_straight sort.ranks in
              if straight > 0 then straight else

                (* Checking for a trio *)
                let trio = check_trio false sort.ranks in
                if trio > 0 then trio else

                  (* Checking for two pair *)
                  let twopair = check_twopair sort.ranks in
                  if twopair > 0 then twopair else

                    (* Get high card value *)
                    get_highcard (List.sort compare (List.map dec sort.ranks))
