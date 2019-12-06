open Deck

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
        if List.mem (inc e) suit' then helper (b :: c :: d :: e :: t) else
        if e = 1 then 327430 else (* Royal Flush [327430] *)
          e + 327416 (* Straight Flush [327421-327429] ; Actual (5-13)*)
      else helper (b :: c :: d :: e :: t)
    | h :: t -> 
      ((List.fold_left max 0 (List.map dec suit)) + 327049) 
      (* Flush [327054-327062] ; Actual (5-13) *) in
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
      let ranks' = List.map dec ranks in
      let highc = List.fold_left max 0 
          (List.filter (fun x -> x <> dec a) ranks') in
      (14 * dec a) + highc + 327226 (* FoaK [327242-327420]
                                               Actual (16-194) *)
    else check_foak (b :: c :: d :: t)
  | _ -> 0

(** [check_trio fh ranks] is the point value of a trio given the list of all
    ranks, however this becomes a helper function for full house
    when [fh] is true and returns only an intermediate point value 
    Requires: [ranks] must be a list of all ranks in ASCENDING order *)
let rec check_trio fh ranks =
  let rec helper fh ranks' =
    match ranks' with 
    | a :: b :: c :: t -> 
      if a = b && b = c then begin
        if fh then (dec a) (* rank of trio part of full house *)
        else 
          let ranks'' = List.map dec ranks in
          let highcrds = begin
            match (List.filter (fun x -> x <> dec a) ranks'') with
            | x :: y :: t -> 14 * x + y
            | _ -> failwith "Could not find kicker cards for trio" end in
          ((dec a) * 195) + highcrds + 324330 (* Trio [324569-327044] ; 
                                                 Actual (239-2714) *)
      end
      else helper fh (b :: c :: t)
    | _ -> 0 in
  helper fh (List.rev ranks)

(** [check_pair fh ranks] is the point value of a pair fiven the list of all
    ranks, however this becomes a helper function when [fh] is 
    true and returns only an intermediate point value 
    Requires: [ranks] must be a list of all ranks in ASCENDING order *)
let check_pair fh ranks = 
  let rec helper fh ranks' = 
    match ranks' with
    | a :: b :: t ->  
      if a = b then begin
        if fh then dec a
        else let highcrds = begin
            match List.filter (fun x -> x <> a) ranks |> List.map dec
                  |> List.sort compare |> List.rev with 
            | x :: y :: z :: t -> 195 * (dec x) + 14 * (dec y) + (dec z)
            | _ -> failwith "Could not find kicker cards for pair"  end in
          ((dec a) * 2715) + highcrds + 284461 (* Pair [288000-322259]
                                                  Actual (3539-37798)*)
      end
      else helper fh (b :: t)
    | _ -> 0 in
  helper fh (List.rev ranks)

(** [check_fh ranks] is the point value of a full house given the list of 
    all ranks. If this is 0 then there is no full house. *)
let check_fh ranks = 
  let trio = check_trio true ranks in
  if trio <> 0 then 
    let pair = check_pair true (List.filter (fun x -> dec x <> trio) ranks) in
    begin 
      if pair <> 0 then (14 * trio) + pair + 327047 (* FH [327063-327241] 
                                                       Actual (16-194)*)
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
        if List.mem (inc e) ranks' then helper (b :: c :: d :: e :: t) else
          (dec e) + 327045 (* Straight [327045-327053] ; Actual (5-13) *)
      else helper (b :: c :: d :: e :: t)
    | _ -> 0 in
  helper (append_first_four ranks)

(** [check_twopair ranks] is the point value of two pairs given the list of all
    ranks. If this is 0 then there is no two pairs. *)
let check_twopair ranks =
  let pair1 = check_pair true ranks in
  if pair1 > 0 then 
    let pair2 = check_pair true (List.filter (fun x -> dec x <> pair1) ranks) in
    begin
      if pair2 > 0 then 
        let ranks' = List.map dec ranks in
        let highcrd = List.fold_left max 0 
            (List.filter (fun x -> x <> pair1 && x <> pair2) ranks') in
        begin
          if pair1 > pair2 then (pair1 * 195) + (pair2 * 14) + highcrd + 321853
          else
            (* TwoPair [322260-324568] ; Actual (407-2714)] *)
            (pair2 * 195) + (pair1 * 14) + highcrd + 321853
        end
      else 0
    end
  else 0

(** [get_highcard ranks] is the point value of the highcards in a given list of
    all ranks. This accounts for kicker cards as well. *)
let get_highcard ranks = 
  match List.rev ranks with
  | a :: b :: c :: d :: e :: t ->
    (* HighCard [1-287999 (238274-526273)] *)
    ((a * 37800) + (b * 2715) + (c * 195) + (d * 14) + e) - 238274
  | _ -> failwith "Invalid hand"


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

            (* Checking for Full House 
               (and by extension finding out if trio exists) *)
            let fh = check_fh sort.ranks in
            if fh > 0 then fh else
              (* Rank of trio if trio exists (chance for optimization) *)
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

                    (* Checking for pairs *)
                    let pair = check_pair false sort.ranks in
                    if pair > 0 then pair else

                      (* Get high card value *)
                      get_highcard (List.sort compare (List.map dec sort.ranks))