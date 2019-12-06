open State

(** [make_easy_move st info] is the string representing the move an easy
    difficulty AI would make given the state [st] and information [info] *)
val make_easy_move : state -> info -> string

(** [make_med_move st info] is the string representing the move a medium
    difficulty AI would make given the state [st] and information [info] *)
val make_med_move : state -> info -> string

(** [make_hard_move st info] is the string representing the move a hard
    difficulty AI would make given the state [st] and information [info] *)
val make_hard_move : state -> info -> string
