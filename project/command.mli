(**
   Parsing of player commands.
*)

(** The type [amount] represents the amount of money that can be part of a 
    player command. *)
type amount = int

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an int. *)
type command = 
  | Default
  | Quit
  | Continue
  | Fold
  | Call
  | Check
  | Allin
  | Raise of amount

(** The type [difficulty] represents the difficulty of the AI desired. *)
type difficulty =
  | Easy
  | Med
  | Hard

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The following integer, if there is one, becomes the amount.
    Examples: 
    - [parse "    bet   20  "] is [Bet 20]
    - [parse "    raise   20  "] is [Raise 20]
    - [parse "fold"] is [Fold]. 
    - [parse "call"] is [Call]. 
    - [parse "check"] is [Check]. 
    - [parse "allin"] is [Allin]. 
    - [parse "quit"] is [Quit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is neither "quit", "fold", "call", "check",
    "bet", "raise", nor "allin", or if the verb is "quit", "fold", "call", or 
    "check" and there is any other words following the command,
    or if the verb is "go" and there is no amount following the command.*)
val parse : string -> command

(** [diff str] parses a player's input into a [difficulty]. It accepts easy,
    medium, or hard.
    Raises: [Empty] if [str] is the empty string or contains only spaces. 
    Raises: [Malformed] if the command is malformed. *)
val diff : string -> difficulty

(** [quit_error] is a message when the player tries to quit at the 
    inappropriate time. *)
val quit_error : string

(** [continue_error] is a message when the player tries to continue at the 
    inappropriate time. *)
val continue_error : string

(** [check_error] is a message when the player tries to check at the 
    inappropriate time. *)
val check_error : string

(** [raise_error] is a message when the player tries to raise when they 
    cannot raise so much money. *)
val raise_error : string

(** [empty_error] is a message when the player puts in an empty command. *)
val empty_error : string

(** [malformed_error] is a message when the player puts in a malformed
    command. *)
val malformed_error : string

(** [move_to_string m] returns m as a string in the past tense. Default is 
    "Has not made a move yet" *)
val move_to_string : command -> string
