(**
   Parsing of player commands.
*)

(** The type [amount] represents the amount of money that can be part of a 
    player command. *)
type amount = int

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an int. *)
type command = 
  | Quit
  | Fold
  | Call
  | Check 
  | Bet of amount
  | Raise of amount
  | Allin

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

(* END DO NOT CHANGE
 **********************************************************************)

(* You are free to add more code here. *)