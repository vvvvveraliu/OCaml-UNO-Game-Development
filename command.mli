(**
   Parsing of player commands.
   It is build based on release code of assignment 2 and assignment code of 
   A2 of Zili Zhou (zz263) and Ruitong Liu (rl699)..
*)

(* You are free to add more code here. *)

(**********************************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will use to test your 
 * submission.
*)

(** The type [object_phrase] represents the object phrase that can be part of a 
    player command.  Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original player command.  For example:
    - If the player command is ["use red one"], then the object phrase is 
      [["red"; "one"]].
    - If the player command is ["use     blue two"], then the object phrase is
      again [["blue"; "two"]]. 

    An [object_phrase] is not permitted to be the empty list. *)
type object_phrase = string list

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. 
    The object phrase contains the type of card used by the user, i.e. "skip", 
    "red two", etc. 
    [Take]: player takes a new card. 
    [Use]: player uses a legal card in hand.  
    [Uno]: players needs to call uno to notify other players 
    he/she only has 1 card. 
    [Quit]: exit game. *)
type command = 
  | Take 
  | Use of object_phrase
  | Uno 
  | Quit

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.
    Examples: 
    - [parse "    use   red   one  "] is [Use ["red"; "one"]]
    - [parse "uno"] is [Uno]. 
    - [parse "take"] is [Take]. 
    - [parse "quit"] is [Quit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is neither "take" nor "use" nor "uno",
    or if the verb is "uno" or "take" and there is a non-empty object phrase,
    or if the verb is "use" and there is an empty object phrase.*)
val parse : string -> command

(* END DO NOT CHANGE
 **********************************************************************)

(* You are free to add more code here. *)
