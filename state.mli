(** 
   Representation of dynamic uno game state.

   This module represents the state of a uno game as it is being played,
   including the current card, the cards that have been taken,
   the cards left to be taken, and functions that cause the state to change.

   It is build based on release code of assignment 2 and assignment code
   of A2 of Zili Zhou (zz263). 
*)

(* You are free to add more code here. *)

(**********************************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will use to test your 
 * submission.
*)

(** The type [player_id] represents the player id. **)
type player_id 

(** The abstract type of values representing the game state. *)
type t 


(** [init_state uno] is the initial state of the game when playing uno [uno]. 
    In that state each player gets random 7 cards.  *)
val init_state : Uno.t -> int -> t

(** [current_card_id st] is the identifier of the card in which the player
    currently is played in state [st]. *)
val current_card_id : t -> string

(** [played_and_held st] is a list of the card ids that either have been 
    used by a player or is holding by a player in state [st]. New card id
    is added to this list if a player takes this new card.  *)
val played_and_held : t -> string list


(** [left st] is a list of the card ids that have not been taken in 
    state [st]. *)
val left : t -> string list

(** [dic st] is an association list of (nc_id * player_id) to keep track
    of the location of each cards. New (nc_id * player_id)element is added to 
    the list if the player with player id [player_id] takes a card with
    id [nc_id]*)
val dic: t -> (string * int) list

(** [current_player_id st] is the player id who is currently playing *)
val current_player_id: t-> int

(** [uno_bool st] is the bool value representing if the user needs to call
    uno. *)
val uno_bool : t -> bool


(** [player_lst st] is the ordered list of player numbers.   *)
val player_lst : t -> int list

(** [current_player_number dic acc] is the number of players in the game. 
    for [dic]. *)
val current_player_number: (string * int) list -> int -> int

(** The type representing the result of an attempted movement. *)
type result = Legal of t | Illegal

(** [get_cards id dic lst] is a list of card ids[nc_id], where these cards are 
    held by the player with player id [id] *)
val get_cards: int->(string * int) list -> string list -> string list


(** [take p st] is [r] if player numbered [p] attempts to take a new card
    in state [st]. If there are cards left, then [r] is [Legal st'], where
    in [st'] the player numbered [p] now holding one more card. Otherwise, 
    the result is [Illegal]. 
    Effects: none.  [take] is not permitted to do any printing. *)
val take :  t -> result

(** [use id p st] is [r] if player numbered [p] attempts to use card [p]
    in state [st]. If he does have [id] and [id] is a valid card to use in 
    state [p], then [r] is [Legal st'], where
    in [st'] the player numbered [p] no longer holding this card. Otherwise, 
    the result is [Illegal]. 
    Effects: none.  [use] is not permitted to do any printing. *)
(* val use : string -> int -> t -> Uno.t -> result *)
val use : t-> Uno.t -> string -> string ->result

(**[count_cards dic current_player acc] is the number of cards of the 
   player with id [current_player] *)
val count_cards: (string * int) list-> int -> int -> int

(** [uno p st] is [r] if player numbered [p] attempts to call uno
    in state [st]. If he has only one card ledf , then [r] is [Legal st'], where
    [st'] completely same as the original state [st]. Otherwise, 
    the result is [Illegal]. 
    Effects: none.  [uno] is not permitted to do any printing. *)
val uno :  t -> result

(* END DO NOT CHANGE
 **********************************************************************)

(* You are free to add more code here. *)
