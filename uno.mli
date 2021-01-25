(** 
   Representation of static adventure data.

   This module represents the data stored in adventure files, including
   the rooms and exits.  It handles loading of that data from JSON as well
   as querying the data.

   It is build based on release code of assignment 2. 
*)

(* You are free to add more code here. *)

(**********************************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will use to test your 
 * submission.
*)

(** The abstract type of values representing an UNO game *)
type t

(** The type of color. *)
type color = string

(** The type of number. *)
type number = string

(** The type of card identifier. *)
type nc_id = string

(** Raised when an unknown color is encountered. *)
exception UnknownColor of color

(** Raised when an unknown number is encountered. *)
exception UnknownNumber of number

(** Raised when an unknown id is encountered. *)
exception UnknownNCID of nc_id

(** [from_json j] is the uno game that [j] represents.
    Requires: [j] is a valid JSON uno representation. *)
val from_json : Yojson.Basic.t -> t

(** [cards_of color uno c] is a set-like list of all of the number cards of color [c]. 
    Raises [UnknownColor c] if [c] is not a valid color in [uno]*)
val cards_of_color : t -> color -> nc_id list

(** [cards_of color uno n] is a set-like list of all of the number cards of number [n]. 
    Raises [UnknownNumber n] if [n] is not a valid number in [uno] *)
val cards_of_number : t -> number -> nc_id list

(** [nc_id_lst uno]return a list of all of the number card identifiers from [uno]*)
val nc_id_lst: t-> nc_id list 

(** [get_color uno id] is the color of the number card with card_id [id]. 
    Raises [UnknownNCID id] if [id] is not a room identifier in [uno]. *)
val get_color : t -> nc_id -> color

(** [get_number uno id] is the number of the number card with card_id [id]. 
    Raises [UnknownNCID id] if [id] is not a room identifier in [uno]. *)
val get_number: t -> nc_id -> number

(** [get_id uno c n] is the first element in the number card ids list 
    associated with color [c] and number[n]. 
    Raises [Failure "hd"] if there is no such number card. *)
val get_id: t -> color -> number -> nc_id






(* END DO NOT CHANGE
 **********************************************************************)

(* You are free to add more code here. *)
