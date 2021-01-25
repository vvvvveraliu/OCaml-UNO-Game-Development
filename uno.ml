type color = string
exception UnknownColor of color

type number = string
exception UnknownNumber of number

type nc_id = string
exception UnknownNCID of nc_id

open Yojson.Basic.Util

(** The abstract type of values representing number cards *)
type nc = {
  number:number;
  color:color;
  nc_id:nc_id;
}

type t={
  number_cards: nc list;
}

(** [number_cards_from_json json] is the number card that [json] represents.
    Requires: [json] is a valid JSON uno representation. *)
let number_cards_from_json json = {
  number = json |> member "number" |> to_string;
  color = json |>member "color" |>to_string;
  nc_id = json |>member "nc_id" |>to_string;
}

let from_json json = {
  number_cards = json |> member "number cards" |> to_list |> 
                 List.map number_cards_from_json;
}

(** [helper_cards_of_color nc_lst c lst ]is a list of number cards ids from the
    list [nc_lst] associated with color [c] *)
let rec helper_cards_of_color nc_lst c lst= 
  match nc_lst with
  |[]-> lst
  |h::t -> if h.color=c then helper_cards_of_color t c (h.nc_id::lst) 
    else helper_cards_of_color t c lst

let cards_of_color uno c = 
  let nc_lst  = uno.number_cards in 
  let lst = [] in
  List.sort_uniq compare(helper_cards_of_color nc_lst c lst)

(** [helper_cards_of_number nc_lst n lst] is a list of number card ids 
    from the list [nc_lst] associated with number [n] *)
let rec helper_cards_of_number nc_lst n lst= 
  match nc_lst with
  |[]-> lst
  |h::t -> if h.number=n then helper_cards_of_number t n (h.nc_id::lst) 
    else helper_cards_of_number t n lst

let cards_of_number uno n = 
  let nc_lst  = uno.number_cards in 
  let lst = [] in
  List.sort_uniq compare (helper_cards_of_number nc_lst n lst)

let nc_id_lst uno = 
  let nc_lst = uno.number_cards in
  let f card = card.nc_id in
  List.sort_uniq compare (List.map f nc_lst)

(** [find_nc nc_lst nc_id] is the number card from the list [nc_lst]
    associated with the id [nc_id] *)
let rec find_nc nc_lst nc_id = 
  match nc_lst with 
  |[]-> raise (UnknownNCID nc_id)
  |h::t -> if (h.nc_id= nc_id) then h else find_nc t nc_id

let get_color uno id= 
  let nc_lst = uno.number_cards in
  let nc = find_nc nc_lst id in
  nc.color

let get_number uno id= 
  let nc_lst = uno.number_cards in
  let nc = find_nc nc_lst id in
  nc.number

(** [get_id_helper nc_lst n c lst] is a list of nc_id from the list [nc_lst]
    if the color of the card is [c] and the number of the card is [n]*)
let rec get_id_helper nc_lst n c lst = 
  match nc_lst with
  |[] -> lst
  |h::t -> if h.number = n && h.color = c 
    then get_id_helper t n c (h.nc_id ::lst)
    else get_id_helper t n c lst 

let get_id uno c n =
  let nc_lst = uno.number_cards in 
  let ids = List.sort_uniq compare(get_id_helper nc_lst n c [] )in
  List.hd ids



