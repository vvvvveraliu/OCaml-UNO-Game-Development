open Uno

type player_id = int

type t = {
  current_card: nc_id;
  cards_played_and_held: nc_id list;
  cards_left: nc_id list;
  dic:(nc_id * player_id) list;
  current_player: player_id;
  uno_bool: bool;
  player_list: int list;
}

(** [remove id id_list] is a helper function that remove the first [id] from
    [id_List].  Requires: [id_list] contains [id]. *)
let rec remove acc id id_list = 
  match id_list with
  | [] -> acc
  | a::b -> if a=id then acc @ b else remove (a::acc) id b 

(** [shuffle id_list] shuffles [id_list]. *)
let shuffle id_list =
  Random.self_init ();
  let rand_id_list = List.map (fun id -> (Random.bits (), id)) id_list in
  let sorted = List.sort compare rand_id_list in
  List.map snd sorted

(** [get ns ne list] is a reverse list of [ns]th element to [ne]th 
    element in [list]. The 0th elt is the first one. *)
let rec get ns ne list acc= 
  if ns = ne + 1 then acc 
  else List.nth list ns :: acc |> get (ns+1) ne list 

(** [random_one id_list] is a random id from [id_list].  
    Requires: [id_list] is not empty.*)
let random_one id_list = 
  Random.self_init ();
  List.length id_list |> Random.int |> List.nth id_list


(** [rep_ok id] is the id of the card [id] that has the same color 
    and same number with it. *)
let rep_ok id = 
  if String.length id = 2 then id
  else if id = "wc1" then "wc2"
  else if id = "wc2" then "wc3"
  else if id = "wc3" then "wc4"
  else if id = "wc4" then "wc1"
  else if id = "wd1" then "wd2"
  else if id = "wd2" then "wd3"
  else if id = "wd3" then "wd4"
  else if id = "wd4" then "wd1"
  else if String.get id 2= '1' then (String.sub id 0 2) ^ "2"
  else (String.sub id 0 2) ^ "1"


(** [get_tuple uno player_id id_lst] is an association list that consists of 
    element (nc_id * player_id), where nc_id are elements from the list 
    [id_lst] *)
let rec get_tuple uno player_id id_lst=
  match id_lst with
  |[] -> []
  |h::t ->  
    (h,player_id):: get_tuple uno player_id t


(** [get_id_lst uno player_id_lst id_lst acc acc2] is an association list 
    that consists of element (nc_id* player_id), where nc_id are elements from 
    [id_lst] and player_id are elements from [player_id_lst] *)
let rec get_id_lst uno player_id_lst id_lst acc acc2= 
  match player_id_lst with
  |[]-> acc2
  |h::t ->   let new_id_lst = get acc (acc+6) id_lst []  in 
    (get_tuple uno h new_id_lst)@ acc2 |>
    (get_id_lst uno t id_lst (acc+7))


(** [make_player_lst] is a reverse list that contains integers from n to 1
    For example, [make_player_lst] is a list [n, n-1, n-2, ..., 1] 
    Requires: n is a positive integer *)
let rec make_player_lst n = 
  match n with 
  |0 ->[]
  |h -> h:: (make_player_lst (n-1))

let init_state uno n =
  let rand_all_id = shuffle (nc_id_lst uno) in
  let player_id_lst = List.sort_uniq compare (make_player_lst n) in 
  let ini_try = List.nth rand_all_id 0 in 
  let ini = if String.get ini_try 0 = 'w' then "r0" else ini_try in 
  {
    current_card = ini;
    cards_played_and_held = get 1 (n*7) rand_all_id [];
    cards_left = get (n*7+1) (List.length rand_all_id -1) rand_all_id [];
    dic = get_id_lst uno player_id_lst rand_all_id 1 [];
    current_player=1;
    uno_bool = false;
    player_list = make_player_lst n|>List.rev;
  }

let current_card_id st = st.current_card

let played_and_held st = st.cards_played_and_held

let left st  = st.cards_left

let dic st = st.dic

let current_player_id st = st.current_player

let uno_bool st = st.uno_bool

let player_lst st = st.player_list

let rec current_player_number dic acc= 
  match dic with 
  | [] -> acc
  | (_, n )::d -> if n>acc then current_player_number d n
    else current_player_number d acc

let rec count_cards dic current_player acc= 
  match dic with 
  |[]-> acc
  |(a,b)::t -> if b=current_player then count_cards t current_player acc+1 
    else count_cards t current_player acc

(** [new_player st] is the id of next player in [st]. *)
let new_player st = 
  let nmax = current_player_number (dic st) 0 in
  let n = current_player_id st in
  if List.hd st.player_list = 1 then 
    (if n < nmax then n + 1 else 1)
  else (if n > 1 then n - 1 else nmax)

(** [new_player_s st] is the id of next player in [st] if a skip card is
    used.  *)
let new_player_s st = 
  let nmax = current_player_number (dic st) 0 in
  let n = current_player_id st in
  if List.hd st.player_list != 1 then 
    (if n < nmax then n + 1 else 1)
  else (if n > 1 then n - 1 else nmax)

(** [new_new_player st] is the id of the next of the next player in [st]. *)
let new_new_player st = 
  let nmax = current_player_number (dic st) 0 in
  let n = current_player_id st in
  if List.hd st.player_list = 1 then 
    (if n < nmax-1 then n + 2
     else if n<nmax then 1 else 2)
  else (if n > 2 then n - 2 
        else if n > 1 then nmax else nmax-1)

type result = Legal of t | Illegal

(** [if_uno_bool dic] is the bool value representing if the user needs
    to call uno in the situation of [dic]. *)
let if_uno_bool dic = 
  count_cards dic 1 0 = 1 

let take st  =
  if st.cards_left != [] 
  then let card_get = random_one st.cards_left in 
    Legal{
      current_card = st.current_card;
      cards_played_and_held = card_get :: st.cards_played_and_held;
      cards_left = remove [] card_get st.cards_left;
      dic = (card_get, st.current_player)::(dic st);
      current_player = new_player st;
      uno_bool = if_uno_bool ((card_get, st.current_player)::(dic st));
      player_list = st.player_list
    }
  else Illegal

(** [if_use id st uno] is a helper function that helps determine whether
    it is legal to use card [id] when the current state is [st]. 
    True if legal; False if not. *)
let if_use id st uno = 
  let current = st.current_card in
  get_color uno id = "wild" ||
  (get_color uno current) = (get_color uno id) ||
  (get_number uno current) = (get_number uno id) 


let rec get_cards id dic lst = 
  match dic with 
  |[]->lst
  |(a,b)::t -> if b=id then get_cards id t (a::lst) else get_cards id t lst


(** [if_skip id] is true if [id] is the id of a skip card; otherwise false. 
    Requires: [id] is a valid card id. *)
let if_skip id = 
  String.get id 1 = 's' 

(** [if_reverse id] is true if [id] is the id of a reverse card; otherwise false. 
    Requires: [id] is a valid card id. *)
let if_reverse id = 
  String.get id 1 = 'r' 

(** [if_skip id] is true if [id] is the id of a skip card; otherwise false. 
    Requires: [id] is a valid card id. *)
let if_draw_two id = 
  String.get id 1 = 'd'  

(** [if_wc id] is true if [id] is the id of a wild color card; otherwise false.*)
let if_wc id = 
  String.sub id 0 2 = "wc" 

(** [if_wd id] is true if [id] is the id of a wild +4 card; otherwise false.*)
let if_wd id = 
  String.sub id 0 2 = "wd" 

(** [draw_two_st_helper st uno card card_rep] is the new st after 
    a draw two card [card] is used in [st] in game [uno]. *)
let draw_two_st_helper st uno card card_rep= 
  if List.length st.cards_left > 1 then (
    let two_rand = get 0 1 (shuffle st.cards_left) [] in 
    let hd = List.hd two_rand in 
    let tl = List.rev two_rand |> List.hd in 
    let new_tuple = get_tuple uno (new_player st) two_rand in 
    Legal {
      current_card = card;
      cards_played_and_held = two_rand @ st.cards_played_and_held;
      cards_left = remove [] hd st.cards_left |> remove [] tl;
      current_player = new_player st;
      dic = new_tuple @ st.dic |> List.remove_assoc card_rep;
      uno_bool = if_uno_bool (new_tuple @ st.dic |> List.remove_assoc card_rep);
      player_list = st.player_list;  
    })
  else Illegal


(** [wd_st_helper st uno card card_rep c] is the new st after 
    a wild draw four [card] is used in [st] in game [uno] and the color
    changes to [c]. *)
let wd_st_helper st uno card_rep c= 
  if List.length st.cards_left > 3 then (
    let four_rand = get 0 3 (shuffle st.cards_left) [] in 
    let new_tuple = get_tuple uno (new_player st) four_rand in 
    let new_current = 
      if String.get st.current_card 0 != 'w' 
      then String.sub c 0 1 ^(String.sub st.current_card 1 
                                (String.length(st.current_card) - 1)) 
      else String.sub c 0 1 ^ "0" in 
    Legal {
      current_card = new_current;
      cards_played_and_held = four_rand @ st.cards_played_and_held;
      cards_left = List.filter (fun x -> List.mem x four_rand = false) 
          st.cards_left;
      current_player = new_player st;
      dic = new_tuple @ st.dic |> List.remove_assoc card_rep;
      uno_bool = if_uno_bool (new_tuple @ st.dic |> List.remove_assoc card_rep);
      player_list = st.player_list;    
    })
  else Illegal

(** [wc_helper st uno card_rep c] is the new st after a wild color card
    with id [card_rep] is used in [st] in game [uno] and the color changes to
    [c].   *)
let wc_helper st uno card_rep c= 
  let new_current = 
    if String.get st.current_card 0 != 'w' 
    then String.sub c 0 1 ^(String.sub st.current_card 1 
                              (String.length(st.current_card) - 1)) 
    else String.sub c 0 1 ^ "0" in 
  Legal {
    current_card = new_current;
    cards_played_and_held = st.cards_played_and_held;
    cards_left = st.cards_left;
    current_player = new_player st;
    dic = List.remove_assoc card_rep st.dic;
    uno_bool = if_uno_bool (List.remove_assoc card_rep st.dic);
    player_list = st.player_list;  
  }


(** [helper_legal nc_id played left p dic ] is a helper function that build 
    a Legal t with each field assigned by [nc_id] [played] [left] [p] [dic]. *)
let helper_legal nc_id played left p dic b pl= 
  Legal {
    current_card = nc_id;
    cards_played_and_held = played;
    cards_left = left;
    current_player= p ;
    dic = dic;
    uno_bool = b;
    player_list = pl;
  }

(** [use_others cards card st uno c] is the new st after 
    a non functional[card] is used in [st] in game [uno]. *)
let use_others cards card st uno c dic= 
  if 
    (List.mem card cards) && (if_use card) st uno 
  then 
    helper_legal card st.cards_played_and_held st.cards_left (new_player st)
      (List.remove_assoc card dic) (if_uno_bool (List.remove_assoc card dic))
      st.player_list
  else if 
    (List.mem (rep_ok card) cards) && (if_use card) st uno 
  then 
    helper_legal card st.cards_played_and_held st.cards_left (new_player st)
      (List.remove_assoc (rep_ok card) dic) 
      (if_uno_bool (List.remove_assoc card dic))
      st.player_list
  else Illegal

(** [use_draw_two  cards card st uno c] is the new st after 
    a draw two card [card] is used in [st] in game [uno]. *)
let use_draw_two  cards card st uno c dic = 
  if 
    (List.mem card cards) && (if_use card) st uno && if_draw_two card
  then 
    draw_two_st_helper st uno card card
  else if 
    (List.mem (rep_ok card) cards) && (if_use card) st uno && if_draw_two card
  then 
    draw_two_st_helper st uno card (rep_ok card)
  else use_others cards card st uno c dic

(** [use_wc cards card st uno c] is a helper function that new st after a wild color card
    with id [card_rep] is used in [st] in game [uno] and the color changes to
    [c]. *)
let use_wc cards card st uno c dic = 
  if 
    (List.mem card cards) && (if_use card) st uno && if_wc card
  then 
    wc_helper st uno card c 
  else if 
    (List.mem (rep_ok card) cards) && (if_use card) st uno && if_wc card
  then 
    wc_helper st uno (rep_ok card) c 
  else if 
    (List.mem (rep_ok (rep_ok card)) cards) && (if_use card) st uno && if_wc card
  then 
    wc_helper st uno (rep_ok (rep_ok card)) c 
  else if 
    (List.mem (rep_ok (rep_ok (rep_ok card))) cards) && (if_use card) st uno 
    && if_wc card
  then 
    wc_helper st uno (rep_ok (rep_ok (rep_ok card))) c 
  else use_draw_two cards card st uno c dic


(** [use_wd cards card st uno c] is the new st after 
    a wild draw four [card] is used in [st] in game [uno] and the color
    changes to [c]. *)
let use_wd cards card st uno c dic= 
  if 
    (List.mem card cards) && (if_use card) st uno && if_wd card
  then 
    wd_st_helper st uno card c 
  else if 
    (List.mem (rep_ok card) cards) && (if_use card) st uno && if_wd card
  then 
    wd_st_helper st uno (rep_ok card) c 
  else if 
    (List.mem (rep_ok (rep_ok card)) cards) && (if_use card) st uno && if_wd card
  then 
    wd_st_helper st uno (rep_ok (rep_ok card)) c 
  else if 
    (List.mem (rep_ok (rep_ok (rep_ok card))) cards) && (if_use card) st uno 
    && if_wd card
  then 
    wd_st_helper st uno (rep_ok (rep_ok (rep_ok card))) c 
  else use_wc cards card st uno c dic





(** [use_reverse cards card st uno c] is the new st after 
    a reverse [card] is used in [st] in game [uno]. *)
let use_reverse cards card st uno c dic = 
  if 
    (List.mem card cards) && (if_use card) st uno && if_reverse card
  then
    helper_legal card st.cards_played_and_held st.cards_left (new_player_s st)
      (List.remove_assoc card dic) (if_uno_bool (List.remove_assoc card dic))
      (List.rev (st.player_list))
  else if 
    (List.mem (rep_ok card) cards) && (if_use card) st uno && if_reverse card 
  then 
    helper_legal card st.cards_played_and_held st.cards_left (new_player_s st)
      (List.remove_assoc (rep_ok card) dic) 
      (if_uno_bool (List.remove_assoc (rep_ok card) dic))
      (List.rev st.player_list)
  else use_wd  cards card st uno c dic

(** [use_skip cards card st uno c] is the new st after 
    a skip [card] is used in [st] in game [uno]. *)
let use_skip cards card st uno c dic = 
  if 
    (List.mem card cards) && (if_use card) st uno && if_skip card
  then
    helper_legal card st.cards_played_and_held st.cards_left (new_new_player st)
      (List.remove_assoc card dic) (if_uno_bool (List.remove_assoc card dic))
      st.player_list
  else if 
    (List.mem (rep_ok card) cards) && (if_use card) st uno && if_skip card 
  then 
    helper_legal card st.cards_played_and_held st.cards_left (new_new_player st)
      (List.remove_assoc (rep_ok card) dic) 
      (if_uno_bool (List.remove_assoc (rep_ok card) dic))
      st.player_list
  else use_reverse cards card st uno c dic


let use st uno card c= 
  let id = current_player_id st in 
  let dic = dic st in 
  let cards = List.sort_uniq compare (get_cards id dic []) in 
  if List.length cards = 0 then Illegal 
  else (use_skip cards card st uno c dic)


let uno st = 
  let dic = dic st in 
  let id = current_player_id st in 
  if ((count_cards dic id 0 )=1) 
  then Legal{
      current_card = st.current_card;
      cards_played_and_held = st.cards_played_and_held;
      cards_left = st.cards_left;
      dic = st.dic;
      current_player = st.current_player;
      uno_bool = false;
      player_list = st.player_list;
    }
  else Illegal


