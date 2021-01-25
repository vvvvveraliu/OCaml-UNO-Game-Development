open Uno
open Command
open State



(** [read_file f] reads file [f] and returns the uno game. *)
let rec read_file f = 
  try from_json(Yojson.Basic.from_file(f)) with
  |Sys_error f -> print_endline "Invalid file.";
    print_endline "";
    read_file (read_line())

(** [invalid_command c] deals with invalid command by displaying 
    error message and let the user make command again.  *)
let rec invalid_command user_command = 
  try parse user_command with 
  |Malformed -> 
    print_endline "I can't understand you. ";
    print_endline "";
    invalid_command (read_line())
  |Empty -> 
    print_endline "Say something or I'm giving up on you. ";
    print_endline ""; 
    invalid_command (read_line())

(** [print_string_list sl] is the helper function that returns the string
    in [sl] that is connected by comma. *)
let rec print_string_list sl = 
  match sl with 
  | [] -> "nothing else"
  | h::t -> (h ^ ", " ^ print_string_list t)


(** [card_description uno id] is the helper function that gives the word
    decription of card with id [id] in [uno]. 
    Example: card w/ id "r0" would be "red zero". *)
let card_description uno id= 
  get_color uno id :: get_number uno id :: [] |> String.concat " "


(** [win p st] determine whether the player [p] wins. If so, true; else falso. *)
let win p st = 
  let dic = State.dic st in 
  if (State.count_cards dic p 0 )=0 then true else false

(** [recommend_command all_cards] is the recommended command computed for the 
    user having cards [all_cards]. *)
let rec recommend_command all_cards st uno= 
  match all_cards with
  | [] -> "take"
  | h::t -> 
    let color = (match (String.split_on_char ' ' h) with 
        | h::_ -> h | _ -> " ") in 
    let number = (match (String.split_on_char ' ' h) with
        | _::t::tt::_ -> t ^ " " ^ tt 
        | _::t::_ -> t
        | _ -> " ") in 
    if color = "wild" then "use " ^ h
    else if number = get_number uno (current_card_id st) then "use " ^ h
    else if color = get_color uno (current_card_id st) then "use " ^ h
    else recommend_command t st uno

(** [beginning_words_1 uno st] prints the beginning words each turn for the user
    player. *)
let beginnning_words_1 uno st = 
  print_endline ("Now comes the player " 
                 ^ string_of_int (current_player_id st) ^ ". Your turn!");
  print_endline "Below is the cards you have. ";
  print_endline (State.get_cards (current_player_id st) (State.dic st) []
                 |> List.map (card_description uno) |> 
                 List.sort compare |> print_string_list);
  print_endline "";
  print_endline ("The current card is " ^ 
                 (current_card_id st |> card_description uno) ^
                 ", you can play a card of either the same color or the same number, or any wild card. If not, take one card by random. ");
  print_endline "Remember to call 'uno' when you have only one card left. ";
  print_endline ("There are " ^ (List.length (left st) |> string_of_int) ^ 
                 " cards left in total. ");
  print_endline "";
  print_endline "What's next? "

(** [beginning_words_others uno st] prints the beginnning words each turn
    for non-user players. *)
let beginnning_words_others uno st = 
  print_endline ("Now comes the player " 
                 ^ string_of_int (current_player_id st));
  print_endline ("The player has " ^ 
                 (State.get_cards (current_player_id st) (State.dic st) [] 
                  |> List.length |> string_of_int) ^ " cards left. ")

(** [beginning_words_others uno st] prints the beginnning words each turn
    for non-user players. 
    let beginnning_words_others uno st = 
    print_endline ("Now comes the player " 
                 ^ string_of_int (current_player_id st));
    print_endline ("The player has " ^ 
                 (State.get_cards (current_player_id st) (State.dic st) [] 
                  |> List.length |> string_of_int) ^ " cards left. ");
    print_endline ("Cheating line comes for testing: ");
    print_endline (State.get_cards (current_player_id st) (State.dic st) []
                 |> List.map (card_description uno) |> 
                 List.sort compare |> print_string_list) *)

(** [helper_skip st id] prints lines if [id] is a skip card and it is used 
    in [st]. Do nothing otherwise. *)
let helper_skip st id = 
  if String.get id 1 = 's' then 
    (print_endline "";
     print_endline ("Player " ^ string_of_int(current_player_id st) ^ 
                    " uses a skip card so the next player is skipped. "))
  else ()

(** [helper_reverse st id] prints lines if [id] is a reverse card and it is used 
    in [st]. Do nothing otherwise. *)
let helper_reverse st id = 
  if String.get id 1 = 'r' then 
    (print_endline "";
     print_endline ("Player " ^ string_of_int(current_player_id st) ^ 
                    " uses a reverse card so the order of players is reversed.  "))
  else ()

(** [helper_draw_two st id] prints lines if [id] is a +2 card and it is used 
    in [st]. Do nothing otherwise. *)
let helper_draw_two st id = 
  if String.get id 1 = 'd' && String.get id 0 != 'w' then 
    (print_endline "";
     print_endline ("Player " ^ string_of_int(current_player_id st) ^ 
                    " uses a draw two card so the next player gets two cards. "))
  else ()

(** [helper_draw_four st id] prints lines if [id] is a +4 card and it is used 
    in [st]. Do nothing otherwise. *)
let helper_draw_four st id = 
  if String.get id 0 = 'w' && String.get id 1 = 'd' then 
    (print_endline "";
     print_endline ("Player " ^ string_of_int(current_player_id st) ^ 
                    " uses a draw four card so the next player gets four cards and color of the current card will be changed. "))
  else ()

(** [illegal_take_helper] prints lines when the user takes illegally. *)
let illegal_take_helper ()= 
  (print_endline "";
   print_endline "There is nothing else for you to take!";
   print_endline "You're good to go. Maybe try restart the game?";)

(** [uno_command c] is true iff c is the uno command. *)
let uno_command c = 
  match c with 
  | Uno -> true 
  | _ -> false

(** [get_card_name t] is the name of card [t]. 
    For example, [get_card_name "red draw two"] is "draw two". *)
let get_card_name t = match t with
  | _::t::tt::_ -> t ^ " " ^ tt 
  | _::t::_ -> t
  | _ -> " "

(** [wild_color_helper name] is the color the user wants to change if [name] is
    a wild color card. *)
let rec wild_color_helper name = 
  if name = "color" || name = "draw four"
  then (print_endline"";
        print_endline "What color do you want to change to? ";
        match read_line() with 
        |"red" -> "red"
        |"yellow" -> "yellow"
        |"blue" -> "blue"
        |"green" -> "green"
        |_ -> print_endline "Invalid color. Please choose one of red, yellow, blue, green. ";
          wild_color_helper "color")
  else "no color"


(** [helper_count_cards id_ls c acc] is the number of cards with color [c]
    in [id_ls]. *)
let rec helper_count_cards id_ls c acc= 
  match id_ls with 
  | [] -> acc
  | h::t -> if String.get h 0 = String.get c 0 
    then helper_count_cards t c acc+1 
    else helper_count_cards t c acc


(** [wild_color_helper_nonuser name st] is the recommended color in [st] if
    [name] is color i.e. a color card is used by a non-user player. *)
let wild_color_helper_nonuser name st = 
  if name = "color" || name = "draw four"
  then (let all_cards = get_cards (current_player_id st) (dic st) [] in
        let reds = helper_count_cards all_cards "red" 0 in 
        let yellows = helper_count_cards all_cards "yellow" 0 in 
        let blues = helper_count_cards all_cards "blue" 0 in 
        let greens = helper_count_cards all_cards "greens" 0 in 
        let max = max reds yellows |> max blues |> max greens in 
        if reds = max then "red"
        else if yellows = max then "yellow"
        else if blues = max then "blue"
        else "green")
  else "no color"


(** [wild_color_lines color st uno] prints lines when [st]'s color
    changes to [color] in game [uno]. *)
let wild_color_lines color st uno = 
  if color = "no color" then ()
  else print_endline ("The color has been changed to " ^ color ^ 
                      ". The current card is " ^ 
                      (current_card_id st |> card_description uno) ^ ". ")


(** [loop uno st] executes a loop of operations under the users command 
    in state [st] of game [uno]. *)
let rec loop uno st = 
  print_endline "";
  if current_player_id st = 1 then 
    ( beginnning_words_1 uno st ;print_endline "";
      let user_command = read_line() in 
      let command= invalid_command user_command in
      (if not (uno_bool st) || (uno_bool st && uno_command command) then 
         (match command with 
          | Take -> (match take st with 
              | Legal st_new -> (print_endline ("You've taken a new card. ");
                                 loop uno st_new)
              | Illegal ->(illegal_take_helper (); exit 0))
          | Use t -> (let c = (List.hd t) in
                      let n = get_card_name t in 
                      let id = get_id uno c n in 
                      let color = wild_color_helper n in 
                      match use st uno id color with 
                      | Legal st_new -> 
                        if win (current_player_id st) st_new 
                        then (print_endline "";print_endline "YOU WIN!"; exit 0)
                        else (helper_draw_four st id; 
                              wild_color_lines color st_new uno;
                              helper_skip st id ; helper_reverse st id;
                              helper_draw_two st id; loop uno st_new)
                      | Illegal -> 
                        print_endline "";
                        print_endline "You DON'T have that card or you can't use it now (maybe you want to use draw cards but we don't have that many cards left, quit and restart). Try another. ";
                        loop uno st)
          | Uno -> (match State.uno st with 
              | Legal st_new -> loop uno st_new
              | Illegal -> print_endline "";
                print_endline "You can't call uno now.";loop uno st )
          | Quit -> exit 0)
       else (match invalid_command "take" with 
           | Take -> (match take st with 
               | Legal st_new -> print_endline ("Oops you didn't call uno so you now have one more card.");
                 loop uno st_new
               | Illegal -> print_endline "Although you didn't call uno, there is no more card so you are good:)";
                 loop uno st )
           | _ -> loop uno st) ))
  else (
    beginnning_words_others uno st;
    let cards = State.get_cards (current_player_id st) (State.dic st) []
                |> List.map (card_description uno) in
    let recommended_command = recommend_command cards st uno in
    let command= invalid_command recommended_command in
    match command with 
    | Take -> (match take st with 
        | Legal st_new -> 
          print_endline ("Player " ^ string_of_int (current_player_id st) ^
                         " takes a new card");
          loop uno st_new
        | Illegal -> (print_endline "There is no card left and it seems like you need to restart the game. ");
          exit 0)
    | Use t -> (let c = (List.hd t) in
                let n = get_card_name t in  
                let id = get_id uno c n in 
                let color = wild_color_helper_nonuser n st in 
                match use  st uno id color with 
                | Legal st_new -> 
                  print_endline ("Player " ^ 
                                 string_of_int (current_player_id st) ^ ": " 
                                 ^ recommended_command) ;
                  print_endline ("There are " ^ (List.length (left st_new) 
                                                 |> string_of_int) ^ " cards left in total. ");
                  if win (current_player_id st) st_new 
                  then (print_endline "";
                        print_endline ("Player " ^ 
                                       string_of_int (current_player_id st) ^ "  wins!");
                        print_endline "YOU LOST!";
                        exit 0)
                  else (helper_draw_four st id; 
                        wild_color_lines color st_new uno;
                        helper_skip st id ;helper_reverse st id; 
                        helper_draw_two st id; 
                        loop uno st_new)
                | Illegal -> (print_endline "There is no card left and it seems like you need to restart the game. ");
                  exit 0)
    | _ -> loop uno st )


(** [choose_player f] allows users to enter the number of players in this game
    and returns the user input *)
let rec choose_player f = 
  print_endline "Enter the number of player (2-10)";
  let input = read_line() in 
  if String.length input != 0 then
    try (match int_of_string input with 
        |h when h>=2 && h<= 10 -> h 
        |_-> print_endline "User number is between 2 and 10"; choose_player f )
    with e -> print_endline "Please enter a valid integer"; choose_player f
  else (print_endline "Say something"; choose_player f)


(** [play_game f] starts the adventure in file [f]. *)
let play_game f =
  let uno = read_file f in 
  let num_of_player = choose_player f in 
  let init_state = State.init_state uno num_of_player in 
  loop uno (init_state)


(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Text Uno Game engine.\n");
  print_endline "Please enter 'uno.json' to start the game.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()

