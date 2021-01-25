open OUnit2
open Uno
open Command
open State

(**Test Plan: Overall uses glass box testing for Uno, Command, and State 
   modules and interactively tests Main module and some functions in State 
   module. 

   For Uno, Command, and some parts of State: 
   Use glass box testing to write OUnit tests.  

   All functions in mli files are tested with at least one test case for each 
   situation. Specifically, for functions in uno.mli, we tested them with
   different types of card names, colors, and ids. For functions in command.mli, 
   we tested them with legal cases and illegal ones. For functions in state.mli, 
   since the generation of state is random so we are only able to test each 
   field of the init state and simple functions on them give results with the 
   right length. We tested the content of results and other functions that 
   directly deal with the user's command interactively.  Helper functions in 
   those modules are employed by functions being tested so they also get tested. 

   For Main module and some functions in state module (those that deals with
   a command to give the new state), we test them interactively by running 
   the game interface with different commands. We tried each command with
   different object phrase to make sure they function properly. This also 
   enabled us to double check Command module. Besides, we
   subsituted the user command with "take" and run the game automatically
   with maximum number of players for numerous times to make sure the game 
   run properly under random situations. 
*)

(********************************************************************
   Here are some helper functions for your testing of set-like lists. 
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(* These tests demonstrate how to use [cmp_set_like_lists] and 
   [pp_list] to get helpful output from OUnit. *)
let cmp_demo = 
  [
    "order is irrelevant" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "bar"] ["bar"; "foo"]);
    (* Uncomment this test to see what happens when a test case fails.
       "duplicates not allowed" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "foo"] ["foo"]);
    *)
  ]

(********************************************************************
   End helper functions.
 ********************************************************************)

(* You are welcome to add strings containing JSON here, and use them as the
   basis for unit tests.  Or you can add .json files in this directory and
   use them, too.  Any .json files in this directory will be included
   by [make zip] as part of your CMS submission. *)
let _ = Printexc.record_backtrace true

let make_cards_of_color_test 
    (name : string) 
    (uno: Uno.t) 
    (color: color)
    (expected_output : nc_id list) : test = 
  name >:: (fun _ -> 
      assert_equal ~cmp:cmp_set_like_lists expected_output 
        (cards_of_color uno color))

let make_cards_of_number_test 
    (name : string) 
    (uno: Uno.t) 
    (number: number)
    (expected_output : nc_id list) : test = 
  name >:: (fun _ -> 
      assert_equal ~cmp:cmp_set_like_lists expected_output
        (cards_of_number uno number))

let make_nc_id_lst_test 
    (name : string) 
    (uno: Uno.t) 
    (expected_output : nc_id list) : test = 
  name >:: (fun _ -> 
      assert_equal ~cmp:cmp_set_like_lists expected_output (nc_id_lst uno))


let make_get_color_test 
    (name : string) 
    (uno: Uno.t) 
    (id:nc_id)
    (expected_output : color) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_color uno id))

let make_get_number_test 
    (name : string) 
    (uno: Uno.t) 
    (id:nc_id)
    (expected_output : number) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_number uno id))

let make_get_id_test 
    (name : string) 
    (uno: Uno.t) 
    (color:color)
    (number:number)
    (expected_output : nc_id) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_id uno color number))

let make_parse_test 
    (name : string) 
    (str: string) 
    (expected_output : command) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (parse str))

(** [create_uno] is the adventure created by uno.json. *)
let import_uno = Yojson.Basic.from_file "uno.json"
let create_uno= from_json import_uno

let uno_tests =
  [
    (* Test Cases for cards_of_color*)
    make_cards_of_color_test "cards_of_color test 1" create_uno "red"
      ["r0";"r11";"r12";"r21";"r22";"r31";"r32";"r41";"r42";"r51";"r52";
       "r61";"r62";"r71";"r72";"r81";"r82";"r91";"r92";"rs1";"rs2";"rd1";
       "rd2";"rr1";"rr2"];
    make_cards_of_color_test "cards_of_color test 2" create_uno "yellow" 
      ["y0";"y11";"y12";"y21";"y22";"y31";"y32";"y41";"y42";"y51";"y52";
       "y61";"y62";"y71";"y72";"y81";"y82";"y91";"y92";"ys1";"ys2";"yd1";
       "yd2";"yr1";"yr2"];
    make_cards_of_color_test "cards_of_color test 3" create_uno "green" 
      ["g0";"g11";"g12";"g21";"g22";"g31";"g32";"g41";"g42";"g51";"g52";
       "g61";"g62";"g71";"g72";"g81";"g82";"g91";"g92";"gs1";"gs2";"gd1";
       "gd2";"gr1";"gr2"];
    make_cards_of_color_test "cards_of_color test 4" create_uno "blue" 
      ["b0";"b11";"b12";"b21";"b22";"b31";"b32";"b41";"b42";"b51";"b52";
       "b61";"b62";"b71";"b72";"b81";"b82";"b91";"b92";"bs1";"bs2";"bd1";
       "bd2";"br1";"br2"];
    make_cards_of_color_test "cards_of_color test 5" create_uno "wild" 
      ["wc1";"wc2";"wc3";"wc4";"wd1";"wd2";"wd3";"wd4"];

    (* Test Cases for cards_of_number*)
    make_cards_of_number_test "cards_of_number test 1" create_uno "one"
      ["b11";"b12";"g11";"g12";"r11";"r12";"y11";"y12"];
    make_cards_of_number_test "cards_of_number test 2" create_uno "zero"
      ["b0";"g0";"r0";"y0"];
    make_cards_of_number_test "cards_of_number test 3" create_uno "skip"
      ["rs1";"rs2";"ys1";"ys2";"bs1";"bs2";"gs1";"gs2"];
    make_cards_of_number_test "cards_of_number test 4" create_uno "draw two"
      ["rd1";"rd2";"yd1";"yd2";"bd1";"bd2";"gd1";"gd2"];
    make_cards_of_number_test "cards_of_number test 5" create_uno "color"
      ["wc1";"wc2";"wc3";"wc4"];
    make_cards_of_number_test "cards_of_number test 6" create_uno "draw four"
      ["wd1";"wd2";"wd3";"wd4"];
    make_cards_of_number_test "cards_of_number test 7" create_uno "reverse"
      ["rr1";"rr2";"yr1";"yr2";"br1";"br2";"gr1";"gr2"];

    (* Test Cases for cards_of_nc_id_lst*)
    make_nc_id_lst_test  "nc_id_lst test 1" create_uno 
      ["b0";"b11";"b12";"b21";"b22";"b31";"b32";"b41";"b42";"b51";"b52";
       "b61";"b62";"b71";"b72";"b81";"b82";"b91";"b92";
       "g0";"g11";"g12";"g21";"g22";"g31";"g32";"g41";"g42";"g51";"g52";
       "g61";"g62";"g71";"g72";"g81";"g82";"g91";"g92";
       "r0";"r11";"r12";"r21";"r22";"r31";"r32";"r41";"r42";"r51";"r52";
       "r61";"r62";"r71";"r72";"r81";"r82";"r91";"r92";"y0";
       "y11";"y12";"y21";"y22";"y31";"y32";"y41";"y42";"y51";"y52";
       "y61";"y62";"y71";"y72";"y81";"y82";"y91";"y92";"rs1";"rs2";
       "ys1";"ys2";"gs1";"gs2";"bs1";"bs2";"rd1";"rd2";"yd1";"yd2";
       "bd1";"bd2";"gd1";"gd2";"wc1";"wc2";"wc3";"wc4";"wd1";"wd2";
       "wd3";"wd4";"rr1";"rr2";"yr1";"yr2";"br1";"br2";"gr1";"gr2"];

    make_get_color_test "get_color test 1" create_uno "g51" "green";
    make_get_color_test "get_color test 2" create_uno "r0" "red";
    make_get_color_test "get_color test 3" create_uno "y61" "yellow";
    make_get_color_test "get_color test 4" create_uno "b12" "blue";
    make_get_color_test "get_color test 5" create_uno "rs1" "red";
    make_get_color_test "get_color test 6" create_uno "ys1" "yellow";
    make_get_color_test "get_color test 7" create_uno "bs1" "blue";
    make_get_color_test "get_color test 8" create_uno "gs2" "green";
    make_get_color_test "get_color test 9" create_uno "wd2" "wild";
    make_get_color_test "get_color test 10" create_uno "wc3" "wild";

    make_get_number_test "get_number test 1" create_uno "g51" "five";
    make_get_number_test "get_number test 2" create_uno "r0" "zero";
    make_get_number_test "get_number test 3" create_uno "bs1" "skip";
    make_get_number_test "get_number test 4" create_uno "gd1" "draw two";
    make_get_number_test "get_number test 5" create_uno "wc2" "color";
    make_get_number_test "get_number test 6" create_uno "wd3" "draw four";

    make_get_id_test "get_id test 1" create_uno "red" "one" "r11";
    make_get_id_test "get_id test 2" create_uno "red" "zero" "r0";
    make_get_id_test "get_id test 3" create_uno "blue" "skip" "bs1";
    make_get_id_test "get_id test 4" create_uno "yellow" "three" "y31";
    make_get_id_test "get_id test 5" create_uno "wild" "color" "wc1";
    make_get_id_test "get_id test 6" create_uno "green" "draw two" "gd1";
    make_get_id_test "get_id test 7" create_uno "wild" "draw four" "wd1";
  ]

let command_tests =
  [
    make_parse_test "parse test 1" "take" Take;
    make_parse_test "parse test 2" "uno" Uno;
    make_parse_test "parse test 3" "use red one" (Use ["red"; "one"]);
    "parse test 4" >:: (fun _ -> 
        assert_raises 
          Malformed (fun _ -> parse "s"));
    "parse test 5" >:: (fun _ -> 
        assert_raises 
          Malformed (fun _ -> parse "take         a"));
    "parse test 6" >:: (fun _ -> 
        assert_raises 
          Malformed (fun _ -> parse "uno   a"));
    "parse test 7" >:: (fun _ -> 
        assert_raises 
          Malformed (fun _ -> parse "use red"));
    "parse test 8" >:: (fun _ -> 
        assert_raises 
          Malformed (fun _ -> parse "use "));
    "parse test 9" >:: (fun _ -> 
        assert_raises 
          Empty (fun _ -> parse "      "));
    make_parse_test "parse test 10" "use blue skip" (Use ["blue"; "skip"]);
    "parse test 11" >:: (fun _ -> 
        assert_raises 
          Malformed (fun _ -> parse "use skip one"));
    make_parse_test "parse test 12" "use blue zero" (Use ["blue"; "zero"]);
    make_parse_test "parse test 13" "use wild   color" (Use ["wild"; "color"]);
    make_parse_test "parse test 14" "use wild draw four" 
      (Use ["wild"; "draw"; "four"]);
    make_parse_test "parse test 15" "use blue draw two" 
      (Use ["blue"; "draw"; "two"]);
    "parse test 16" >:: (fun _ -> 
        assert_raises 
          Malformed (fun _ -> parse "use wild color four"));
    "parse test 17" >:: (fun _ -> 
        assert_raises 
          Malformed (fun _ -> parse "use c four"));
    "parse test 18" >:: (fun _ -> 
        assert_raises 
          Malformed (fun _ -> parse "use wild draw two"));
    make_parse_test "parse test 19" "use red reverse  " (Use ["red";"reverse"]);
  ]

let state_tests =
  [
    (* TODO: add tests for the State module here *) 
    "init_state_1" >:: (fun _ ->
        assert_equal 14 (((init_state create_uno 2 |> played_and_held) 
                          |> List.length)));
    "init_state_2" >:: (fun _ ->
        assert_equal 93 (((init_state create_uno 2 |> left)) 
                         |> List.length)); 
    "init_state_3" >:: (fun _ ->
        assert_equal 1 (((init_state create_uno 2|> current_player_id))  
                       )); 
    "init_state_4" >:: (fun _ ->
        assert_equal  14 (((init_state create_uno 2|> dic))|> List.length  
                         )); 
    "init_state_5" >:: (fun _ ->
        assert_equal  70 (((init_state create_uno 10|> dic))|> List.length  
                         )); 
    "init_state_6" >:: (fun _ ->
        assert_equal 37 (((init_state create_uno 10 |> left)) 
                         |> List.length));
    "init_state_7" >:: (fun _ ->
        assert_equal false (init_state create_uno 10 |> uno_bool));
    "init_state_8" >:: (fun _ ->
        assert_equal 4 (current_player_number (init_state create_uno 4
                                               |> dic) 0)); 
    "init_state_8" >:: (fun _ ->
        assert_equal 7 ((get_cards 2 (init_state create_uno 8 |> dic) [] )
                        |> List.length));
    "count_cards_1" >:: (fun _ -> let ini = init_state create_uno 4 in 
                          assert_equal 7 
                            (count_cards (dic ini) 3 0));

  ]

let suite =
  "test suite for A2"  >::: List.flatten [
    uno_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite
