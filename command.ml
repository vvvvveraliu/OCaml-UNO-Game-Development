(* Note: You may introduce new code anywhere in this file. *) 

type object_phrase = string list

type command = 
  | Take 
  | Use of object_phrase
  | Uno 
  | Quit

exception Empty

exception Malformed


let parse str =
  if String.length (String.trim str) = 0 then raise Empty
  else let word_ls = String.split_on_char ' ' str 
                     |> List.filter (fun x -> String.length x != 0) in 
    let hd = List.hd word_ls in 
    if not (hd = "take" || hd = "use" || hd = "uno" || hd = "quit") ||
       hd = "take" && List.length word_ls != 1 ||
       hd = "use" && List.length word_ls != 3 && List.length word_ls !=4||
       hd = "uno" && List.length word_ls != 1 ||
       hd = "quit" && List.length word_ls != 1 
    then raise Malformed
    else if hd = "use" && List.length word_ls = 3 && List.nth word_ls 1 = "wild"
            && List.nth word_ls 2 = "color" 
    then match word_ls with 
      | [] -> raise Empty
      | h::t -> Use t
    else if hd = "use" && List.length word_ls = 4 && List.nth word_ls 1 = "wild"
            && List.nth word_ls 2 = "draw" && List.nth word_ls 3 = "four" 
    then match word_ls with 
      | [] -> raise Empty
      | h::t -> Use t
    else if hd = "use" && List.length word_ls = 3 &&
            (["red";"yellow";"blue";"green"] |> 
             List.mem (List.nth word_ls 1) = false ||
             ["zero";"one";"two";"three";"four";"five";"six";"seven";"eight";
              "nine";"skip";"reverse"] |> List.mem (List.nth word_ls 2) = false)
    then raise Malformed
    else if hd = "use" && List.length word_ls = 4 &&
            (["red";"yellow";"blue";"green"] |> 
             List.mem (List.nth word_ls 1) = false ||
             ["draw"] |> List.mem (List.nth word_ls 2) = false ||
             ["two"] |> List.mem (List.nth word_ls 3) = false)
    then raise Malformed
    else match word_ls with 
      | [] -> raise Empty 
      | h::t -> 
        if h="use" then Use t
        else if h="take" then Take 
        else if h = "quit" then Quit
        else Uno 