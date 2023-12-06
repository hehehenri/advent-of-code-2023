let read_file filename =
  let channel = open_in filename in
  try
    let rec read_lines acc =
      try
        let line = input_line channel in
        read_lines (line :: acc)
      with End_of_file -> List.rev acc
    in
    let lines = read_lines [] in
    close_in channel;
    lines 
  with e ->
    close_in channel;
  raise e
;;

let char_to_int digit =
  let digit = Char.escaped digit in
  try
  int_of_string digit
  with
  (* TODO: this is a bad way to handle cases when left char is not a valid digit*)
  Failure _ -> 0
;;

module Game = struct
  type game_set = { red : int; green : int; blue : int} ;;
  type game = { id : int; sets : game_set list } ;;

  let new_set = { red = 0; green = 0; blue = 0 } ;;
  let max_set = { red = 12; green = 13; blue = 14 } ;;

  let game_id game = game.id ;;

  let set_red red game_set = { red = red; green = game_set.green; blue = game_set.blue } ;;  
  let set_green green game_set = { red = game_set.red; green = green; blue = game_set.blue } ;;  
  let set_blue blue game_set = { red = game_set.red; green = game_set.green; blue = blue } ;;  

  let set_is_possible set =
    print_endline (Printf.sprintf "Red: %s > %s" (string_of_int max_set.red) (string_of_int set.red));
    print_endline (Printf.sprintf "Green: %s > %s" (string_of_int max_set.green) (string_of_int set.green));
    print_endline (Printf.sprintf "Blue: %s > %s" (string_of_int max_set.blue) (string_of_int set.blue));
    let is_possible = max_set.red > set.red
    && max_set.green > set.green
    && max_set.blue > set.blue in
    print_endline (Printf.sprintf "Set is possible: %s" (string_of_bool is_possible));

    is_possible

  let game_is_possible game = 
    let rec game_is_possible sets = match sets with
      | [] -> true
      | set :: tail -> match set_is_possible set with
        | true -> game_is_possible tail
        | false -> false in
    game_is_possible game.sets

  let parse_set input =
    print_endline (Printf.sprintf "parsing set: %s" (input |> List.to_seq |> String.of_seq));
    let rec parse_set input acc =
      match input with
      | digit :: ' ' :: 'b' :: 'l' :: 'u' :: 'e' :: rest -> let number = char_to_int digit in parse_set rest (set_blue number acc) 
      | digit :: ' ' :: 'r' :: 'e' :: 'd' :: rest -> let number = char_to_int digit in parse_set rest (set_red number acc)
      | digit :: ' ' :: 'g' :: 'r' :: 'e' :: 'e' :: 'n' :: rest -> let number = char_to_int digit in parse_set rest (set_green number acc)
      | digit :: digit' :: ' ' :: 'b' :: 'l' :: 'u' :: 'e' :: rest -> 
        let number = char_to_int digit in
        let number' = char_to_int digit' in
        let number = (number * 10) + number' in
        parse_set rest (set_blue number acc) 
      | digit :: digit' :: ' ' :: 'r' :: 'e' :: 'd' :: rest ->
        let number = char_to_int digit in
        let number' = char_to_int digit' in
        let number = (number * 10) + number' in
        parse_set rest (set_red number acc) 
      | digit :: digit' :: ' ' :: 'g' :: 'r' :: 'e' :: 'e' :: 'n' :: rest ->
        let number = char_to_int digit in
        let number' = char_to_int digit' in
        let number = (number * 10) + number' in
        parse_set rest (set_green number acc) 
      | _ :: rest -> parse_set rest acc
      | [] -> acc
    in
    parse_set input new_set
  ;;

  let parse_game input =
    let (prefix, sets) = match String.split_on_char ':' input with
    | prefix :: sets :: [] -> (prefix, sets)
    | invalid_input -> failwith (Printf.sprintf "invalid game format: %s" (String.concat "" invalid_input)) in

    let id = match prefix |> String.to_seq |> List.of_seq with
    | 'G' :: 'a' :: 'm' :: 'e' :: ' ' :: id :: _ -> id
    | _ -> failwith "invalid game prefix" in
    let id = char_to_int id in
    print_endline (Printf.sprintf "parsing game with id %s" (string_of_int id));

    let sets = String.split_on_char ';' sets in
    let sets = List.map (fun x -> x |> String.to_seq |> List.of_seq) sets in
    let sets = List.map parse_set sets in

    { id; sets }
  ;;
end

let lines = read_file "input.txt"
let games = List.map Game.parse_game lines
let possible_games = List.filter Game.game_is_possible games

let sum = List.fold_left (fun sum game -> Game.game_id game + sum) 0 possible_games

let () = print_int sum

