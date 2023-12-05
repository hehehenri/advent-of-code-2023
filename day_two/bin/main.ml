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
    max_set.red > set.red
    && max_set.green > set.green
    && max_set.blue > set.blue

  let game_is_possible game = 
    let rec game_is_possible sets = match sets with
      | [] -> true
      | set :: tail -> match set_is_possible set with
        | true -> game_is_possible tail
        | false -> false in
    game_is_possible game.sets

  let parse_game_set input =
    let rec parse_game_set input acc =
      match input with
      | digit :: ' ' :: 'b' :: 'l' :: 'u' :: 'e' :: rest -> let number = int_of_char digit in 
        parse_game_set rest (set_blue number acc) 
      | digit :: ' ' :: 'r' :: 'e' :: 'd' :: rest -> let number = int_of_char digit in parse_game_set rest (set_red number acc)
      | digit :: ' ' :: 'g' :: 'r' :: 'e' :: 'e' :: 'n' :: rest -> let number = int_of_char digit in parse_game_set rest (set_green number acc) 
      | _ :: rest -> parse_game_set rest acc
      | [] -> acc
    in
    parse_game_set input new_set
  ;;

  let parse_game input =
    let (prefix, sets) = match String.split_on_char ';' input with
    | prefix :: sets :: [] -> (prefix, sets)
    | _ -> failwith "invalid game format" in

    let id = match prefix |> String.to_seq |> List.of_seq with
    | 'G' :: 'a' :: 'm' :: 'e' :: ' ' :: id :: _ -> id
    | _ -> failwith "invalid game prefix" in
    let id = int_of_char id in

    let sets = String.split_on_char ';' sets in
    let sets = List.map (fun x -> x |> String.to_seq |> List.of_seq) sets in
    let sets = List.map parse_game_set sets in

    { id; sets }
  ;;
end

let lines = read_file "input.txt"
let games = List.map Game.parse_game lines
let possible_games = List.filter Game.game_is_possible games

let sum = List.fold_left (fun sum game -> Game.game_id game + sum) 0 possible_games

let () = print_int sum

