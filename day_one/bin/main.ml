let rec string_first_number calibration =
  match calibration with
  | 'o' :: 'n' :: 'e' :: _tail -> Some 1
  | 't' :: 'w' :: 'o' :: _tail -> Some 2
  | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _tail -> Some 3
  | 'f' :: 'o' :: 'u' :: 'r' :: _tail -> Some 4
  | 'f' :: 'i' :: 'v' :: 'e' :: _tail -> Some 5
  | 's' :: 'i' :: 'x' :: _tail -> Some 6
  | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _tail -> Some 7
  | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _tail -> Some 8
  | 'n' :: 'i' :: 'n' :: 'e' :: _tail -> Some 9
  | '1' :: _tail -> Some 1
  | '2' :: _tail -> Some 2
  | '3' :: _tail -> Some 3
  | '4' :: _tail -> Some 4
  | '5' :: _tail -> Some 5
  | '6' :: _tail -> Some 6
  | '7' :: _tail -> Some 7
  | '8' :: _tail -> Some 8
  | '9' :: _tail -> Some 9
  | _ :: tail -> string_first_number tail
  | [] -> None
;;

let chars_to_string chars =
  chars |> List.to_seq |> String.of_seq 

let calibration_first_number calibration =
  let number = match string_first_number calibration with
    | Some number -> number
    | None -> failwith "calibration has no numbers" in
  number
;;

let rec walk_backwards reverse_calibration acc =
  print_endline (chars_to_string acc);
  match string_first_number acc with
  | Some(number) -> Some number
  | None -> match reverse_calibration with
    | head :: tail -> walk_backwards tail (head::acc)
    | [] -> None 
;;

let calibration_last_number calibration =
  let number = walk_backwards (List.rev calibration) [] in
  match number with
  | Some number -> number
  | None -> failwith (Printf.sprintf "calibration %s has no numbers" (calibration |> List.to_seq |> String.of_seq))
;;

let calibration_value input =
  let input = input |> String.to_seq |> List.of_seq in
  let first_number = calibration_first_number input in
  let last_number = calibration_last_number input in
  (first_number * 10) + last_number
  
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

let lines = read_file "./input.txt"

let calibration_value = List.fold_left (fun acc input ->
    let value = calibration_value input in 
    value + acc
  ) 0 lines 

let () = print_int calibration_value
