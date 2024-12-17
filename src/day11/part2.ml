open Utils

let day = D11

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

let even_digits stone = String.length stone mod 2 = 0

let trim stone =
  let seq = String.to_seq stone |> Seq.drop_while (fun c -> c = '0') in
  if Seq.is_empty seq then "0" else String.of_seq seq

let split stone =
  let len = String.length stone in
  let half = len / 2 in
  (String.sub stone 0 half, String.sub stone half half)

let replace_stone stone = stone |> int_of_string |> ( * ) 2024 |> string_of_int

let blink = function
  | "0" -> [ "1" ]
  | stone when even_digits stone ->
      let left, right = split stone in
      [ left; trim right ]
  | stone -> [ replace_stone stone ]

let rec calculate_num_stones memo stone blinks =
  try Hashtbl.find memo (stone, blinks)
  with Not_found ->
    let result =
      if blinks = 0 then 1
      else
        let transformed_stones = blink stone in
        List.fold_left
          (fun acc num -> acc + calculate_num_stones memo num (blinks - 1))
          0 transformed_stones
    in
    Hashtbl.add memo (stone, blinks) result;
    result

let solve input : string =
  let memo = Hashtbl.create 100 in
  List.fold_left (fun acc num -> acc + calculate_num_stones memo num 75) 0 input
  |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let parse (lines : string list) = lines |> List.hd |> String.split_on_char ' '

(* Main function to read input and run the solution *)
let () =
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
