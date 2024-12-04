open Utils

let day = D4

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

let out_of_bounds i bound array =
  i < bound || i >= Array.length array.(0) - bound

let diag_neighbours (row, col) array =
  ( array.(row - 1).(col + 1),
    array.(row + 1).(col - 1),
    array.(row - 1).(col - 1),
    array.(row + 1).(col + 1) )

let xmas array = function
  | row, _ when out_of_bounds row 1 array -> 0
  | _, col when out_of_bounds col 1 array -> 0
  | row, col when array.(row).(col) = 'A' -> (
      match diag_neighbours (row, col) array with
      | 'S', 'M', 'S', 'M' -> 1
      | 'M', 'S', 'S', 'M' -> 1
      | 'S', 'M', 'M', 'S' -> 1
      | 'M', 'S', 'M', 'S' -> 1
      | _ -> 0)
  | _ -> 0

let map2d f array =
  Array.mapi (fun row l -> Array.mapi (fun col _ -> f array (row, col)) l) array

let solve (input : char array array) : string =
  map2d xmas input
  |> Array.fold_left (fun acc l -> acc + Array.fold_left ( + ) 0 l) 0
  |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let parse (lines : string list) =
  List.map (fun s -> String.to_seq s |> Array.of_seq) lines |> Array.of_list

(* Main function to read input and run the solution *)
let () =
  test2 day parse solve "9";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
