open Utils

let day = D4

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

let directions =
  [| (-1, 0); (1, 0); (0, -1); (0, 1); (-1, -1); (-1, 1); (1, -1); (1, 1) |]

let go (row, col) (row', col') = (row + row', col + col')

let out_of_bounds i bound array =
  i < bound || i >= Array.length array.(0) - bound

let xmas array pos dir : bool =
  let rec aux acc n = function
    | _, _ when n = 4 -> acc = "XMAS"
    | row, _ when out_of_bounds row 0 array -> false
    | _, col when out_of_bounds col 0 array -> false
    | (row, col) as pos ->
        let c = array.(row).(col) in
        aux (append_char acc c) (n + 1) (go pos dir)
  in
  aux "" 0 pos

let sum = Array.fold_left ( + ) 0

let map2d f array =
  Array.mapi (fun row l -> Array.mapi (fun col _ -> f array (row, col)) l) array

let count_xmas array pos =
  sum (Array.map (fun dir -> Bool.to_int (xmas array pos dir)) directions)

let solve (input : char array array) : string =
  map2d count_xmas input
  |> Array.fold_left (fun acc l -> acc + sum l) 0
  |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let parse (lines : string list) =
  List.map (fun s -> String.to_seq s |> Array.of_seq) lines |> Array.of_list

(* Main function to read input and run the solution *)
let () =
  test1 day parse solve "18";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
