open Utils

let day = D6

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

type direction = Up | Down | Left | Right

module Positions = Set.Make (struct
  type t = int * int

  let compare = compare
end)

let dir_of_guard = function
  | '^' -> Up
  | 'v' -> Down
  | '<' -> Left
  | '>' -> Right
  | _ -> failwith "Invalid direction"

let move (row, col) = function
  | Up -> (row - 1, col)
  | Down -> (row + 1, col)
  | Left -> (row, col - 1)
  | Right -> (row, col + 1)

let turn_right = function
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up

let guard_opt row col char =
  let is_guard = function '^' | 'v' | '<' | '>' -> true | _ -> false in
  if is_guard char then Some (row, col, dir_of_guard char) else None

let find_guard input =
  Array.find_mapi (fun row line -> Array.find_mapi (guard_opt row) line) input

let out_of_bounds (row, col) input =
  row < 0
  || row >= Array.length input
  || col < 0
  || col >= Array.length input.(0)

let rec move_until_out_of_bounds visited input (row, col, dir) =
  let visited = Positions.add (row, col) visited in
  let row', col' = move (row, col) dir in
  if out_of_bounds (row', col') input then visited
  else if input.(row').(col') = '#' then
    let dir' = turn_right dir in
    move_until_out_of_bounds visited input (row, col, dir')
  else move_until_out_of_bounds visited input (row', col', dir)

let solve input : string =
  let visited = Positions.empty in
  let row, col, dir = Option.get (find_guard input) in
  let visited = move_until_out_of_bounds visited input (row, col, dir) in
  Positions.cardinal visited |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)
let parse (lines : string list) =
  List.map (fun s -> String.to_seq s |> Array.of_seq) lines |> Array.of_list

(* Main function to read input and run the solution *)
let () =
  test1 day parse solve "41";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
