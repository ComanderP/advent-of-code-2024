open Utils

let day = D6

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

type direction = Up | Down | Left | Right

module States = Set.Make (struct
  type t = int * int * direction

  let compare = compare
end)

module Obstacles = Set.Make (struct
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

let is_stuck input guard =
  let rec simulate visited (row, col, dir) =
    if States.mem (row, col, dir) visited then true
    else
      let visited = States.add (row, col, dir) visited in
      let row', col' = move (row, col) dir in
      if out_of_bounds (row', col') input then false
      else if input.(row').(col') = '#' then
        let dir' = turn_right dir in
        simulate visited (row, col, dir')
      else simulate visited (row', col', dir)
  in
  simulate States.empty guard

let rec move_until_out_of_bounds placed acc input (row, col, dir) =
  (* Place obstacle in front of the guard unless out of bounds *)
  let row', col' = move (row, col) dir in
  if out_of_bounds (row', col') input then acc
  else if input.(row').(col') = '#' then
    let dir' = turn_right dir in
    move_until_out_of_bounds placed acc input (row, col, dir')
  else if Obstacles.mem (row', col') placed then
    move_until_out_of_bounds placed acc input (row', col', dir)
  else
    let input' = Array.map Array.copy input in
    input'.(row').(col') <- '#';
    let placed = Obstacles.add (row', col') placed in
    let res = is_stuck input' (row, col, dir) in
    if res then move_until_out_of_bounds placed (acc + 1) input (row', col', dir)
    else move_until_out_of_bounds placed acc input (row', col', dir)

let solve input : string =
  let row, col, dir = Option.get (find_guard input) in
  let placed = Obstacles.(empty |> add (row, col)) in
  move_until_out_of_bounds placed 0 input (row, col, dir) |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)
let parse (lines : string list) =
  List.map (fun s -> String.to_seq s |> Array.of_seq) lines |> Array.of_list

(* Main function to read input and run the solution *)
let () =
  test2 day parse solve "6";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
