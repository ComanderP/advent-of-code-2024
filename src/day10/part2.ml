open Utils

let day = D10

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

module Position = struct
  type t = int * int

  let compare = compare
end

module Map = Map.Make (Position)
module Pos_set = Set.Make (Position)

let trailheads map = Map.filter (fun _ height -> height = 0) map

let rec count_paths prev (row, col) map =
  match Map.find_opt (row, col) map with
  | Some x when x - prev = 1 ->
      if x = 9 then 1
      else
        let up = count_paths x (row - 1, col) map in
        let right = count_paths x (row, col + 1) map in
        let down = count_paths x (row + 1, col) map in
        let left = count_paths x (row, col - 1) map in
        up + right + down + left
  | _ -> 0

let solve input : string =
  input |> trailheads
  |> Fun.flip (Map.fold (fun pos _ acc -> acc + count_paths (-1) pos input)) 0
  |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)
let map_of_seqs seqs =
  Seq.fold_lefti
    (fun map row line ->
      Seq.fold_lefti
        (fun map col char -> Map.add (row, col) (int_of_char_digit char) map)
        map line)
    Map.empty seqs

let parse (lines : string list) =
  lines |> List.map String.to_seq |> List.to_seq |> map_of_seqs

(* Main function to read input and run the solution *)
let () =
  test1 day parse solve "81";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
