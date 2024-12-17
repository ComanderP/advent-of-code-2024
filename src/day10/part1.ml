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
let ( ++ ) = Pos_set.union

let rec paths_to_nine nines prev map (row, col) =
  match Map.find_opt (row, col) map with
  | Some x when x - prev = 1 ->
      if x = 9 then Pos_set.add (row, col) nines
      else
        let up = paths_to_nine nines x map (row - 1, col) in
        let right = paths_to_nine nines x map (row, col + 1) in
        let down = paths_to_nine nines x map (row + 1, col) in
        let left = paths_to_nine nines x map (row, col - 1) in
        up ++ right ++ down ++ left
  | _ -> Pos_set.empty

let solve input : string =
  let trailheads = trailheads input in
  Map.fold
    (fun pos _ acc ->
      acc + Pos_set.cardinal (paths_to_nine Pos_set.empty (-1) input pos))
    trailheads 0
  |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let map_of_seqs seqs =
  let add_to_map row map col char =
    Map.add (row, col) (int_of_char_digit char) map
  in
  let add_map_row map row = Seq.fold_lefti (add_to_map row) map in
  Seq.fold_lefti add_map_row Map.empty seqs

let parse (lines : string list) =
  lines |> List.map String.to_seq |> List.to_seq |> map_of_seqs

(* Main function to read input and run the solution *)
let () =
  test1 day parse solve "36";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
