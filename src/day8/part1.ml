open Utils

let day = D8

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

module Set = Set.Make (struct
  type t = int * int

  let compare = compare
end)

type antenna = { char : char; pos : int * int }

let rec insert ({ char; _ } as ant) = function
  | [] -> [ [ ant ] ]
  | hd :: tl ->
      if char = (List.hd hd).char then (ant :: hd) :: tl
      else hd :: insert ant tl

let rec group_by_char acc = function
  | [] -> acc
  | x :: xs -> group_by_char (insert x acc) xs

let dx a1 a2 = snd a2.pos - snd a1.pos
let dy a1 a2 = fst a2.pos - fst a1.pos

let out_of_bounds bound (row, col) =
  row < 0 || row >= bound || col < 0 || col >= bound

let antinode_opt map_size a1 a2 =
  let row, col = a1.pos in
  let antirow, anticol = (row - dy a1 a2, col - dx a1 a2) in
  if out_of_bounds map_size (antirow, anticol) then None
  else Some (antirow, anticol)

let antinodes map_size antennas =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl ->
        let antinode_pos = antinode_opt map_size in
        let antinodes = List.filter_map (antinode_pos hd) tl in
        let antinodes2 = List.filter_map (Fun.flip antinode_pos hd) tl in
        aux (antinodes @ antinodes2 @ acc) tl
  in
  aux [] antennas

let antinodes_set map_size acc antennas =
  antinodes map_size antennas |> Set.of_list |> Set.union acc

let solve (size, antennas) : string =
  antennas |> group_by_char []
  |> List.fold_left (antinodes_set size) Set.empty
  |> Set.cardinal |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let antenna_opt row (col, c) =
  if c = '.' then None else Some { char = c; pos = (row, col) }

let line_antennas row line =
  Seq.filter_map (antenna_opt row) line |> List.of_seq

let parse (lines : string list) =
  let size = List.length lines in
  let antennas =
    List.(lines |> map String.to_seqi |> mapi line_antennas |> flatten)
  in
  (size, antennas)

(* Main function to read input and run the solution *)
let () =
  test1 day parse solve "14";
  let input = get_input day Input |> parse |> solve in
  print_endline input
