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

let line_points map_size pos d_x d_y =
  let rec aux acc (row, col) =
    if out_of_bounds map_size (row, col) then acc
    else aux ((row, col) :: acc) (row + d_y, col + d_x)
  in
  aux [] pos

let line map_size antenna d_x d_y =
  let row, col = antenna.pos in
  let points = line_points map_size (row + d_y, col + d_x) d_x d_y in
  let points2 = line_points map_size (row - d_y, col - d_x) (-d_x) (-d_y) in
  ((row, col) :: points) @ points2

let line_antinodes map_size hd acc hd2 =
  let d_x = dx hd hd2 in
  let d_y = dy hd hd2 in
  let c = line map_size hd d_x d_y in
  acc @ c

let antinodes map_size antennas =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl ->
        let antinodes = List.fold_left (line_antinodes map_size hd) [] tl in
        aux (acc @ antinodes) tl
  in
  aux [] antennas

let add_antinodes map_size acc antennas =
  antinodes map_size antennas |> Set.of_list |> Set.union acc

let solve (size, antennas) : string =
  antennas |> group_by_char []
  |> List.fold_left (add_antinodes size) Set.empty
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
  test2 day parse solve "34";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
