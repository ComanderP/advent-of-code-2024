open Utils

let day = D14

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

type robot = { x : int; y : int; vx : int; vy : int }

module Set = Set.Make (struct
  type t = int * int

  let compare = compare
end)

let new_coord coord bound =
  let v = abs coord mod bound in
  if v = 0 then v else if coord < 0 then bound - v else v

let position_in width height n robot =
  let x = robot.x + (n * robot.vx) in
  let y = robot.y + (n * robot.vy) in
  (new_coord x width, new_coord y height)

let find_christmas_tree width height input =
  let rec aux n =
    let coords = List.map (position_in width height n) input in
    if List.length coords = Set.cardinal (Set.of_list coords) then n
    else aux (succ n)
  in
  aux 0

let solve width height input : string =
  input |> find_christmas_tree width height |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let parse_line (line : string) =
  let nth_int l n = int_of_string (List.nth l n) in
  let line = nth_int (split_on_chars [ ','; '='; ' ' ] line) in
  let x, y = (line 1, line 2) in
  let vx, vy = (line 4, line 5) in
  { x; y; vx; vy }

let parse (lines : string list) = List.map parse_line lines

(* Main function to read input and run the solution *)
let () =
  let solution = get_input day Input |> parse |> solve 101 103 in
  print_endline solution
