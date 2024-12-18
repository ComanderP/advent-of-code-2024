open Utils

let day = D14

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

type robot = { x : int; y : int; vx : int; vy : int }

let new_coord coord bound =
  let v = abs coord mod bound in
  if v = 0 then v else if coord < 0 then bound - v else v

let position_in width height n robot =
  let x = robot.x + (n * robot.vx) in
  let y = robot.y + (n * robot.vy) in
  (new_coord x width, new_coord y height)

let filter_list w h positions f1 f2 =
  List.filter (fun (x, y) -> f1 x (w / 2) && f2 y (h / 2)) positions

let quadrants w h positions =
  let filter_for_quadrant = filter_list w h positions in
  let q1 = filter_for_quadrant ( < ) ( < ) in
  let q2 = filter_for_quadrant ( > ) ( < ) in
  let q3 = filter_for_quadrant ( < ) ( > ) in
  let q4 = filter_for_quadrant ( > ) ( > ) in
  (q1, q2, q3, q4)

let apply4 f (a, b, c, d) = (f a, f b, f c, f d)
let mult4 (a, b, c, d) = a * b * c * d

let solve width height input : string =
  input
  |> List.map (position_in width height 100)
  |> quadrants width height |> apply4 List.length |> mult4 |> string_of_int

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
  test1 day parse (solve 11 7) "12";
  let solution = get_input day Input |> parse |> solve 101 103 in
  print_endline solution
