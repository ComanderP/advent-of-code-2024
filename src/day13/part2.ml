open Utils

let day = D13

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

type coords = { x : int; y : int }
type crane_game = { button_a : coords; button_b : coords; prize : coords }

let coefficients { button_a; button_b; prize } =
  let a =
    ((prize.x * button_b.y) - (button_b.x * prize.y))
    / ((button_a.x * button_b.y) - (button_b.x * button_a.y))
  in
  let b =
    ((button_a.x * prize.y) - (prize.x * button_a.y))
    / ((button_a.x * button_b.y) - (button_b.x * button_a.y))
  in
  if
    (a * button_a.x) + (b * button_b.x) = prize.x
    && (a * button_a.y) + (b * button_b.y) = prize.y
  then Some (a, b)
  else None

let solve input : string =
  List.filter_map coefficients input
  |> List.map (fun (a, b) -> (3 * a) + b)
  |> sum |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let button_of_string s =
  let s = split_on_chars [ '+'; ',' ] s in
  let x = int_of_string @@ List.nth s 1 in
  let y = int_of_string @@ List.nth s 3 in
  { x; y }

let prize_of_string s =
  let s = split_on_chars [ ','; '=' ] s in
  let x = int_of_string @@ List.nth s 1 in
  let y = int_of_string @@ List.nth s 3 in
  { x; y }

let rec cranes lines : crane_game list =
  match lines with
  | [] -> []
  | "" :: tl -> cranes tl
  | a :: b :: prize :: tl ->
      let button_a = button_of_string a in
      let button_b = button_of_string b in
      let { x; y } = prize_of_string prize in
      let prize = { x = x + 10000000000000; y = y + 10000000000000 } in
      { button_a; button_b; prize } :: cranes tl
  | _ -> assert false

let parse (lines : string list) = cranes lines

(* Main function to read input and run the solution *)
let () =
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
