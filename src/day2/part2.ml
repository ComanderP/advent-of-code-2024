open Utils

let day = D2

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

let rec levels = function
  | [] -> []
  | hd :: tl -> tl :: List.map (fun x -> hd :: x) (levels tl)

let rec slide2 f = function
  | x :: y :: tl -> f x y && slide2 f (y :: tl)
  | _ -> true

let is_increasing l = slide2 ( < ) l
let is_decreasing l = slide2 ( > ) l

let is_within_bounds x y =
  let diff = Int.abs (x - y) in
  diff > 0 && diff < 4

let differ = slide2 is_within_bounds
let rec any f = function [] -> false | hd :: tl -> f hd || any f tl

let count_safe acc l =
  let is_safe =
    levels l
    |> List.map (fun x -> (is_increasing x || is_decreasing x) && differ x)
    |> any (( = ) true)
  in
  acc + Bool.to_int is_safe

let solve input : string = input |> List.fold_left count_safe 0 |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)
let parse (lines : string list) =
  lines
  |> List.map (String.split_on_char ' ')
  |> List.map (List.map int_of_string)

(* Main function to read input and run the solution *)
let () =
  test1 day parse solve "4";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
