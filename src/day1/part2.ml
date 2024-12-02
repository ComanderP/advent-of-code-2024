open Utils

let day = D1

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

let rec count n list =
  match list with
  | [] -> 0
  | hd :: tl ->
      let x = if hd = n then 1 else 0 in
      x + count n tl

let solve (list1, list2) : string =
  let sum_similarity acc n = acc + (n * count n list2) in
  List.fold_left sum_similarity 0 list1 |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let tuple2 = function [ x; y ] -> (x, y) | _ -> assert false

let parse (lines : string list) =
  List.(
    lines
    |> map (String.split_on_char ' ')
    |> map (filter_map int_of_string_opt)
    |> map tuple2 |> split)

(* Main function to read input and run the solution *)
let () =
  test2 day parse solve "31";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
