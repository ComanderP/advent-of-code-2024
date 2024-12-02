open Utils

let day = D1

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

let sort = List.sort Int.compare

let solve (list1, list2) : string =
  let sumDistance acc n1 n2 = acc + Int.abs (n2 - n1) in
  List.fold_left2 sumDistance 0 (sort list1) (sort list2) |> string_of_int

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
  test1 day parse solve "11";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
