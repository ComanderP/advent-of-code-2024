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

let extract_input (input : int list list) =
  let rec aux input acc1 acc2 =
    match input with
    | [] -> (acc1, acc2)
    | [ i1; i2 ] :: tl -> aux tl (i1 :: acc1) (i2 :: acc2)
    | _ -> assert false
  in
  aux input [] []

let parse (lines : string list) =
  lines
  |> List.map (String.split_on_char ' ')
  |> List.map (List.filter_map int_of_string_opt)
  |> extract_input

(* Main function to read input and run the solution *)
let () =
  test2 day parse solve "31";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
