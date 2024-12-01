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
  let list1 = List.sort Int.compare list1 in
  let list2 = List.sort Int.compare list2 in
  let res =
    List.fold_left (fun acc n1 -> acc + (n1 * count n1 list2)) 0 list1
  in
  string_of_int res

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
