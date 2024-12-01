open Utils

let day = D1

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)
let solve (list1, list2) : string =
  let list1 = List.sort Int.compare list1 in
  let list2 = List.sort Int.compare list2 in
  let res =
    List.fold_left2 (fun acc n1 n2 -> acc + Int.abs (n2 - n1)) 0 list1 list2
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
  test1 day parse solve "11";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
