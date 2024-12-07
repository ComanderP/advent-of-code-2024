open Utils

let day = D7

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

let possible_solutions (nums : int list) : int list =
  let rec aux acc = function
    | [] -> acc
    | [ n ] -> n :: acc
    | n1 :: n2 :: tl ->
        let acc = aux acc ((n1 + n2) :: tl) in
        aux acc ((n1 * n2) :: tl)
  in
  aux [] nums

let solve input : string =
  List.filter (fun (res, nums) -> List.mem res (possible_solutions nums)) input
  |> List.split |> fst |> sum |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let equations = function
  | res :: _ :: numbers -> (int_of_string res, List.map int_of_string numbers)
  | _ -> failwith "Invalid input"

let parse (lines : string list) =
  lines |> List.map (split_on_chars [ ':'; ' ' ]) |> List.map equations

(* Main function to read input and run the solution *)
let () =
  test1 day parse solve "3749";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
