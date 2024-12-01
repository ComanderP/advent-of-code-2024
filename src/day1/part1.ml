open Utils

let day = D1

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)
let solve input : string = assert false

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)
let parse (lines : string list) = assert false

(* Main function to read input and run the solution *)
let () =
  test1 day parse solve "TODO";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
