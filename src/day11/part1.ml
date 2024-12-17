open Utils

let day = D11

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

let even_digits stone = String.length stone mod 2 = 0

let trim stone =
  let seq = String.to_seq stone |> Seq.drop_while (fun c -> c = '0') in
  if Seq.is_empty seq then "0" else String.of_seq seq

let split stone =
  let len = String.length stone in
  let half = len / 2 in
  (String.sub stone 0 half, String.sub stone half half)

let replace_stone stone = stone |> int_of_string |> ( * ) 2024 |> string_of_int

let[@tail_mod_cons] rec blink = function
  | [] -> []
  | "0" :: tl -> "1" :: blink tl
  | stone :: tl when even_digits stone ->
      let left, right = split stone in
      left :: trim right :: blink tl
  | stone :: tl -> replace_stone stone :: blink tl

let rec repeat n f x = if n = 0 then x else repeat (n - 1) f (f x)

let solve input : string =
  let input = repeat 25 blink input in
  List.length input |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let parse (lines : string list) = lines |> List.hd |> String.split_on_char ' '

(* Main function to read input and run the solution *)
let () =
  test1 day parse solve "55312";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
