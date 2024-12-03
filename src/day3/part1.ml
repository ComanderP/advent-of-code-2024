open Utils

let day = D3

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

type op = Mul of int * int

let solve input : string =
  List.fold_left (fun acc (Mul (x, y)) -> acc + (x * y)) 0 input
  |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let list_to_str l = List.to_seq l |> String.of_seq

let rec parse_num acc seq =
  match seq () with
  | Seq.Cons (')', rest) | Cons (',', rest) ->
      let num = List.rev acc |> list_to_str |> int_of_string in
      Some (num, rest)
  | Cons (('0' .. '9' as c), rest) -> parse_num (c :: acc) rest
  | _ -> None

let parse_line (line : string) : op list =
  let line = String.to_seq line in
  let rec parse_op acc seq =
    match Seq.take 4 seq |> String.of_seq with
    | "mul(" ->
        let x = parse_num [] (Seq.drop 4 seq) in
        if x = None then parse_op acc (Seq.drop 4 seq)
        else
          let x, rest = Option.get x in
          let y = parse_num [] rest in
          if y = None then parse_op acc rest
          else
            let y, rest = Option.get y in
            parse_op (Mul (x, y) :: acc) rest
    | "" -> acc
    | _ -> parse_op acc (Seq.drop 1 seq)
  in
  parse_op [] line

let parse (lines : string list) = lines |> List.map parse_line |> List.concat

(* Main function to read input and run the solution *)
let () =
  test1 day parse solve "161";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
