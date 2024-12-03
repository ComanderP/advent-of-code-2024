open Utils

let day = D3

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

type op = Mul of int * int | Do | Don't

let multiply (b, sum) = function
  | Mul (x, y) -> (b, sum + (x * y * b))
  | Do -> (1, sum)
  | Don't -> (0, sum)

let solve input : string =
  List.fold_left multiply (1, 0) input |> snd |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let starts_with ~prefix seq =
  let prefix = String.to_seq prefix in
  let rec aux prefix seq =
    match (prefix (), seq ()) with
    | Seq.Nil, _ -> true
    | Seq.Cons (p, ps), Seq.Cons (s, ss) when p = s -> aux ps ss
    | _ -> false
  in
  aux prefix seq

let list_to_str l = List.to_seq l |> String.of_seq

let rec parse_num acc (seq : char Seq.t) =
  match seq () with
  | Cons (')', rest) | Cons (',', rest) ->
      let num = List.rev acc |> list_to_str |> int_of_string in
      Some (num, rest)
  | Cons (('0' .. '9' as c), rest) -> parse_num (c :: acc) rest
  | _ -> None

let parse_line (line : string) : op list =
  let line = String.to_seq line in
  let rec parse_op acc seq =
    if starts_with ~prefix:"do()" seq then parse_op (Do :: acc) (Seq.drop 4 seq)
    else if starts_with ~prefix:"don't()" seq then
      parse_op (Don't :: acc) (Seq.drop 7 seq)
    else if starts_with ~prefix:"mul(" seq then
      let x = parse_num [] (Seq.drop 4 seq) in
      if x = None then parse_op acc (Seq.drop 4 seq)
      else
        let x, rest = Option.get x in
        let y = parse_num [] rest in
        if y = None then parse_op acc rest
        else
          let y, rest = Option.get y in
          parse_op (Mul (x, y) :: acc) rest
    else if seq () = Seq.Nil then List.rev acc
    else parse_op acc (Seq.drop 1 seq)
  in
  parse_op [] line

let parse (lines : string list) = lines |> List.map parse_line |> List.concat

(* Main function to read input and run the solution *)
let () =
  test2 day parse solve "48";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
