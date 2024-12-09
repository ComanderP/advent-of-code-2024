open Utils

let day = D9

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

type space = File of int | Free of int

let last_filled_index array last_empty_index =
  let i = ref (pred last_empty_index) in
  while array.(!i) = -1 do
    i := pred !i
  done;
  !i

let compact input =
  Array.fold_left
    (fun ((cur, last), acc) x ->
      if cur = last then ((cur, last), x :: acc)
      else if x = -1 then (
        let x = input.(last) in
        input.(last) <- -1;
        ((succ cur, last_filled_index input last), x :: acc))
      else ((succ cur, last), x :: acc))
    ((0, last_filled_index input (Array.length input)), [])
    input
  |> snd |> List.rev

let checksum i c = if c = -1 then 0 else i * c

let solve input : string =
  compact input |> List.mapi checksum |> sum |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let rec prepend element list = function
  | 0 -> list
  | n -> element :: prepend element list (n - 1)

let uncompress disk_map =
  Seq.fold_left
    (fun (id, acc) -> function
      | File x -> (succ id, prepend id acc x)
      | Free x -> (id, prepend (-1) acc x))
    (0, []) disk_map
  |> snd |> List.rev |> Array.of_list

let parse (lines : string list) =
  let disk_map = List.hd lines |> String.to_seq |> Seq.map int_of_char_digit in
  Seq.mapi (fun i x -> if i mod 2 = 0 then File x else Free x) disk_map
  |> uncompress

(* Main function to read input and run the solution *)
let () =
  test1 day parse solve "1928";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
