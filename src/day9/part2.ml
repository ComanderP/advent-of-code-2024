open Utils

let day = D9

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

type space = File of int | Free of int

let is_free array i = array.(i) = -1

(** Starting at [from], finds the first index in [array] that is not filled *)
let first_unfilled_index from array =
  let rec loop i =
    if i = Array.length array then None
    else if is_free array i then Some i
    else loop (succ i)
  in
  loop from

(** Checks if there is enough space in [array] starting at [from] for [size] *)
let enough_space size from array =
  let n = Array.length array in
  let rec loop i =
    if i = n then false
    else if i = from + size then true
    else is_free array i && loop (succ i)
  in
  loop from

let span_of_space right_index array =
  let c = array.(right_index) in
  let rec loop i =
    if i = 0 then (i, right_index)
    else if array.(i) <> c then (succ i, right_index)
    else loop (pred i)
  in
  loop right_index

let next_file_to_move from array =
  if from = Array.length array then
    let rec loop i =
      if i = -1 then None
      else if is_free array i then loop (pred i)
      else
        let left, right = span_of_space i array in
        Some (left, right)
    in
    loop (pred from)
  else
    let c = array.(from) in
    let rec loop i =
      if i = -1 then None
      else if is_free array i || array.(i) = c then loop (pred i)
      else
        let left, right = span_of_space i array in
        Some (left, right)
    in
    loop from

let find_suitable_space max size array =
  let n = Array.length array in
  let rec loop i =
    if i = n || i >= max then None
    else if enough_space size i array then Some i
    else
      match first_unfilled_index (succ i) array with
      | None -> None
      | Some i -> loop i
  in
  loop 0

let rev_iteri f array =
  let rec loop i =
    if i = -1 then ()
    else (
      f i array.(i);
      loop (pred i))
  in
  loop (pred (Array.length array))

let checksum i c = if c = -1 then 0 else i * c

let swap_space (left, right) space array =
  Array.fill array space (right - left + 1) array.(right);
  Array.fill array left (right - left + 1) (-1)

let solve input : string =
  let f =
   fun i _ ->
    match next_file_to_move i input with
    | None -> ()
    | Some (left, right) -> (
        let space = find_suitable_space left (right - left + 1) input in
        match space with
        | None -> ()
        | Some space -> swap_space (left, right) space input)
  in
  f (Array.length input) 0;
  rev_iteri f input;
  Array.fold_left ( + ) 0 (Array.mapi checksum input) |> string_of_int

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
  test2 day parse solve "2858";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
