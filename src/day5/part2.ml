open Utils

let day = D5

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

let rec is_after x y = function
  | [] -> false
  | hd :: tl when hd = y -> List.find_opt (( = ) x) tl |> Option.is_some
  | _ :: tl -> is_after x y tl

and is_before x y = is_after y x

let part_of_rule page rule = page = fst rule || page = snd rule

let is_correctly_ordered seq page (prev, next) =
  match page with
  | page when page = next ->
      if List.mem prev seq then is_after page prev seq else true
  | page when page = prev ->
      if List.mem next seq then is_before page next seq else true
  | _ -> true

let is_page_in_order seq rules page =
  let rules = List.filter (part_of_rule page) rules in
  List.for_all (is_correctly_ordered seq page) rules

let correctly_ordered rules pages =
  List.for_all (is_page_in_order pages rules) pages

let middle list = List.nth list (List.length list / 2)
let sum_middle acc seq = acc + middle seq

let compare rules x y =
  if List.mem (y, x) rules then 1 else if List.mem (x, y) rules then -1 else 0

let fix_order rules = List.sort (compare rules)

let solve input =
  let rules, (pages : int list list) = input in
  List.filter (fun seq -> not (correctly_ordered rules seq)) pages
  |> List.map (fix_order rules)
  |> List.fold_left sum_middle 0
  |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let split lines : string list * string list =
  let rec aux acc = function
    | "" :: t -> (List.rev acc, t)
    | h :: t -> aux (h :: acc) t
    | [] -> assert false
  in
  aux [] lines

let parse_rule line =
  match String.split_on_char '|' line with
  | [ a; b ] -> (int_of_string a, int_of_string b)
  | _ -> assert false

let parse_pages line = String.split_on_char ',' line |> List.map int_of_string

let parse (lines : string list) =
  let rules, pages = split lines in
  let rules = List.map parse_rule rules in
  let pages = List.map parse_pages pages in
  (rules, pages)

(* Main function to read input and run the solution *)
let () =
  test2 day parse solve "123";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
