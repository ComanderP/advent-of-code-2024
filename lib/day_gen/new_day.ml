let empty_file path file =
  Out_channel.with_open_text (path ^ file) (fun _ -> ())

let create_input_files day =
  let path = "data/day" ^ day ^ "/" in
  let create_input = empty_file path in
  create_input "input.txt";
  create_input "ex1.txt";
  create_input "ex2.txt"

let replace string template replacement =
  let regex = Re.compile (Re.str template) in
  Re.replace_string regex ~by:replacement string

let apply_replacement s (template, replacement) = replace s template replacement

let create_template_file template temp_repl fn =
  In_channel.with_open_text ("lib/day_gen/" ^ template) (fun ic ->
      let template = In_channel.input_all ic in
      let content = List.fold_left apply_replacement template temp_repl in
      Out_channel.with_open_text
        ("src/day" ^ Sys.argv.(1) ^ "/" ^ fn)
        (fun oc -> Out_channel.output_string oc content))

let () =
  if Array.length Sys.argv <> 2 then failwith "Usage: new_day <day>\n";
  let day = int_of_string Sys.argv.(1) in
  if day < 1 || day > 25 then failwith "Day must be between 1 and 25";

  (* Input folder *)
  Sys.mkdir ("data/day" ^ Sys.argv.(1)) 0o755;
  create_input_files Sys.argv.(1);
  (* Source code *)
  Sys.mkdir ("src/day" ^ Sys.argv.(1)) 0o755;

  create_template_file "dune_template" [ ("<day>", Sys.argv.(1)) ] "dune";

  create_template_file "template"
    [ ("<day>", Sys.argv.(1)); ("<i>", "1") ]
    "part1.ml";

  create_template_file "template"
    [ ("<day>", Sys.argv.(1)); ("<i>", "2") ]
    "part2.ml";

  Printf.printf "Day %s created\n" Sys.argv.(1)
