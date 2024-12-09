let mark tests =
  Command_unix.run
    Core_bench.(
      Bench.make_command
        (List.map
           (fun test -> Bench.Test.create ~name:(fst test) (snd test))
           tests))
