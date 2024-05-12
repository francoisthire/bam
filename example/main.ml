open Tezt_bam

let () =
  Simple.register () ;
  Simple_failure.register () ;
  Debugging.register () ;
  Writing_generators.register () ;
  Deriving_generators.register () ;
  Shrinking_strategy.register () ;
  Test.run ()
