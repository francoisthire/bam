open Tezt

let () =
  Tree.register () ;
  Gen.register () ;
  Std.register () ;
  Pbt.register () ;
  Test.run ()
