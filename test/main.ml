open Tezt

let () =
  Tree.register () ;
  Gen.register () ;
  Std.register () ;
  Pbt.register () ;
  Ppx.register () ;
  Test.run ()
