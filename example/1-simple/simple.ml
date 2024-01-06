open Tezt_bam

let register () =
  let gen = Std.int () in
  let property _x = Ok () in
  Pbt.register ~__FILE__ ~title:"Simple example of bam" ~tags:["bam"; "simple"]
    ~gen ~property ()
