open Tezt_bam

let register () =
  let gen = Std.int () in
  let property x =
    if x < 100 then Ok () else Error (`Fail "integer is not smallar than 100")
  in
  Pbt.register ~pp:Format.pp_print_int ~__FILE__
    ~title:"Simple failure example with bam" ~tags:["bam"; "simple_failure"]
    ~gen ~property ()
