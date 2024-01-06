open Tezt_bam

let register () =
  let gen = Std.int () in
  let property x =
    Format.eprintf "HEY: %d@." x ;
    if x < 100 && x <> 16 then Ok ()
    else Error (`Fail "integer is not smallar than 100")
  in
  Pbt.register ~pp:Format.pp_print_int ~__FILE__ ~title:"Debugging with bam"
    ~tags:["bam"; "debugging"] ~gen ~property ()
