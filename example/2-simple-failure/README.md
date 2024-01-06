# 2-simple-failure

Let's try *BAM* when a failure arises.

```ocaml
open Tezt_bam

let register () =
  let gen = Std.int () in
  let property x =
    if x < 100 then Ok () else Error (`Fail "integer is not smallar than 100")
  in
  Pbt.register ~pp:Format.pp_print_int ~__FILE__
    ~title:"Simple failure example with bam" ~tags:["bam"; "simple_failure"]
    ~gen ~property ()
```

Do note this time we have provided an optional parameter `pp` to enable `Tezt` to print the counter-example found.

You can run the example as follows:

```bash
dune exec example/main.exe -- simple_failure
[15:30:55.229] [pbt] \  | /
[15:30:55.229] [pbt] - BAM -
[15:30:55.229] [pbt]  / | \ 
[15:30:55.229] [pbt] Please run the test again with option --shrink to get a smaller counter-example
[15:30:55.229] [pbt] Counter example found:
[15:30:55.229] [pbt] 323253551143260932
[15:30:55.229] [error] Test failed with error:
[15:30:55.229] [error] integer is not smallar than 100
[15:30:55.229] [FAILURE] (1/1, 1 failed) Simple failure example with bam
[15:30:55.229] Try again with: _build/default/example/main.exe --verbose --file example/2-simple-failure/simple_failure.ml --title 'Simple failure example with bam' --seed 555586205
```

This time `Tezt` is more verbose. On a failure, the counter-example
found is printed. By default, shrinking is not enabled, hence the
counter-example found can be rather big.

Moreover, the error raised associated to the counter-example is
printed.

If you want to run this example again using the shrinking of bam, you can use the option `--shrink`. To be sue the very same initial counter-example will be printed, you can use the seed given by Tezt:

```ocaml
$ dune exec example/main.exe -- simple_failure --shrink --seed 555586205
[15:36:00.803] [pbt] \  | /         
[15:36:00.803] [pbt] - BAM -
[15:36:00.803] [pbt]  / | \ 
[15:36:00.803] [pbt] Counter example found:
[15:36:00.803] [pbt] 100
[15:36:00.803] [error] Test failed with error:
[15:36:00.803] [error] integer is not smallar than 100
[15:36:00.803] [FAILURE] (1/1, 1 failed) Simple failure example with bam
[15:36:00.803] Try again with: _build/default/example/main.exe --verbose --file example/2-simple-failure/simple_failure.ml --title 'Simple failure example with bam' --seed 555586205
```

At this stage, we see the shrinking heuristic allowed us to find a
smaller-counter example which is `100`. Such a counter-example can be
a good candidate for starting debugging the test.

**Next steps:**
- The next example
  [3-debugging](https://github.com/francoisthire/bam/tree/master/example/3-debugging)
  will investigate how bam can help for debugging
