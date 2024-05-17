# 6-debugging

Let's take a simple example and let's add a `printf` inside the property function:

```ocaml
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
```

And now run it:

```bash
$ dune exec example/main.exe -- debugging --shrink
HEY: 1531078954999384395            
[15:46:01.805] [pbt] \  | /
[15:46:01.805] [pbt] - BAM -
[15:46:01.805] [pbt]  / | \ 
[15:46:01.805] [pbt] Counter example found:
[15:46:01.805] [pbt] 100
[15:46:01.805] [error] Test failed with error:
[15:46:01.805] [error] integer is not smallar than 100
[15:46:01.805] [FAILURE] (1/1, 1 failed) Debugging with bam
[15:46:01.805] Try again with: _build/default/example/main.exe --verbose --file example/3-debugging/debugging.ml --title 'Debugging with bam' --seed 830377664
```

The property function is run numerous times for all the various attempts but
also because of the shrinking. However, only one line appear on stdout. The
reason is that by default, **BAM** captures stdout/stderr and run one last time
the test on the smallest counter-example found. This is to ease debugging so
that what is seen is only relevant output on the counter-example found. Such
behaviour can be relaxed by specifying the option `--no-capture`:

```bash
saroupille@saroupille-debian:~/Git/bam$ dune exec example/main.exe -- debugging --shrink --no-capture
HEY: 3365002382978735690
HEY: 0
HEY: 1
HEY: 2
HEY: 4
HEY: 8
HEY: 16
HEY: 9
HEY: 10
HEY: 11
HEY: 13
HEY: 15
HEY: 16
[16:08:35.821] [pbt] \  | /
[16:08:35.821] [pbt] - BAM -
[16:08:35.821] [pbt]  / | \ 
[16:08:35.821] [pbt] Counter example found:
[16:08:35.821] [pbt] 16
[16:08:35.821] [error] Test failed with error:
[16:08:35.821] [error] integer is not smallar than 100
[16:08:35.821] [FAILURE] (1/1, 1 failed) Debugging with bam
[16:08:35.821] Try again with: _build/default/example/main.exe --verbose --file example/3-debugging/debugging.ml --title 'Debugging with bam' --seed 353027053
```

We see that during the shrinking, a lot of values are tested. Since
[bam] runs twice the property on the counter-example, this explains
why we see twice `16`. But do note that the first time `16` is
executed other attempts are made to find a smaller counter-example.

**Next steps:**
- The next example
  [4-writing-generators](https://github.com/francoisthire/bam/tree/master/example/4-writing-generators)
  will investigate how bam can help
