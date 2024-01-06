# 1-simple

A simple example on how to use the library.

```ocaml
open Tezt_bam

let register () =
  let gen = Std.int () in
  let property _x = Ok () in
  Pbt.register ~__FILE__ ~title:"Simple example of bam" ~tags:["bam"; "simple"]
    ~gen ~property ()	
```

You can run the example with:

```bash
$ dune exec example/main.exe -- simple
[14:05:37.035] [SUCCESS] (1/1) Simple example of bam
```

The test run successfully!

**Next steps:**
- The next example
  [2-simple-failure](https://github.com/francoisthire/bam/tree/master/example/2-simple-failure)
  will investigate how to debug a test when the property fails.

