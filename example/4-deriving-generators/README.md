# 4-deriving-generators

One can take advantage of the `bam-ppx` library to derive automatically generators.

```ocaml
open Tezt_bam

type t = A of {x: int; b: bool} | B of string [@weight 4]
[@@deriving gen] [@@size.min 1] [@@size.max 10]

let pp fmt = function
  | A {x; b} ->
      Format.fprintf fmt "A {x=%d;%b}" x b
  | B str ->
      Format.fprintf fmt "B %s" str

let register () =
  let property = function
    | B str ->
        if String.get str 0 = 'c' then Ok ()
        else Error (`Fail "string did not start with 'c'")
    | A _ ->
        Ok ()
  in
  Pbt.register ~pp ~__FILE__ ~title:"Deriving generators"
    ~tags:["bam"; "deriving_generators"]
    ~gen ~property ()
```
The attribute `[@@deriving gen]` derives automatically a generator for the type
attached. The generator derived with `gen` can be tuned via other `attributes`
as shown in the example above. In this example, we have specified that the
minimum size and the maximum size that should be used are `1` and `10`. And that
the weight of the constructor `B` if `4` (by default it is `1`). Meaning that
the variant `B` will be generated `4` times more often than variant `A`.

More examples can be found
[here](https://github.com/francoisthire/bam/blob/master/test/ppx.ml) to
understand how the deriver can be tuned.

And now if your un it:

```bash
$ dune exec example/main.exe -- deriving_generators --shrink
[23:37:37.020] [pbt] \  | /           
[23:37:37.020] [pbt] - BAM -
[23:37:37.020] [pbt]  / | \ 
[23:37:37.020] [pbt] Counter example found:
[23:37:37.020] [pbt] B a
[23:37:37.020] [error] Test failed with error:
[23:37:37.020] [error] string did not start with 'c'
[23:37:37.020] [FAILURE] (1/1, 1 failed) Deriving generators
[23:37:37.020] Try again with: _build/default/example/main.exe --verbose --file example/5-deriving-generators/deriving_generators.ml --title 'Deriving generators' --seed 268040292
```

**Next steps:**
- The next example
  [5-shrinking-strategy](https://github.com/francoisthire/bam/tree/master/example/5-shrinking-strategy)
  will investigate how we can modify the shrinking strategies to find better counter-examples.  
- The next example
  [6-debugging](https://github.com/francoisthire/bam/tree/master/example/6-debugging)
  will investigate how *tezt-bam* can help for debugging

