# 4-deriving-generators

One can take advantage of the `bam-ppx` library to derive automatically generators.

```ocaml
open Tezt_bam
open Bam.Std.Syntax

type t = A of {x: int; b: bool} | B of string [@@deriving gen] [@@size.min 1][@@size.max 10]

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
The attribute `[@@deriving gen]` derives automatically a generator for the type attached. The derived generator can be tuned via multiple options. To mimic the example showed in the previous example, we have specified that the minimum size and the maximum size that should be used are `1` and `10`.

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



