# 4-writing-generators

One can take advantage of the monadic interface of *bam* to define
generators.

```ocaml
open Tezt_bam

type t = A of {x: int; b: bool} | B of string

let pp fmt = function
  | A {x; b} ->
      Format.fprintf fmt "A {x=%d;%b}" x b
  | B str ->
      Format.fprintf fmt "B %s" str

let register () =
  let gen =
    let open Std.Syntax in
    let a =
      let* x = Std.int () in
      let* b = Std.bool () in
      return (A {x; b})
    in
    let b =
      let* str = Std.string ~size:(Std.int ~min:1 ~max:10 ()) () in
      return (B str)
    in
    Std.oneof [(1, a); (1, b)]
  in
  let property = function
    | B str ->
        if String.get str 0 = 'c' then Ok ()
        else Error (`Fail "string did not start with 'c'")
    | A _ ->
        Ok ()
  in
  Pbt.register ~pp ~__FILE__ ~title:"Writing generators"
    ~tags:["bam"; "writing_generators"]
    ~gen ~property ()
```

And now run it:

```bash
$ dune exec example/main.exe -- writing_generators --shrink
[16:27:21.716] [pbt] \  | /         
[16:27:21.716] [pbt] - BAM -
[16:27:21.716] [pbt]  / | \ 
[16:27:21.716] [pbt] Counter example found:
[16:27:21.716] [pbt] B a
[16:27:21.716] [error] Test failed with error:
[16:27:21.716] [error] string did not start with 'c'
[16:27:21.716] [FAILURE] (1/1, 1 failed) Writing generators
```

With *bam* it is encouraged to extensively use and abuse of the
monadic interface. The library ensures it will behave well with
respect to shrinking.


