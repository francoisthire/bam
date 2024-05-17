# 3-writing-generators

One can take advantage of the monadic interface of *bam* to define
generators.

```ocaml
open Tezt_bam

(** The deriver creates a value [val gen : t Bam.Std.t]. *)
type t = Foo of {a: int; b: string} | Bar of int list [@@deriving gen]

let gen =
  let open Bam.Std.Syntax in
  let gen_Foo =
    let* a = Bam.Std.int () in
    let* b = Bam.Std.string ~size:(Bam.Std.int ~max:10 ()) () in
    return (Foo {a; b})
  in
  let gen_Bar =
    let* arg_0 = Bam.Std.list ~size:(Bam.Std.int ~max:10 ()) (Bam.Std.int ()) in
    return (Bar arg_0)
  in
  Bam.Std.oneof [(1, gen_Foo); (1, gen_Bar)]

let pp fmt = function
  | Foo {a; b} ->
      Format.fprintf fmt "Foo {x=%d;%s}" a b
  | Bar list ->
      Format.fprintf fmt "Bar %a"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_int)
        list

let register () =
  let property = function
    | Foo {a; b} ->
        if a > 1_000 && String.contains b 'z' then
          Error (`Fail "A counter-example was found")
        else Ok ()
    | Bar [1; 2; 3; 4] ->
        Error `Bad_value
    | Bar _ ->
        Ok ()
  in
  Pbt.register ~pp ~__FILE__ ~title:"Writing generators"
    ~tags:["bam"; "writing_generators"]
    ~gen ~property ()
```

And now run it:

```bash
$ dune exec example/main.exe -- writing_generators --shrink
[11:31:02.335] [pbt] \  | /           
[11:31:02.335] [pbt] - BAM -
[11:31:02.335] [pbt]  / | \ 
[11:31:02.335] [pbt] Counter example found:
[11:31:02.335] [pbt] A {x=1001;aaaaz}
[11:31:02.335] [error] Test failed with error:
[11:31:02.335] [error] A counter-example was found
[11:31:02.335] [FAILURE] (1/1, 1 failed) Writing generators
[11:31:02.335] Try again with: _build/default/example/main.exe --verbose --file example/4-writing-generators/writing_generators.ml --title 'Writing generators' --seed 493027010
```

With *bam* it is encouraged to extensively use and abuse of the
monadic interface. The library ensures it will behave well with
respect to shrinking.

However, one can notice that *bam* did not find the smallest counter-example on this example. The reason is due to the default shrinking strategy which is efficient. Fortunately, *bam* provides different shrinking strategies to help you.

**Next steps:**
- The next example
  [4-deriving-generators](https://github.com/francoisthire/bam/tree/master/example/4-deriving-generators)
  will investigate how `bam-ppx` can be used to generate derivers automatically.
- The next example
  [5-shrinking-strategy](https://github.com/francoisthire/bam/tree/master/example/5-shrinking-strategy)
  will investigate how we can modify the shrinking strategies to find better counter-examples.  
