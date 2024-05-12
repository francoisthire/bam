# 5-shrinking-strategy

Let's use our running example. As shown in Example
[3-writing-generators](https://github.com/francoisthire/bam/tree/master/example/3-writing-generators),
the default shrinking strategy does not allow us to find the smallest
counterexample.

For instance, the shrinking strategy identifies `A {x=1001; aaaz}` as a
counterexample (using seed `963166253` for example), while a better
counterexample could be `A {x=1001; z}`.

To understand this behavior, one must comprehend the default shrinker for
strings. The default shrinking strategy for the type `string` in the standard
library of *Bam* behaves similarly to the type `char list`. Hence, to understand
the shrinking strategy for string, one must first understand the shrinking
strategy for the type `char list`.

By default, the shrinking strategy for `'a list` reduces the list to a prefix
and then calls the shrinker for the type `'a`, in our case, `char`. The shrinker
for char generates only printable characters by default, and hence the smallest
character is `a`.

Let's assume that the initial counterexample found for the string was the string
`dpnzuf`. The shrinking strategy then finds a smaller counterexample by trying
all the prefixes: `d`, `dp`, `dpn` and `dpz`. Since `dpnz` is the first attempts
that produces an error, the shrinking goes on with this string. Then the
shrinking strategy will try to shrink all the characters in the list one by one
starting from the first character with the default shrinker for the type char.
Thus, `dpnz` is reduced to `apnz`, then `aanz`, and finally `aaaz`.

Note that if we replace `String.contains` with `String.starts_with` in the
example below, the shrinking strategy would always find the smallest
counterexample.

One of the strategies proposed by *Bam* to find better counterexamples with list
is to allow the shrinking to skip elements in the list.

For example, if we allow skipping one element in the list, the counterexample
found by *Bam* would be `aaz` instead of `aaaz`. Thus, to find the smallest
counterexample here, skipping 3 elements is sufficient.

Skipping elements in the list can substantially increase the time taken by the
shrinker to find a counterexample, as the number of attempts may increase
exponentially. *Bam* implements a heuristic to determine a reasonable number of
elements to skip. This approach is very experimental, which is why it is not
activated by default. We may revisit this choice based on user feedback.

Returning to our original example, this means that we could try using the `Skip
Auto` shrinking strategy instead.

The full explanation of how shrinking works in Bam can be found
[here](https://francoisthire.github.io/bam/bam/shrinking.html).

```ocaml
open Tezt_bam

(** The deriver creates a value [val gen : t Bam.Std.t]. *)
type t =
  | Foo of {a: int; b: (string[@shrinker Bam.Std.Shrinker.skip_auto])}
  | Bar of int list[@@deriving gen]

(* Equivalently, one could write the generator below. *)
let _gen =
  let open Bam.Std.Syntax in
  let gen_Foo =
    let* a = Bam.Std.int () in
    let* b =
      Bam.Std.string
        ~shrinker:Bam.Std.Shrinker.skip_auto
        ~size:(Bam.Std.int ~max:10 ()) ()
    in
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
  Pbt.register ~pp ~__FILE__ ~title:"Shrinking strategy"
    ~tags:["bam"; "shrinking_strategy"]
    ~gen ~property ()
```

And now run it:

```bash
$ dune exec example/main.exe -- shrinking_strategy --shrink --seed 963166253
[12:55:41.423] [warn] Leftover temporary file from previous run: /tmp/tezt-963881
[12:55:41.423] [warn] Leftover temporary file from previous run: /tmp/tezt-963911
[12:55:41.428] [pbt] \  | /
[12:55:41.428] [pbt] - BAM -
[12:55:41.428] [pbt]  / | \ 
[12:55:41.428] [pbt] Counter example found:
[12:55:41.428] [pbt] Foo {x=1001;z}
[12:55:41.428] [error] Test failed with error:
[12:55:41.428] [error] A counter-example was found
[12:55:41.429] [FAILURE] (1/1, 1 failed) Shrinking strategy
[12:55:41.429] Try again with: _build/default/example/main.exe --verbose --file example/5-shrinking-strategy/shrinking_strategy.ml --title 'Shrinking strategy' --seed 963166253
```

*Bam* provides another tool for improving the shrinking efficiency. One can use
the `--aggressive <int>`. By default, this value is set to 0, and the higher
this parameter is, the larger the space of values explored by Bam. The effect of
this parameter is detailed on the [primer on
shrinking](https://francoisthire.github.io/bam/bam/shrinking.html) in *bam*.
Tuning this parameter can yield better counterexamples, albeit with the downside
of requiring more time to find a smaller counterexample.

For this specific example, the parameter would not be very useful because the
default shrinking strategy does not skip list elements. Therefore, increasing
the aggressiveness would not yield a smaller counterexample. However, it can be
beneficial for values with an interval-like type such as int. In such cases,
because the default shrinker implements a binary search, increasing the
aggressiveness allows more values to be explored.


**Next steps:**
- The next example
  [6-debugging](https://github.com/francoisthire/bam/tree/master/example/6-debugging)
  will investigate how one can debug a PBT test with *Bam*.