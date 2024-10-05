(** @closed *)
include module type of Tezt

(** @closed *)
include module type of Bam

module Pbt : sig
  (** This module declares a unique function to register a new test
      within Tezt. A typical way of using this function is as follows:
      {[
      type t = A | B | C
      
      let my_test =
      let gen =
      let open Std.Syntax in
        let gen =
          let* i = Std.int ~max:3 () in
          (if i = 0 then A else if i = 1 then B else C) |> return
        in
        Std.list ~size:(Std.int ~max:10 ()) gen
      in
      let property l =
        List.length l < 5 && List.mem A l
      in
      Pbt.register ~__FILE__ ~title:"Simple test" ~tags:["pbt";"simple"] ~gen ~property ()
      ]}

      Running this example with Tezt would give:

      {[
[18:06:14.776] [pbt] Please run the test again with option --shrink to get a minimal example.
[18:06:14.776] [pbt] Counter example found:
[18:06:14.776] [pbt] C B B C A C
[18:06:14.776] [pbt] Error:
[18:06:14.776] [pbt] Counter-example was found
[18:06:14.776] [pbt] Runtime statistics (before shrinking):
[18:06:14.776] [pbt] Execution time: 1.28ms
[18:06:14.776] [pbt] Minimum execution time: 737ns
[18:06:14.776] [pbt] Maximum execution time: 1.25ms
[18:06:14.776] [pbt] Average execution time: 255μs
[18:06:14.776] [pbt] Number of executions (without shrinking): 5
[18:06:14.776] [pbt] Number of distinct samples (not producing 'Bad_value' error): 5
[18:06:14.776] [pbt] sampling ratio: 1.000000
[18:06:14.776] [pbt] Running property on the smaller counter example found...
[18:06:14.776] [pbt] Total execution time: 1.35ms
[18:06:14.776] [error] Test failed with error:
[18:06:14.776] [error] Counter-example was found
[18:06:14.776] [FAILURE] (1/1, 1 failed) Simple test
[18:06:14.776] Try again with: _build/default/test/main.exe --verbose --file test/pbt.ml --title 'Simple test' --seed 227649358
      ]}

      Following the suggestion provided, if we run the same test with
      option [--shrink] and option [--seed 227649358] to get the same
      counter-example we get this time:

      {[
[18:06:37.025] Starting test: Simple test
[18:06:37.025] [pbt] Start searching for a counter example...
[18:06:37.026] [pbt] Counter example found:
[18:06:37.026] [pbt] C B B C A C
[18:06:37.026] [pbt] Error:
[18:06:37.026] [pbt] Counter-example was found
[18:06:37.026] [pbt] Runtime statistics (before shrinking):
[18:06:37.026] [pbt] Execution time: 1.97ms
[18:06:37.026] [pbt] Minimum execution time: 1.75μs
[18:06:37.026] [pbt] Maximum execution time: 1.93ms
[18:06:37.026] [pbt] Average execution time: 394μs
[18:06:37.026] [pbt] Number of executions (without shrinking): 5
[18:06:37.026] [pbt] Number of distinct samples (not producing 'Bad_value' error): 5
[18:06:37.026] [pbt] sampling ratio: 1.000000
[18:06:37.026] [pbt] Start shrinking...
[18:06:37.027] [pbt] Smaller counter example found:
[18:06:37.027] [pbt] B B
[18:06:37.027] [pbt] Running property on the smaller counter example found...
[18:06:37.027] [pbt] Total execution time: 2.13ms
[18:06:37.027] [error] Test failed with error:
[18:06:37.027] [error] Counter-example was found
[18:06:37.027] [FAILURE] (1/1, 1 failed) Simple test
[18:06:37.027] Try again with: _build/default/test/main.exe --verbose --file test/pbt.ml --title 'Simple test' --seed 227649358
      ]}
      The framework did not find the smalletest counter-example. We can make the default shrinking strategy more aggressive. For example by changing the generator as dollows:
      {[
  let gen =
    let open Std.Syntax in
    let gen =
      let* i = Std.int ~max:3 () in
      (if i = 0 then A else if i = 1 then B else C) |> return
    in
    Std.list ~shrinker:(Skip `Auto) ~size:(Std.int ~max:10 ()) gen
      ]}
      Please, refers to {!Bam.Std.Shrinker.Skip} for more detailed explanations.
*)

  val register :
       ?hash:('a -> int)
    -> ?pp:(Format.formatter -> 'a -> unit)
    -> ?expected_sampling_ratio:float
    -> ?minimum_number_of_samples:int
    -> ?no_stats:bool
    -> ?regression:'a list
    -> ?stop_after:[`Timeout of float | `Count of int | `Loop]
    -> ?on_sample:('a -> unit)
    -> __FILE__:string
    -> title:string
    -> tags:string list
    -> gen:'a Gen.t
    -> property:('a -> ('b, [`Fail of string | `Bad_value]) Result.t)
    -> unit
    -> unit
  (** [register ?hash ?pp ?expected_sampling_ratio
    ?minimum_number_of_samples ?regression ?stop_after ?on_sample gen property]
    runs the [property] on random values generated with [gen].

    The property is run on as many samples as possible depending on
    [stop_after]. [gen] is called any time a new simple must be
    provided.

    The seed used to generate samples is controlled by Tezt (see
    {!Test.register}). This function ensures that if the test
    is run twice with the same seed, then the same values will be
    generated.

    The [property] will be called on each generated sample produced by
    [gen]. It does so in an environment where the output on both
    [stdout] and [stderr] is captured by default. Except for the
    counter-example found for which, [property] is run again without
    capturing anything. This behaviour should ease the debugging
    (especially hello debugging).

    The test can fail in the following cases:

    1) A counter example was found

    2) The last run with the counter example succeeded
    
    3) Not enough samples were generated

    The first case is expected. The reason for the second case, is
    likely related to non-determinism of the property since the test
    was run twice with the same generated value. For the third case,
    this is to ensure the property is run on many samples as detailed
    below.

    One issue with proerty-based testing is that it is not easy to
    ensure that the generator and the property are run an a large
    range of values. The default behaviour of this register function
    is to prevent such a case to occur. To do so, two parameters are
    used: [expected_sampling_ratio] and[minimum_number_of_samples].

    [expected_sampling_ratio] (by default 0.10) ensures that the
    generator generates in general different sample.

    [minimum_number_of_samples] (by default 50) ensures that the
    property is run at least that numbero f times.

    Those default values should ensure that it is not a constraint
    most of the times, while at the same time should capture
    erronenous cases.

    [stop_after] allows to determine how many samples should be drawn
    to run the property on. By default it is set to [`Timeout 0.10],
    meaning that the test will stop after 100ms. It can be set to
    [`Count n] to decide a precise number of times the test should be
    run. Do note that the default value depends on the CLI arguments
    provided by the user. If [--loop] was set, then the default value
    is [`Loop]. While if [`--loop-count n] was set with [n > 1], the
    default value will be [`Count n].

    [on_sample] is called any time a new value is generated.


    If [pp] is provided, it can be used to print the counter-example
    found.

    If [hash] is provided, it will be used to [hash] values. This
    function is used to count the number of distinct samples.

    [regression] can be instantiated to be sure that some fixed
    deterministic examples is always run.

    This function can be influenced with two options from the command-line:

    - [--shrink] triggers the shrinker to find a smaller
    counter-example. An explanation about shrinking can be found in
    the documentation of the package [Bam].

    - [--no-capture] so that output is not captured
 *)
end
