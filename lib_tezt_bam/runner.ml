open Tezt
open Bam
module Set = Set.Make (Int)

let default_minimum_sampling_ratio = 0.10

let default_minimum_number_of_samples = 50

let default_stop_after =
  match Cli.Options.loop_mode with
  | Infinite ->
      `Loop
  | Count 1 ->
      `Timeout 0.100
  | Count n ->
      `Count n

let log ?(level = Cli.Logs.Info) ?(color = Log.Color.FG.magenta) text =
  Log.log ~level ~prefix:"pbt" ~color text

module Stats = struct
  type t =
    { start: Mtime_clock.counter
    ; min: Mtime.span
    ; max: Mtime.span
    ; avg: Mtime.span
    ; count: int
    ; total: Mtime.span
    ; distinct_values: Set.t }

  let empty () =
    { start= Mtime_clock.counter ()
    ; min= Mtime.Span.max_span
    ; max= Mtime.Span.min_span
    ; avg= Mtime.Span.zero
    ; count= 0
    ; total= Mtime.Span.zero
    ; distinct_values= Set.empty }

  let update =
    let counter = ref (Mtime_clock.counter ()) in
    fun stats hash ->
      let distinct_values = Set.add hash stats.distinct_values in
      let single_run = Mtime_clock.count !counter in
      let min =
        if Mtime.Span.compare single_run stats.min < 0 then single_run
        else stats.min
      in
      let max =
        if Mtime.Span.compare single_run stats.max > 0 then single_run
        else stats.max
      in
      let avg =
        let single_run = Mtime.Span.to_float_ns single_run in
        let avg = Mtime.Span.to_float_ns stats.avg in
        let left = single_run /. Float.of_int (stats.count + 1) in
        let right =
          avg *. Float.of_int stats.count /. Float.of_int (stats.count + 1)
        in
        left +. right |> Mtime.Span.of_float_ns
        |> Option.value ~default:Mtime.Span.year
      in
      let count = stats.count + 1 in
      let total = Mtime_clock.count stats.start in
      let start = stats.start in
      counter := Mtime_clock.counter () ;
      {start; min; max; avg; count; total; distinct_values}

  let uncount stats x =
    { stats with
      count= stats.count - 1
    ; distinct_values= Set.remove x stats.distinct_values }

  let sampling_ratio {count; distinct_values; _} =
    if count = 0 then 0.
    else Float.of_int (Set.cardinal distinct_values) /. Float.of_int count

  let should_stop stop_after stats =
    match stop_after with
    | `Loop ->
        false
    | `Count n ->
        stats.count > n
    | `Timeout t ->
        t *. 1_000_000_000. < Mtime.Span.to_float_ns stats.total

  let samples stats = Set.cardinal stats.distinct_values

  let pp fmt ({start= _; min; max; avg; count; total; distinct_values} as stats)
      =
    Format.fprintf fmt
      "Execution time: %a@.Minimum execution time: %a@.Maximum execution time: \
       %a@.Average execution time: %a@.Number of executions: %d@.Number of \
       distinct samples (not producing 'Bad_value' error): %d@.sampling ratio: \
       %f"
      Mtime.Span.pp total Mtime.Span.pp min Mtime.Span.pp max Mtime.Span.pp avg
      count
      (Set.cardinal distinct_values)
      (sampling_ratio stats)
end

let run ~aggressive ~expected_sampling_ratio ~minimum_number_of_samples ~hash
    ~pp ~regression ~capture ~shrink ~stop_after ~on_sample gen f =
  let update stats value =
    on_sample value ;
    Stats.update stats (hash value)
  in
  (* Tezt uses [Random] for initializing the seed. For compatibility
     with OCaml 4.14, the library uses a different module.

     Since Tezt allows replayability with a deterministic seed and
     because Tezt does not give us access to this seed, we draw a
     number as a seed. This process is deterministic if the seed is
     fixed. *)
  let seed = Random.full_int Int.max_int in
  let get_state =
    let state = ref (Gen.Random.make [|seed|]) in
    fun () ->
      let left, right = Gen.Random.split !state in
      state := left ;
      right
  in
  let pp_stop_after fmt = function
    | `Loop ->
        Format.fprintf fmt "Loop until finding a counter-example"
    | `Count n ->
        Format.fprintf fmt "Stop after running %d samples" n
    | `Timeout f ->
        Format.fprintf fmt "Stop after %f seconds elapsed" f
  in
  log "Start searching for a counter example" ;
  log "%a" pp_stop_after stop_after ;
  (* This file will contain the stdout output captured. *)
  let captured_stdout = Temp.file "stdout" in
  (* This file will contain the stderr output captured. *)
  let captured_stderr = Temp.file "stderr" in
  (* This function is [f] where output is captured if
     [captured_output] is set. *)
  let with_capture f =
    if not capture then f ()
    else
      let dump_stdout =
        captured_stdout |> open_out |> Unix.descr_of_out_channel
      in
      let dump_stderr =
        captured_stderr |> open_out |> Unix.descr_of_out_channel
      in
      let stdout = Unix.dup ~cloexec:true Unix.stdout in
      let stderr = Unix.dup ~cloexec:true Unix.stderr in
      Unix.dup2 ~cloexec:true dump_stdout Unix.stdout ;
      Unix.dup2 ~cloexec:true dump_stderr Unix.stderr ;
      let finally () =
        Unix.dup2 ~cloexec:true stdout Unix.stdout ;
        Unix.dup2 ~cloexec:true stderr Unix.stderr ;
        Unix.close stdout ;
        Unix.close stderr ;
        Unix.close dump_stdout ;
        Unix.close dump_stderr
      in
      (* If [f] raises an exception we restore stdout/stderr so that
         Tezt can inform correctly what had happened to the user. *)
      Fun.protect ~finally f
  in
  (* Execute regression outputs provided by the users. *)
  let rec regressions stats values =
    match values with
    | [] ->
        Ok stats
    | x :: l -> (
        let result = f x in
        let stats = update stats x in
        match result with
        | Ok _ ->
            regressions stats l
        | Error `Bad_value ->
            Error
              ( Tree.return x
              , "Regression test triggered a [`Bad_value] failure"
              , stats )
        | Error (`Fail message) ->
            Error (Tree.return x, message, stats) )
  in
  (* Repeat the function [f] [count] times with random inputs. *)
  let rec loop stats =
    if Stats.should_stop stop_after stats then Ok stats
    else
      let tree = Gen.run gen (get_state ()) in
      let result = f (Tree.root tree) in
      let stats = update stats (Tree.root tree) in
      match result with
      | Ok _x ->
          loop stats
      | Error `Bad_value ->
          (* We remove this sampling. *)
          loop (Stats.uncount stats (hash (Tree.root tree)))
      | Error (`Fail message) ->
          Error (tree, message, stats)
  in
  let stats = Stats.empty () in
  let result =
    with_capture (fun () ->
        match regressions stats regression with
        | Ok stats ->
            loop stats
        | Error err ->
            Error err )
  in
  match result with
  | Ok stats ->
      log "No counter-example found" ;
      log ~level:Debug "Runtime statistics:@.%a" Stats.pp stats ;
      (* We consider the test fails if it did not run with enough
         distinct samples. This is because this is probably an error
         that should be notified and is hard to catch otherwise. *)
      let sampling_ratio = Stats.sampling_ratio stats in
      if sampling_ratio < expected_sampling_ratio then
        let msg =
          Format.asprintf
            "No counter example was found. However, the property run with a \
             sampling ratio of: %f. For this test, it was expected at least: \
             %f. If this is expected, consider decreasing the expected \
             sampling ratio (default: %f). Otherwise, it may be possible there \
             is an issue with the generator used by the test."
            sampling_ratio expected_sampling_ratio
            default_minimum_sampling_ratio
        in
        `Not_enough_samples msg
      else if Stats.samples stats < minimum_number_of_samples then
        let msg =
          Format.asprintf
            "No counter example was found. However, the property was run with \
             %d distinct samples while for this test, it was expected to run \
             with at least %d. If this is expected, consier decreasing the \
             expected number of samples (default: %d). Otherwise, it may be \
             possible there is an issue the property or the generator."
            (Stats.samples stats) minimum_number_of_samples
            default_minimum_number_of_samples
        in
        `Not_enough_samples msg
      else `Ok
  | Error (tree, message, stats) -> (
      log "First counter example found: %a@.With error:@.%s@." pp
        (Tree.root tree) message ;
      log ~level:Debug "Runtime statistics:@.%a@." Stats.pp stats ;
      let counter_example =
        if shrink then (
          log "Start shrinking..." ;
          let value =
            with_capture (fun () ->
                tree |> Tree.crunch aggressive |> Tree.shrink f )
          in
          log "Smaller counter example found:@.%a" pp value ;
          value )
        else Tree.root tree
      in
      (* We reexecute the function [f] on the counter-example. This
         enables users to observe any output on the counter-example
         returned. *)
      log ~level:Debug
        "Running property on the smaller counter example found (without \
         capturing stdout/stderr)..." ;
      let result = f counter_example in
      let stats = update stats counter_example in
      log "Total execution time: %a@." Mtime.Span.pp stats.total ;
      log ~level:Report "\\  | /" ;
      log ~level:Report "- BAM -" ;
      log ~level:Report " / | \\ " ;
      if not shrink then
        log ~level:Report ~color:Log.Color.FG.yellow
          "Please run the test again with option --shrink to get a smaller \
           counter-example" ;
      log "Counter example found:@.%a" pp counter_example ;
      match result with
      | Ok _ | Error `Bad_value ->
          `Not_deterministic counter_example
      | Error (`Fail err) ->
          `Failed err )

module Cli = struct
  let section =
    Clap.section
      ~description:"Options that can be used for PBT tests using Bam." "Bam"

  let shrink =
    Clap.flag ~section ~set_long:"shrink"
      ~description:"Use for PBT test to find a smaller counter-example." false

  let capture =
    Clap.flag ~section ~unset_long:"no-capture"
      ~description:
        "While running examples, do not capture any output from stdout or \
         stderr."
      true

  let aggressive =
    Clap.default_int ~section ~long:"aggressive"
      ~description:
        "Make the shrinking heuristic more aggressive (should be >= 1)." 0
end

include Cli

let register ?(hash = Hashtbl.hash)
    ?(pp =
      fun fmt _s ->
        Format.fprintf fmt "<Unable to print the value: no printer given>")
    ?(expected_sampling_ratio = default_minimum_sampling_ratio)
    ?(minimum_number_of_samples = default_minimum_number_of_samples)
    ?(regression = []) ?(stop_after = default_stop_after)
    ?(on_sample = fun _ -> ()) ~__FILE__ ~title ~tags ~gen ~property () =
  Test.register ~seed:Random ~__FILE__ ~title ~tags
  @@ fun () ->
  match
    run ~aggressive ~expected_sampling_ratio ~minimum_number_of_samples ~hash
      ~pp ~regression ~capture ~shrink ~stop_after ~on_sample gen property
  with
  | `Ok ->
      Lwt.return_unit
  | `Not_enough_samples msg ->
      Test.fail "%s" msg
  | `Not_deterministic counter_example ->
      Test.fail
        "A counter example '%a' was found by the shrinking, but when run \
         again, the test succeeded. Is the test not deterministic?"
        pp counter_example
  | `Failed message ->
      Test.fail "Test failed with error:@.%s" message
