open Tezt_bam
open Std

let capture ?eol fmt = Format.kasprintf (Regression.capture ?eol) fmt

module Predicate = struct
  module Range = struct
    let always_fail x = Error x

    let fail_if_not_null_and_multiple_of_3 x =
      if x mod 3 = 0 && x > 0 then Error x else Ok x

    let fail_if_9 x = if x = 9 then Error x else Ok x

    let fail_if_greater_than_10_and_not_power_of_2 x =
      let power_of_2 = [0; 1; 2; 4; 8; 16; 32; 63; 128; 256; 512; 1024] in
      if x > 10 && List.mem x power_of_2 = false then Error x else Ok x

    let all =
      [ ("always fail", always_fail)
      ; ( "fail_if_not_null_and_multiple_of_3"
        , fail_if_not_null_and_multiple_of_3 )
      ; ("fail if 9", fail_if_9)
      ; ( "fail if greater than 10 and not power of 2"
        , fail_if_greater_than_10_and_not_power_of_2 ) ]
  end

  module Pair = struct
    let not_null (x, y) = if x < 10 && y < 10 then Ok x else Error x

    let modulo (x, y) = if (x + y) mod 11 = 0 then Ok x else Error x

    let difference_must_not_be_zero (x, y) =
      if x < 10 || abs (x - y) <> 0 then Ok x else Error x

    let difference_must_not_be_small (x, y) =
      if x < 10 || 1 > abs (x - y) || abs (x - y) > 4 then Ok x else Error x

    let difference_must_not_be_one (x, y) =
      if x < 10 || abs (x - y) <> 1 then Ok x else Error x

    let _all =
      [ not_null
      ; modulo
      ; difference_must_not_be_zero
      ; difference_must_not_be_small
      ; difference_must_not_be_one ]
  end

  module List = struct
    let never_fails l = Ok l

    let all_different_three l =
      if l |> List.sort_uniq compare |> List.length < 3 then Ok l else Error l

    (* Threshold should be 10% of the maximum value of the list *)
    let length_list ~threshold l =
      if List.fold_left max 0 l > threshold then Error l else Ok l

    let sum l =
      let x = List.fold_left ( + ) 0 l in
      (* We had a side-effect to test the runner. *)
      Format.eprintf "STDOUT@." ;
      Format.eprintf "STDERR@." ;
      if x < 10 || x mod 13 <> 0 then Ok l else Error l

    let _all = [never_fails; all_different_three; sum]
  end

  module List_list = struct
    module Set = Set.Make (Int)

    let large_union_list l =
      let set =
        Stdlib.List.fold_left
          (fun set l -> Set.of_list l |> Set.union set)
          Set.empty l
      in
      if Set.cardinal set < 5 then Ok l else Error l
  end
end

let binary_search () =
  (* This test prints a regression trace of the current tree associate
     to a range of integers. This order could change in the future,
     but if this happens, this would appear in the regression
     trace. *)
  Regression.register ~__FILE__ ~title:"[pbt] Tree.binary-search"
    ~tags:["pbt"; "tree"; "binary"; "search"]
  @@ fun () ->
  capture "Check tree returned by [Tree.binary_search]" ;
  let predicates = Predicate.Range.all in
  let to_string x = Format.asprintf " %d" x in
  let check_function ?expect ~name tree f =
    capture "Check [%s]" name ;
    let seq = f tree in
    Option.iter (fun expect -> expect seq) expect ;
    Seq.iter (fun x -> capture ~eol:false "%s" (to_string x)) seq ;
    capture "" ;
    capture ""
  in
  let shrink ?(strategy = "default") ~name ~predicate tree =
    (* Even though the initial root value may not be a counter
       example, we check the shrinker beahvior on it. *)
    capture "Shrink the tree with root %d for predicate [%s]." (Tree.root tree)
      name ;
    capture "Shrinking strategy used: %s" strategy ;
    let counter_example = Tree.shrink predicate tree in
    if Tree.root tree = counter_example then capture "No smaller example found"
    else capture "Minimum example found: %d" counter_example ;
    capture ""
  in
  let run ~predicates ~min ~initial =
    capture "Tree generated with [Tree.binary_search ~min=%d ~initial=%d]" min
      initial ;
    capture "" ;
    let tree =
      Tree.binary_search ~initial:(Z.of_int initial) ~origin:(Z.of_int min) ()
      |> Tree.map Z.to_int
    in
    let expect seq =
      let sum = Seq.fold_left ( + ) 0 seq in
      let expected_sum =
        if initial >= min then
          (((initial * (initial + 1)) - (min * (min + 1))) / 2) + min
        else min
      in
      let error_msg =
        Format.asprintf
          "[Tree.binary_search] Some values are missing from the tree computed \
           for range [%d;%d]. Expected the sum of the values to be '%%R'. Got \
           '%%L'"
          min initial
      in
      Check.((sum = expected_sum) int ~error_msg)
    in
    check_function ~expect ~name:"Tree.dfs" tree Tree.dfs ;
    check_function ~name:"Tree.row 1" tree (Tree.row 1) ;
    check_function ~name:"Tree.row 2" tree (Tree.row 2) ;
    check_function ~name:"Tree.row 3" tree (Tree.row 3) ;
    check_function ~name:"Tree.crunch 0 | Tree.row 0" tree (fun tree ->
        Tree.crunch 0 tree |> Tree.row 0 ) ;
    check_function ~name:"Tree.crunch 0 | Tree.row 1" tree (fun tree ->
        Tree.crunch 0 tree |> Tree.row 1 ) ;
    check_function ~name:"Tree.crunch 1 | Tree.row 0" tree (fun tree ->
        Tree.crunch 1 tree |> Tree.row 0 ) ;
    check_function ~name:"Tree.crunch 1 | Tree.row 1" tree (fun tree ->
        Tree.crunch 1 tree |> Tree.row 1 ) ;
    check_function ~name:"Tree.crunch 2 | Tree.dfs" tree (fun tree ->
        Tree.crunch 2 tree |> Tree.dfs ) ;
    check_function ~name:"Tree.crunch 2 | Tree.row 1" tree (fun tree ->
        Tree.crunch 2 tree |> Tree.row 1 ) ;
    Fun.flip List.iter predicates (fun (name, predicate) ->
        shrink ~name ~predicate tree ;
        shrink ~strategy:"crunch 2" ~name ~predicate (Tree.crunch 2 tree) ;
        shrink ~strategy:"crunch 3" ~name ~predicate (Tree.crunch 3 tree) )
  in
  let mins = [0; 1; 2; 7; 10; 25] in
  let initials = [0; 1; 2; 5; 8; 10; 21; 51] in
  Fun.flip List.iter mins (fun min ->
      Fun.flip List.iter initials (fun initial ->
          if initial >= min then run ~predicates ~min ~initial ) ) ;
  Lwt.return_unit

let constant : ?origin:int -> int -> int t =
 fun ?origin n ->
  let origin = Option.value ~default:n origin |> Z.of_int in
  let min = Z.of_int n in
  let max = Z.of_int (n + 1) in
  Gen.z_range ~origin ~min ~max () |> Gen.map Z.to_int

let range () =
  Regression.register ~__FILE__ ~title:"[pbt] - Std.int"
    ~tags:["pbt"; "gen"; "range"]
  @@ fun () ->
  let state = Gen.Random.make [|0|] in
  let gen_int_and_pp ?origin ~min ~max () =
    ( match origin with
    | None ->
        capture "[Std.int ~min=%d ~max=%d]" min max
    | Some origin ->
        capture "[Std.int ~origin:(Std.Shrinker.Int %d) ~min=%d ~max=%d]" origin
          min max ) ;
    let shrinker = Option.map (fun x -> Std.Shrinker.Int x) origin in
    let gen = Std.int ?shrinker ~min ~max () in
    let tree = Gen.run gen state in
    let root = Tree.root tree in
    capture "root: %d" root ;
    let children = Tree.row 1 tree in
    capture ~eol:false "children:" ;
    Seq.iter (fun x -> capture ~eol:false "%d " x) children ;
    capture ""
  in
  let () = gen_int_and_pp ~min:0 ~max:0 () in
  let _ = gen_int_and_pp ~min:Int.min_int ~max:0 () in
  let _ = gen_int_and_pp ~min:0 ~max:Int.min_int () in
  let _ = gen_int_and_pp ~min:(-1000) ~max:1000 () in
  let _ = gen_int_and_pp ~min:Int.min_int ~max:Int.max_int () in
  let _ =
    gen_int_and_pp ~origin:(-10000) ~min:(Int.min_int + 1) ~max:Int.max_int ()
  in
  let _ = gen_int_and_pp ~origin:1 ~min:(Int.min_int + 1) ~max:Int.max_int () in
  let pp_row ~prefix seq =
    Regression.capture prefix ;
    Regression.capture
      ( String.concat " "
      @@ (Seq.map (fun x -> string_of_int x) seq |> List.of_seq) ) ;
    Regression.capture ""
  in
  let () =
    constant ~origin:0 (Int.max_int - 1)
    |> Fun.flip Gen.run state |> Tree.row 1
    |> pp_row ~prefix:"[Gen.constant ~origin:0 (Int.max_int - 1)] | First row"
  in
  let () =
    constant ~origin:0 100 |> Fun.flip Gen.run state |> Tree.row 1
    |> pp_row ~prefix:"[Gen.constant ~origin:0 100] | Row 1"
  in
  let () =
    constant ~origin:0 100 |> Fun.flip Gen.run state |> Tree.row 2
    |> pp_row ~prefix:"[Gen.constant ~origin:0 100] | Row 2"
  in
  let () =
    constant ~origin:20 100 |> Fun.flip Gen.run state |> Tree.row 1
    |> pp_row ~prefix:"[Gen.constant ~origin:20 100] | Row 1"
  in
  let () =
    constant ~origin:20 100 |> Fun.flip Gen.run state |> Tree.row 2
    |> pp_row ~prefix:"[Gen.constant ~origin:20 100] | Row 2"
  in
  let () =
    constant ~origin:(-100) 100
    |> Fun.flip Gen.run state |> Tree.row 1
    |> pp_row ~prefix:"[Gen.constant ~origin:-100 100] | Row 1"
  in
  let () =
    constant ~origin:(-100) 100
    |> Fun.flip Gen.run state |> Tree.row 2
    |> pp_row ~prefix:"[Gen.constant ~origin:-100 100] | Row 2"
  in
  Lwt.return_unit

let pbt_pair () =
  Regression.register ~__FILE__ ~title:"[pbt] - Std.pair"
    ~tags:["pbt"; "std"; "pair"]
  @@ fun () ->
  let predicates =
    Predicate.Pair.
      [ ("not null", not_null)
      ; ("modulo", modulo)
      ; ("difference must not be zero", difference_must_not_be_zero)
      ; ("difference must not be small", difference_must_not_be_small)
      ; ("difference must not be one", difference_must_not_be_one) ]
  in
  let gen ~shrinker =
    pair ~shrinker (int ~min:0 ~max:50 ()) (int ~min:0 ~max:50 ())
  in
  let gen0 = gen ~shrinker:Pair_left in
  let gen1 = gen ~shrinker:Pair_right in
  let compare (x, y) (x', y') = Stdlib.compare (x + y) (x' + y') in
  let gen2 = gen ~shrinker:(Pair_compare compare) in
  let seeds = [[|20; 10|]; [|50; 20|]] in
  seeds
  |> List.iter (fun seed ->
         predicates
         |> List.iter (fun (name, predicate) ->
                capture "Predicate: %s" name ;
                let state = Gen.Random.make seed in
                let to_string (x, y) = Format.asprintf "(%d,%d)" x y in
                let value0 = Gen.run gen0 state in
                let state = Gen.Random.make seed in
                let value1 = Gen.run gen1 state in
                let state = Gen.Random.make seed in
                let value2 = Gen.run gen2 state in
                let root0 = Tree.root value0 in
                let root2 = Tree.root value2 in
                assert (root0 = root2) ;
                capture "%s" (to_string root0) ;
                Tree.dfs value0
                |> Seq.map (fun v -> to_string v)
                |> List.of_seq |> String.concat " " |> capture "%s" ;
                Tree.row 1 value0
                |> Seq.map (fun v -> to_string v)
                |> List.of_seq |> String.concat " " |> capture "%s" ;
                capture "%s" (to_string @@ Tree.shrink predicate value0) ;
                capture "%s"
                  (to_string @@ Tree.shrink predicate (Tree.crunch 2 value0)) ;
                capture "@." ;
                Tree.dfs value1
                |> Seq.map (fun v -> to_string v)
                |> List.of_seq |> String.concat " " |> capture "%s" ;
                Tree.row 1 value1
                |> Seq.map (fun v -> to_string v)
                |> List.of_seq |> String.concat " " |> capture "%s" ;
                capture "%s" (to_string @@ Tree.shrink predicate value1) ;
                capture "%s"
                  (to_string @@ Tree.shrink predicate (Tree.crunch 2 value1)) ;
                capture "@." ;
                Tree.dfs value2
                |> Seq.map (fun v -> to_string v)
                |> List.of_seq |> String.concat " " |> capture "%s" ;
                Tree.row 1 value2
                |> Seq.map (fun v -> to_string v)
                |> List.of_seq |> String.concat " " |> capture "%s" ;
                capture "%s" (to_string @@ Tree.shrink predicate value2) ;
                capture "%s"
                  (to_string @@ Tree.shrink predicate (Tree.crunch 2 value2)) ;
                capture "@." ) ) ;
  Lwt.return ()

let pair_shrinking () =
  Regression.register ~__FILE__ ~title:"[pbt] - Check strategies of Std.pair"
    ~tags:["pbt"; "std"; "pair"; "shrinking"; "tactic"]
  @@ fun () ->
  let predicates =
    Predicate.Pair.
      [ ("not null", not_null)
      ; ("modulo", modulo)
      ; ("difference must not be zero", difference_must_not_be_zero)
      ; ("difference must not be small", difference_must_not_be_small)
      ; ("difference must not be one", difference_must_not_be_one) ]
  in
  (* A bit meta: We want to generate a deterministic seed to generates
     other seeds. *)
  let gen ~shrinker =
    pair ~shrinker (int ~min:0 ~max:100 ()) (int ~min:0 ~max:100 ())
  in
  let compare (x, y) (x', y') = compare (x + y) (x' + y') in
  let get_tree ~shrinker state = Gen.run (gen ~shrinker) state in
  let a = ("left", fun state -> get_tree ~shrinker:Pair_left state) in
  let b = ("right", fun state -> get_tree ~shrinker:Pair_right state) in
  let c =
    ("compare sum", fun state -> get_tree ~shrinker:(Pair_compare compare) state)
  in
  let d =
    ( "left | crunch"
    , fun state -> get_tree ~shrinker:Pair_left state |> Tree.crunch 2 )
  in
  let e =
    ( "right | crunch"
    , fun state -> get_tree ~shrinker:Pair_right state |> Tree.crunch 2 )
  in
  let f =
    ( "compare sum | crunch"
    , fun state ->
        get_tree ~shrinker:(Pair_compare compare) state |> Tree.crunch 2 )
  in
  let table = Hashtbl.create 6 in
  let strategies = [a; b; c; d; e; f] in
  let run predicate seed =
    let has_failed = ref false in
    let values =
      strategies
      |> List.map (fun (name, f) ->
             let state = Gen.Random.make seed in
             ( match predicate (Tree.root (f state)) with
             | Error _ ->
                 has_failed := true
             | Ok _ ->
                 () ) ;
             (name, f state |> Tree.shrink predicate) )
    in
    let winner =
      values |> List.sort (fun (_, v) (_, v') -> compare v v') |> List.hd |> snd
    in
    List.iter
      (fun (name, value) ->
        if !has_failed && value = winner then
          match Hashtbl.find_opt table name with
          | None ->
              Hashtbl.add table name 1
          | Some v ->
              Hashtbl.replace table name (v + 1) )
      values
  in
  let seeds = Seq.ints 0 |> Seq.take 1000 |> Seq.map (fun x -> [|x|]) in
  predicates
  |> List.iter (fun (name, predicate) ->
         Hashtbl.reset table ;
         seeds |> Seq.iter (fun seed -> run predicate seed) ;
         capture "Predicate: %s" name ;
         List.iter
           (fun (name, _tactic) ->
             match Hashtbl.find_opt table name with
             | None ->
                 capture "%s: 0" name
             | Some v ->
                 capture "%s: %d" name v )
           strategies ) ;
  Lwt.return ()

let pbt_list () =
  Regression.register ~__FILE__ ~title:"[pbt] - Std.list"
    ~tags:["pbt"; "std"; "list"]
  @@ fun () ->
  let predicates =
    Predicate.List.
      [ ("all different three", all_different_three)
      ; ("length list", length_list ~threshold:90) ]
  in
  let gen = list ~size:(int ~min:0 ~max:10 ()) (int ~min:0 ~max:10 ()) in
  let seeds = [[|20; 10|]; [|50; 20|]] in
  seeds
  |> List.iter (fun seed ->
         predicates
         |> List.iter (fun (name, predicate) ->
                capture "Predicate: %s" name ;
                let state = Gen.Random.make seed in
                let to_string l =
                  Format.asprintf "[%s]"
                  @@ String.concat " " (List.map string_of_int l)
                in
                let to_string_depth =
                  let current_depth = ref 0 in
                  fun (d, l) ->
                    if !current_depth = d then to_string l
                    else if !current_depth < d then (
                      incr current_depth ;
                      string_of_int d ^ "{ " ^ to_string l ^ "" )
                    else
                      let depths =
                        List.init (!current_depth - d) (fun x ->
                            !current_depth - x )
                      in
                      current_depth := d ;
                      List.map (fun x -> string_of_int x ^ "} ") depths
                      |> String.concat ""
                      |> fun s -> s ^ to_string l
                in
                let value = Gen.run gen state in
                let root = Tree.root value in
                capture "Root: %s" (to_string root) ;
                Tree.dfs value
                |> Seq.map (fun v -> to_string v)
                |> List.of_seq |> String.concat " " |> capture "DFS: %s" ;
                Tree.dfs_with_depth value
                |> Seq.map (fun (depth, v) -> to_string_depth (depth, v))
                |> List.of_seq |> String.concat " "
                |> capture "DFS (with depth): %s" ;
                Tree.row 1 value
                |> Seq.map (fun v -> to_string v)
                |> List.of_seq |> String.concat " " |> capture "First row: %s" ;
                capture "Counter example: %s"
                  (to_string @@ Tree.shrink predicate value) ;
                capture "Counter example (crunch): %s"
                  (to_string @@ Tree.shrink predicate (Tree.crunch 2 value)) ;
                capture "@." ) ) ;
  Lwt.return ()

let list_skip_shrinking () =
  Regression.register ~__FILE__ ~title:"[pbt] - Std.list skip strategy"
    ~tags:["pbt"; "std"; "list"; "skip"]
  @@ fun () ->
  let gen ~skip =
    list
      ~shrinker:(Skip {eq= ( = ); skip= `Int skip})
      ~size:(constant ~origin:0 3) (int ~min:0 ~max:10 ())
  in
  let state = Gen.Random.make [|0|] in
  Fun.flip List.iter [0; 1; 2] (fun skip ->
      let tree = Gen.run (gen ~skip) state in
      let to_string l =
        Format.asprintf "[%s]" @@ String.concat " " (List.map string_of_int l)
      in
      let to_string_depth =
        let current_depth = ref 0 in
        fun (d, l) ->
          if !current_depth = d then to_string l
          else if !current_depth < d then (
            incr current_depth ;
            string_of_int d ^ "{ " ^ to_string l ^ "" )
          else
            let depths =
              List.init (!current_depth - d) (fun x -> !current_depth - x)
            in
            current_depth := d ;
            List.map (fun x -> string_of_int x ^ "} ") depths
            |> String.concat ""
            |> fun s -> s ^ to_string l
      in
      Tree.row 1 tree
      |> Seq.map (fun l -> to_string l)
      |> List.of_seq |> String.concat " " |> capture "First row: %s" ;
      Tree.dfs_with_depth tree
      |> Seq.map (fun (depth, v) -> to_string_depth (depth, v))
      |> List.of_seq |> String.concat " "
      |> capture "DFS (with depth): %s" ) ;
  Lwt.return ()

let list_strategies () =
  Regression.register ~__FILE__ ~title:"[pbt] - Std.list strategies"
    ~tags:["pbt"; "std"; "list"; "tactic"]
  @@ fun () ->
  let small_gen ~shrinker =
    list ~shrinker ~size:(int ~min:0 ~max:10 ()) (int ~min:0 ~max:8 ())
  in
  let big_gen ~shrinker =
    list ~shrinker ~size:(int ~min:10 ~max:20 ()) (int ~min:0 ~max:8 ())
  in
  let to_string l =
    Format.asprintf "[%s]" @@ String.concat " " (List.map string_of_int l)
  in
  let seeds = [[|1|]; [|2|]; [|3|]; [|4|]] in
  seeds
  |> List.iter (fun seed ->
         let strategies =
           [ Shrinker.Prefix
           ; Suffix
           ; Skip {eq= ( = ); skip= `Int 0}
           ; Skip {eq= ( = ); skip= `Int 1}
           ; Skip {eq= ( = ); skip= `Int 2}
           ; Skip {eq= ( = ); skip= `Auto} ]
         in
         let shrinker_of_string = function
           | Shrinker.Prefix ->
               "prefix"
           | Suffix ->
               "suffix"
           | Skip {eq= _; skip= `Int i} ->
               "skip " ^ string_of_int i
           | Skip {eq= _; skip= `Auto} ->
               "skip smart"
           | Default ->
               "default"
           | Manual _ ->
               "manual"
         in
         strategies
         |> List.iter (fun shrinker ->
                let state = Gen.Random.make seed in
                let small_tree = Gen.run (small_gen ~shrinker) state in
                capture
                  "Generate a random list of size between 0 and 20 and \
                   elements of size 0 and 8 (tactic is %s)"
                  (shrinker_of_string shrinker) ;
                capture "Generated list: %s" (to_string (Tree.root small_tree)) ;
                let cpt = ref 0 in
                capture ~eol:false "First row: " ;
                Tree.row 1 small_tree
                |> Seq.iter (fun l ->
                       incr cpt ;
                       capture ~eol:false "%s " (to_string l) ) ;
                capture "" ;
                capture "Number of elements in the first row: %d@." !cpt ;
                let big_tree = Gen.run (big_gen ~shrinker) state in
                capture
                  "Generate a random list of size between 0 and 50 and \
                   elements of size 0 and 8 (shrink is %s)"
                  (shrinker_of_string shrinker) ;
                capture "Generated list: %s" (to_string (Tree.root big_tree)) ;
                let cpt = ref 0 in
                Tree.row 1 big_tree |> Seq.iter (fun _l -> incr cpt) ;
                capture "Number of elements in the first row: %d@." !cpt ) ) ;
  Lwt.return ()

let list_nested () =
  Regression.register ~__FILE__ ~title:"[pbt] - Std.list nested"
    ~tags:["pbt"; "std"; "list"]
  @@ fun () ->
  let predicates =
    Predicate.List_list.[("large union list", large_union_list)]
  in
  let gen =
    list ~size:(int ~min:0 ~max:10 ())
      (list ~size:(int ~min:0 ~max:10 ()) (int ~min:0 ~max:50 ()))
  in
  let seeds = [[|20; 10|]; [|50; 20|]] in
  seeds
  |> List.iter (fun seed ->
         predicates
         |> List.iter (fun (name, predicate) ->
                capture "Predicate: %s" name ;
                let state = Gen.Random.make seed in
                let to_string l =
                  let to_string_inner l =
                    Format.asprintf "[%s]"
                    @@ String.concat " " (List.map string_of_int l)
                  in
                  Format.asprintf "[%s]"
                  @@ String.concat " " (List.map to_string_inner l)
                in
                let value = Gen.run gen state in
                let root = Tree.root value in
                capture "Root: %s" (to_string root) ;
                Tree.row 1 value
                |> Seq.map (fun v -> to_string v)
                |> List.of_seq |> String.concat " " |> capture "First row: %s" ;
                capture "Counter example: %s"
                  (to_string @@ Tree.shrink predicate value) ;
                capture "Counter example (crunch): %s"
                  (to_string @@ Tree.shrink predicate (Tree.crunch 2 value)) ;
                capture "@." ) ) ;
  Lwt.return ()

let register () =
  binary_search () ;
  range () ;
  pbt_pair () ;
  pair_shrinking () ;
  pbt_list () ;
  list_skip_shrinking () ;
  list_strategies () ;
  list_nested ()
