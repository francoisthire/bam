open Tezt_bam
module Tree = Bam.Tree
module Gen = Bam.Gen

let capture_tree ?(filter = Tree.dfs_with_depth) to_string tree =
  filter tree
  |> Seq.iter (fun (depth, x) ->
         let string = Format.asprintf "(%d,%s) " depth (to_string x) in
         Regression.capture ~eol:false string ) ;
  Regression.capture ""

let z_range_regression () =
  Regression.register ~__FILE__ ~title:"Gen.z_range" ~tags:["gen"; "z_range"]
  @@ fun () ->
  let min = [-10; -5; 0; 8; 24; 50] |> List.to_seq |> Seq.map Z.of_int in
  let max = [-12; -8; 0; 8; 32] |> List.to_seq |> Seq.map Z.of_int in
  let origin = [-20; -5; 4; 8; 100] |> List.to_seq |> Seq.map Z.of_int in
  let state = Gen.Random.make [|0|] in
  Seq.product min max
  |> Seq.iter (fun (min, max) ->
         Regression.capture
         @@ Format.asprintf "Gen.z_range ?origin ~min:%s ~max:%s"
              (Z.to_string min) (Z.to_string max) ;
         Gen.z_range ~min ~max () |> Fun.flip Gen.run state
         |> capture_tree Z.to_string ;
         Seq.iter
           (fun origin ->
             Regression.capture
             @@ Format.asprintf "Gen.z_range ~origin:%s ~min:%s ~max:%s"
                  (Z.to_string origin) (Z.to_string min) (Z.to_string max) ;
             Gen.z_range ~origin ~min ~max ()
             |> Fun.flip Gen.run state |> capture_tree Z.to_string )
           origin ) ;
  Lwt.return_unit

let float_range () =
  Regression.register ~__FILE__ ~title:"Gen.float_range"
    ~tags:["gen"; "float_range"]
  @@ fun () ->
  let exhaustive_search_digits = [0; 1; 2] |> List.to_seq in
  let precision_digits = [0; 10; 15] |> List.to_seq in
  let origin = [-2.45; -1.5; 0.; 0.555; 0.1234] |> List.to_seq in
  let min = [-10.; -5.5; 0.; 7.22; 32.111] |> List.to_seq in
  let max = [-20.01; -5.9; 4.0; 8.90; 100.001] |> List.to_seq in
  let state = Gen.Random.make [|0|] in
  Seq.product min max
  |> Seq.product
       (Seq.product
          (Seq.product exhaustive_search_digits precision_digits)
          origin )
  |> Seq.iter
       (fun
         (((exhaustive_search_digits, precision_digits), origin), (min, max)) ->
         Regression.capture
         @@ Format.asprintf
              "Gen.float_range ~exhaustive_search_digits:%d \
               ~precision_digits:%d ~origin:%f ~min:%f ~max:%f"
              exhaustive_search_digits precision_digits origin min max ;
         let exhaustive_search_digits =
           if exhaustive_search_digits = 0 then None
           else Some exhaustive_search_digits
         in
         let precision_digits =
           if precision_digits = 0 then None else Some precision_digits
         in
         let origin = if origin = 0. then None else Some origin in
         let tree =
           Gen.float_range ?exhaustive_search_digits ?precision_digits ?origin
             ~min ~max ()
           |> Fun.flip Gen.run state
         in
         capture_tree
           ~filter:(fun tree ->
             tree |> Tree.dfs_with_depth
             |> Seq.filter (fun (d, _) -> d <= 2)
             |> Seq.take 50 )
           Float.to_string tree ;
         Regression.capture @@ Format.asprintf "crunch 1" ;
         capture_tree
           ~filter:(fun tree ->
             tree |> Tree.row 1
             |> Seq.map (fun x -> (1, x))
             |> Seq.cons (0, Tree.root tree) )
           Float.to_string (Tree.crunch 1 tree) ) ;
  Lwt.return_unit

let float_in_bounds () =
  let gen =
    let open Std.Syntax in
    let* min = Std.float ~max:10. () in
    let* max = Std.float ~min ~max:10. () in
    let* x = Std.float ~min ~max () in
    return (min, max, x)
  in
  let property (min, max, x) =
    if min <= max then
      if min <= x && x <= max then Ok ()
      else
        Error
          (`Fail
            (Format.asprintf "fail with: min:%f@ max:%f, value:%f" min max x) )
    else Ok ()
  in
  Tezt_bam.Pbt.register ~__FILE__ ~title:"Gen.float_in_bounds"
    ~tags:["gen"; "float_range"] ~gen ~property ()

let crunch () =
  let gen =
    let open Std.Syntax in
    let* min = Std.int ~max:10_000 () in
    let* max = Std.int ~min ~max:min () in
    let* crunch = Std.int ~max:10 () in
    return (min, max, crunch)
  in
  let property (min, max, crunch) =
    let gen = Std.int ~min ~max () in
    let state = Gen.Random.make [|0|] in
    let left = Gen.crunch crunch gen |> Fun.flip Gen.run state |> Tree.dfs in
    let right = Gen.run gen state |> Tree.crunch crunch |> Tree.dfs in
    if Seq.equal ( = ) left right then Ok () else Error (`Fail "Not equal")
  in
  Tezt_bam.Pbt.register ~__FILE__ ~title:"Gen.crunch" ~tags:["gen"; "crunch"]
    ~gen ~property ()

let map_bind_return () =
  let gen =
    let open Std.Syntax in
    let* min = Std.int ~max:100_000 () in
    let* max = Std.int ~min ~max:min () in
    return (min, max)
  in
  let property (min, max) =
    let gen = Std.int ~min ~max () in
    let state = Gen.Random.make [|0|] in
    let left =
      Gen.map (fun x -> x + 5) gen |> Fun.flip Gen.run state |> Tree.dfs
    in
    let right =
      Gen.bind gen (fun x -> x + 5 |> Gen.return)
      |> Fun.flip Gen.run state |> Tree.dfs
    in
    if Seq.equal ( = ) left right then Ok () else Error (`Fail "Not equal")
  in
  Tezt_bam.Pbt.register ~__FILE__ ~title:"Gen.map_bind_return"
    ~tags:["gen"; "map"; "bind"; "return"]
    ~gen ~property ()

let root () =
  let gen =
    let open Std.Syntax in
    let* min = Std.int ~max:100_000 () in
    let* max = Std.int ~min ~max:min () in
    return (min, max)
  in
  let property (min, max) =
    let gen = Std.int ~min ~max () in
    let state = Gen.Random.make [|0|] in
    let left =
      Gen.root gen (fun x -> Gen.return x) |> Fun.flip Gen.run state |> Tree.dfs
    in
    let right =
      Fun.flip Gen.run state gen |> Tree.root |> Seq.repeat
      |> Seq.take (Seq.length left)
    in
    if Seq.equal ( = ) left right then Ok () else Error (`Fail "Not equal")
  in
  Tezt_bam.Pbt.register ~__FILE__ ~title:"Gen.root" ~tags:["gen"; "root"] ~gen
    ~property ()

let hard_coded_values () =
  let gen =
    let int_gen =
      let gen = [0; 1; 2; 3; 4] |> List.to_seq |> Seq.cycle |> Gen.of_seq in
      Gen.bind gen (fun root -> Std.int ?root ~min:0 ~max:10 ())
    in
    Std.list ~size:(Std.return 5) int_gen
  in
  Tezt_bam.Pbt.register ~__FILE__ ~title:"self"
    ~compute_execution_statistics:false ~stop_after:(`Count 1) ~tags:["self"]
    ~gen
    ~property:(fun l ->
      if l = [0; 1; 2; 3; 4] then Ok ()
      else Error (`Fail "The generated list should be [0;1;2;3;4]") )
    ()

let register () =
  z_range_regression () ;
  float_range () ;
  float_in_bounds () ;
  crunch () ;
  map_bind_return () ;
  root () ;
  hard_coded_values ()
