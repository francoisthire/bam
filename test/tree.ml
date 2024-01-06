open Tezt_bam

let capture_tree ?(filter = Tree.dfs_with_depth) to_string tree =
  filter tree
  |> Seq.iter (fun (depth, x) ->
         let string = Format.asprintf "(%d,%s) " depth (to_string x) in
         Regression.capture ~eol:false string ) ;
  Regression.capture ""

let binary_search () =
  Regression.register ~__FILE__ ~title:"Tree.binary_search"
    ~tags:["tree"; "binary_search"]
  @@ fun () ->
  let values =
    [-32; -31; -25; -12; -1; 0; 1; 13; 15; 32]
    |> List.to_seq |> Seq.map Z.of_int
  in
  Seq.product values values
  |> Seq.iter (fun (origin, initial) ->
         let tree = Tree.binary_search ~initial ~origin () in
         Regression.capture
         @@ Format.asprintf "Tree.binary_search ~initial:%s ~origin:%s:"
              (Z.to_string initial) (Z.to_string origin) ;
         capture_tree Z.to_string tree ) ;
  Lwt.return_unit

let fractional_binary_search () =
  Regression.register ~__FILE__ ~title:"Tree.fractional_binary_search"
    ~tags:["tree"; "fractional_binary_search"]
  @@ fun () ->
  let values =
    [-1.; -0.7; -0.5; -0.123456; 0.; 0.1; 0.5; 0.987654321; 1.] |> List.to_seq
  in
  Seq.product values values
  |> Seq.iter (fun (origin, initial) ->
         let tree1 =
           Tree.fractional_search ~precision_digits:2 ~initial ~origin ()
         in
         let tree2 =
           Tree.fractional_search ~exhaustive_search_digits:2
             ~precision_digits:3 ~initial ~origin ()
         in
         Regression.capture
         @@ Format.asprintf
              "Tree.fractional_binary_search ~precision_digits:2 ~initial:%f \
               ~origin:%f:"
              initial origin ;
         Regression.capture "First row:" ;
         capture_tree
           ~filter:(fun tree -> tree |> Tree.row 1 |> Seq.map (fun x -> (1, x)))
           Float.to_string tree1 ;
         Regression.capture "Second row (first 100 elements):" ;
         capture_tree
           ~filter:(fun tree ->
             tree |> Tree.row 2 |> Seq.take 100 |> Seq.map (fun x -> (2, x)) )
           Float.to_string tree1 ;
         Regression.capture
         @@ Format.asprintf
              "Tree.fractional_binary_search ~exhaustive_search_digits:2 \
               ~precision_digits:3 ~initial:%f ~origin:%f:"
              initial origin ;
         Regression.capture "First row:" ;
         capture_tree
           ~filter:(fun tree -> tree |> Tree.row 1 |> Seq.map (fun x -> (1, x)))
           Float.to_string tree2 ;
         Regression.capture "Second row (first 100 elements):" ;
         capture_tree
           ~filter:(fun tree ->
             tree |> Tree.row 2 |> Seq.take 100 |> Seq.map (fun x -> (2, x)) )
           Float.to_string tree2 ) ;
  Lwt.return_unit

let crunch () =
  Regression.register ~__FILE__ ~title:"Tree.crunch"
    ~tags:["tree"; "binary_search"; "crunch"]
  @@ fun () ->
  let values =
    [0; 1; 3; 15; 16; 42; 100; 512] |> List.to_seq |> Seq.map Z.of_int
  in
  Seq.iter
    (fun initial ->
      let tree = Tree.binary_search ~initial ~origin:Z.zero () in
      Regression.capture
      @@ Format.asprintf "Tree.binary_search ~initial:%s" (Z.to_string initial) ;
      capture_tree Z.to_string tree ;
      Regression.capture "Wtih crunch 1" ;
      capture_tree Z.to_string (tree |> Tree.crunch 1) ;
      Regression.capture "Wtih crunch 2" ;
      capture_tree Z.to_string (tree |> Tree.crunch 2) ;
      Regression.capture "Wtih crunch 5" ;
      capture_tree Z.to_string (tree |> Tree.crunch 5) )
    values ;
  Lwt.return_unit

let shrink () =
  Regression.register ~__FILE__ ~title:"Tree.shrink" ~tags:["tree"; "shrink"]
  @@ fun () ->
  let values = [100; 2100; 4200] |> List.to_seq |> Seq.map Z.of_int in
  let prop_a x =
    if Z.rem x (Z.of_int 3) = Z.zero && Z.gt x (Z.of_int 1000) then Error ()
    else Ok ()
  in
  let prop_b y =
    if Z.rem y (Z.of_int 7) = Z.zero && Z.gt y (Z.of_int 1000) then Error ()
    else Ok ()
  in
  let prop_c y = if y = Z.of_int 1321 then Error () else Ok () in
  let props = [("a", prop_a); ("b", prop_b); ("c", prop_c)] |> List.to_seq in
  let crunches =
    [(1, Tree.crunch 1); (2, Tree.crunch 2); (5, Tree.crunch 5)] |> List.to_seq
  in
  Seq.iter
    (fun initial ->
      Regression.capture
      @@ Format.asprintf "Tree.binary_search ~initial:%s ~origin:Z.zero ()"
           (Z.to_string initial) ;
      let tree = Tree.binary_search ~initial ~origin:Z.zero () in
      Seq.iter
        (fun (name, f) ->
          Regression.capture @@ Format.asprintf "Property %s" name ;
          Seq.iter
            (fun (level, crunch) ->
              Regression.capture
              @@ Format.asprintf "Tree.shrink: %s (crunch %d)"
                   (Z.to_string (Tree.shrink f (tree |> crunch)))
                   level )
            crunches )
        props )
    values ;
  Lwt.return_unit

let register () =
  binary_search () ;
  fractional_binary_search () ;
  crunch () ;
  shrink ()
