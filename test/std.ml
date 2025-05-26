open Tezt_bam

let std_int_range_inclusive_bounds () =
  Test.register ~__FILE__ ~title:"std int range inclusive bounds"
    ~tags:["std"; "int"; "range"]
  @@ fun () ->
  let values =
    Seq.ints 0 |> Seq.take 10_000
    |> Seq.map (fun i ->
           let gen = Bam.Std.int ~min:0 ~max:10 () in
           Bam.Gen.run gen (Bam.Gen.Random.make [|i|]) |> Bam.Tree.root )
    |> List.of_seq |> List.sort_uniq compare
  in
  let expected = List.init 11 Fun.id in
  if values = expected then Lwt.return_unit
  else Test.fail "Did not draw all the integers in the interval"

let std_oneof () =
  Regression.register ~__FILE__ ~title:"std oneof" ~tags:["std"; "oneof"]
  @@ fun () ->
  let values =
    Seq.ints 0 |> Seq.take 30
    |> Seq.map (fun i ->
           let small_gen = Bam.Std.int ~min:0 ~max:10 () in
           let medium_gen = Bam.Std.int ~min:100 ~max:200 () in
           let large_gen = Bam.Std.int ~min:1000 ~max:10_000 () in
           let gen =
             Bam.Std.oneof [(3, small_gen); (2, medium_gen); (1, large_gen)]
           in
           Bam.Gen.run gen (Bam.Gen.Random.make [|i|]) |> Bam.Tree.root )
    |> List.of_seq |> List.sort compare
  in
  let str = values |> List.map string_of_int |> String.concat " " in
  Regression.capture str ; Lwt.return_unit

let std_char_custom_root () =
  Test.register ~__FILE__ ~title:"std char custom root"
    ~tags:["std"; "char"; "root"]
  @@ fun () ->
  let gen = Bam.Std.char ~printable:false ~root:(Char.chr 5) () in
  let v =
    Bam.Gen.run gen (Bam.Gen.Random.make [|0|]) |> Bam.Tree.root |> Char.code
  in
  if v = 5 then Lwt.return_unit
  else Test.failf "expected 5 got %d" v

let register () =
  std_int_range_inclusive_bounds () ;
  std_oneof () ;
  std_char_custom_root ()
