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

let register () = std_int_range_inclusive_bounds ()
