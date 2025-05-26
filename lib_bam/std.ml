module Syntax = Gen.Syntax
open Syntax

module Shrinker = struct
  type 'a t =
    | Default : 'a t
    | Manual : ('a -> 'a Seq.t) -> 'a t
    | Pair_left : ('a * 'b) t
    | Pair_right : ('a * 'b) t
    | Pair_compare : ('a * 'b -> 'a * 'b -> Int.t) -> ('a * 'b) t
    | Char : Char.t -> Char.t t
    | Bool : Bool.t -> Bool.t t
    | Float : Float.t -> Float.t t
    | Float_precision :
        {exhaustive_search_digits: int; precision_digits: int; target: Float.t}
        -> Float.t t
    | Int : Int.t -> Int.t t
    | Int32 : Int32.t -> Int32.t t
    | Int64 : Int64.t -> Int64.t t
    | Skip :
        {eq: 'a -> 'a -> bool; skip: [`Auto | `Int of Int.t]}
        -> 'a List.t t
    | Prefix : 'a List.t t
    | Suffix : 'a List.t t

  let skip_auto = Skip {eq= ( = ); skip= `Auto}
end

type 'a t = 'a Gen.t

let return = Gen.return

let bind = Gen.bind

let root = Gen.root

let crunch = Gen.crunch

let of_seq = Gen.of_seq

let int ?root ?(shrinker = Shrinker.Default) ?(min = 0) ?(max = Int.max_int) ()
    =
  let range ?origin ~min ~max () =
    let origin = Option.map Z.of_int origin in
    let root = Option.map Z.of_int root in
    Gen.z_range ?root ?origin ~min:(Z.of_int min) ~max:(Z.of_int max) ()
    |> Gen.map Z.to_int
  in
  match shrinker with
  | Manual shrinker ->
      let gen = range ~min ~max () in
      Gen.root gen (fun x -> Gen.make x shrinker)
  | Default ->
      range ~min ~max ()
  | Int n ->
      range ~origin:n ~min ~max ()

let int32 ?root ?(shrinker = Shrinker.Default) ?(min = Int32.zero)
    ?(max = Int32.max_int) () =
  let range ?origin ~min ~max () =
    let origin = Option.map Z.of_int32 origin in
    let root = Option.map Z.of_int32 root in
    Gen.z_range ?root ?origin ~min:(Z.of_int32 min) ~max:(Z.of_int32 max) ()
    |> Gen.map Z.to_int32
  in
  match shrinker with
  | Manual shrinker ->
      let gen = range ~min ~max () in
      Gen.root gen (fun x -> Gen.make x shrinker)
  | Default ->
      range ~min ~max ()
  | Int32 n ->
      range ~origin:n ~min ~max ()

let int64 ?root ?(shrinker = Shrinker.Default) ?(min = Int64.zero)
    ?(max = Int64.max_int) () =
  let range ?origin ~min ~max () =
    let origin = Option.map Z.of_int64 origin in
    let root = Option.map Z.of_int64 root in
    Gen.z_range ?root ?origin ~min:(Z.of_int64 min) ~max:(Z.of_int64 max) ()
    |> Gen.map Z.to_int64
  in
  match shrinker with
  | Manual shrinker ->
      let gen = range ~min ~max () in
      Gen.root gen (fun x -> Gen.make x shrinker)
  | Default ->
      range ~min ~max ()
  | Int64 n ->
      range ~origin:n ~min ~max ()

let float ?root ?(shrinker = Shrinker.Default) ?(min = 0.)
    ?(max = Float.max_float) () =
  match shrinker with
  | Manual shrinker ->
      let*! root = Gen.float_range ?root ~min ~max () in
      Gen.make root shrinker
  | Default ->
      Gen.float_range ?root ~min ~max ()
  | Float f ->
      Gen.float_range ?root ~origin:f ~min ~max ()
  | Float_precision {exhaustive_search_digits; precision_digits; target} ->
      Gen.float_range ?root ~exhaustive_search_digits ~precision_digits
        ~origin:target ~min ~max ()

let pair ?(shrinker = Shrinker.Default) left right =
  match shrinker with
  | Manual shrinker ->
      let*! a = left in
      let*! b = right in
      Gen.make (a, b) shrinker
  | Default | Pair_left ->
      let* a = left in
      let* b = right in
      return (a, b)
  | Pair_right ->
      let* b = right in
      let* a = left in
      return (a, b)
  | Pair_compare compare ->
      let* a = left in
      let* b = right in
      return (a, b) |> Gen.(with_merge (Merge.of_compare ~compare))

(* Use an unbiased integer range [0;1] to generate booleans. *)
let bool ?(shrinker = Shrinker.Default) () =
  match shrinker with
  | Manual shrinker ->
      let*! root = int ~min:0 ~max:1 () in
      Gen.make (root = 0) shrinker
  | Default ->
      let* x = int ~min:0 ~max:1 () in
      if x = 0 then return false else return true
  | Bool b ->
      let* x = int ~min:0 ~max:1 () in
      if x = 0 = b then return true else return false

let char ?root ?(shrinker = Shrinker.Default) ?(printable = true) () =
  let base = if printable then Char.code 'a' else 0 in
  let max = if printable then 26 else 256 in
  let root = root |> Option.map (fun root -> Char.code root - Char.code 'a') in
  match shrinker with
  | Manual shrinker ->
      let*! root = int ?root ~min:0 ~max () in
      Gen.make (Char.chr (base + root)) shrinker
  | Default ->
      let* offset = int ?root ~min:0 ~max () in
      return (Char.chr (base + offset))
  | Char c ->
      let origin = Char.code c - base in
      let* offset = int ?root ~shrinker:(Int origin) ~min:0 ~max () in
      return (Char.chr (base + offset))

module Gen_list = struct
  let simple_list : size:int Gen.t -> 'a Gen.t -> 'a list Gen.t =
   fun ~size gen ->
    let open Gen.Syntax in
    let* size = size in
    let rec loop n acc =
      if n = 0 then Gen.return (List.rev acc)
      else
        let* x = gen in
        loop (n - 1) (x :: acc)
    in
    loop size []

  let approximation_first_row_length_log10 n m =
    let n = float_of_int n in
    let m = float_of_int m in
    ((n *. log n) -. (m *. log m) -. ((n -. m) *. log (n -. m))) /. log 10.

  let default_maximum_number_of_skipped_elements n =
    let default_limit = 6. in
    let rec loop candidate =
      if candidate >= n then n
      else
        let approx = approximation_first_row_length_log10 n candidate in
        if approx < default_limit then loop (candidate + 1) else candidate
    in
    loop 1

  let skip ~eq max_skipped ~original_size ~size ~index:_ ~skipped x original_x =
    if not (eq x original_x) then `Default
    else if original_size - size > skipped && skipped < max_skipped then `Both
    else `Default

  let skip_smart ~original_size =
    let max_skipped =
      default_maximum_number_of_skipped_elements original_size
    in
    skip max_skipped ~original_size

  let suffix ~original_size ~size ~index ~skipped _ _ =
    if index + skipped < original_size - size then `Skip else `Default

  let list :
         shrink:[< `Suffix | `Skip of ('a -> 'a -> bool) * [`Auto | `Int of int]]
      -> size:int Gen.t
      -> 'a Gen.t
      -> 'a list Gen.t =
   fun ~shrink ~size gen ->
    let open Gen.Syntax in
    let shrink =
      match shrink with
      | `Suffix ->
          suffix
      | `Skip (eq, `Auto) ->
          skip_smart ~eq
      | `Skip (eq, `Int n) ->
          skip n ~eq
    in
    let*! original_size = size in
    let* size = size in
    let shrink = shrink ~original_size ~size in
    let rec loop skipped n acc =
      if n = 0 then Gen.return (List.rev acc)
      else
        (* This returns always the initial value produced by the generator. *)
        let*! original_x = gen in
        let alt =
          loop (skipped + 1) n acc |> Gen.(with_merge Merge.drop_left)
        in
        (* The [x] can be any node which is part of the tree defined
           by the generator during the shrinking process. *)
        let* x = gen in
        let main = loop skipped (n - 1) (x :: acc) in
        match shrink ~index:(size - n) ~skipped x original_x with
        | `Both ->
            Gen.sequence main (Seq.return alt)
        | `Default ->
            Gen.sequence main Seq.empty
        | `Skip ->
            Gen.sequence alt Seq.empty
    in
    loop 0 size []

  let gen ?(shrinker = Shrinker.Default) ~size gen =
    match shrinker with
    | Manual shrinker ->
        let*! root = simple_list ~size gen in
        Gen.make root shrinker
    | Default | Prefix ->
        simple_list ~size gen
    | Suffix ->
        list ~shrink:`Suffix ~size gen
    | Skip {eq; skip} ->
        list ~shrink:(`Skip (eq, skip)) ~size gen
end

let list = Gen_list.gen

let string ?shrinker ?(char = char ()) ~size () =
  let* chars = list ?shrinker ~size char in
  chars |> List.to_seq |> String.of_seq |> return

let bytes ?shrinker ?(char = char ~printable:false ()) ~size () =
  let* chars = list ?shrinker ~size char in
  chars |> List.to_seq |> Bytes.of_seq |> return

let oneof : ?shrinker:int Shrinker.t -> (int * 'a Gen.t) list -> 'a Gen.t =
 fun ?shrinker weighted_list ->
  let weights, _gens = List.split weighted_list in
  if List.exists (fun x -> x < 0) weights then
    raise @@ invalid_arg "[Std.oneof] was called with a negative weight." ;
  let total = List.fold_left ( + ) 0 weights in
  if total = 0 then
    raise
    @@ invalid_arg
         "[Std.oneof] was called with a weighted_list whose weights sum is 0." ;
  let* i = int ?shrinker ~min:0 ~max:(total - 1) () in
  List.fold_left
    (fun (acc, gen') (weight, gen) ->
      match gen' with
      | Some gen ->
          (acc, Some gen)
      | None ->
          if i < acc + weight then (acc, Some gen) else (acc + weight, None) )
    (0, None) weighted_list
  |> snd
  |> function None -> assert false | Some gen -> gen

let oneofg : ?shrinker:int Shrinker.t -> 'a Gen.t list -> 'a Gen.t =
 fun ?shrinker list -> oneof ?shrinker (List.map (fun gen -> (1, gen)) list)

let oneofl : ?shrinker:int Shrinker.t -> 'a list -> 'a Gen.t =
 fun ?shrinker list -> oneofg ?shrinker (List.map Gen.return list)
