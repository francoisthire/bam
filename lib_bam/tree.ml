(* A representation of a tree with maybe infinite branching. Children
   are evaluated lazily. Children aims to encode smaller values than
   the root. There is no order, this is ensured by the user of the
   module. See [shrink] and [bind] to see how it is used. *)
type 'a t =
  { root: 'a
  ; children: 'a t Seq.t
  ; merge: 'a t Seq.t -> 'a t Seq.t -> 'a t Seq.t
        (* This component say how children should be merged using [bind]. *) }

type 'a tree = 'a t

(* By default, children are merged using [Seq.append]. This is the
   natural way to merge children. *)
let default_merge = Seq.append

let root {root; _} = root

let return : 'a -> 'a t =
 fun value -> {root= value; children= Seq.empty; merge= default_merge}

let rec make make_children root =
  { root
  ; children= Seq.map (make make_children) (make_children root)
  ; merge= default_merge }

let make root make_children = make make_children root

let rec with_merge ~merge tree =
  { root= tree.root
  ; children= Seq.map (fun tree -> with_merge ~merge tree) tree.children
  ; merge }

let get_merge {merge; _} = merge

(* This simple function will not be used in practice. It is a
   particular case of the one defined in the module [Forest]. *)
let rec bind : 'a t -> ('a -> 'b t) -> 'b t =
 (* We are making an arbitrary choice by using [default_merge] here:

    We could concatenate the right sequence first or we could even
    give the opportunity to the user to chose the ordering. From
    experiments, it seems that using [Seq.append] works pretty-well.

    To define more complex behaviors, the user may redefine how merge
    is done. This enalbes new possibilities for shrinking. *)
 fun a f ->
  let seq_left = Seq.map (fun tree -> bind tree f) a.children in
  let b = f a.root in
  let seq_right = b.children in
  let children = b.merge seq_left seq_right in
  {root= b.root; children; merge= b.merge}

module Syntax = struct
  let ( let* ) = bind

  let return = return
end

let rec map f x =
  {root= f x.root; children= Seq.map (map f) x.children; merge= Seq.append}

module Seq = struct
  (* [Seq] is a monad. *)
  include Seq

  let bind x f = Seq.map f x |> Seq.concat

  let snoc x seq = default_merge seq (Seq.return x)
end

let crunch : int -> 'a t -> 'a t =
 fun depth tree ->
  let depths = Seq.ints 0 |> Seq.take depth in
  let fold tree _ =
    let children =
      Seq.flat_map
        (fun tree -> Seq.snoc (return tree.root) tree.children)
        tree.children
    in
    {tree with children}
  in
  Seq.fold_left fold tree depths

module Forest = struct
  type 'a t = 'a tree Seq.t
  (* Invariant: forall rs t, Seq.length (t rs) > 0

     This invariant is ensured by the function of this module. We
     could let the user ensuring this invariant or ensuring it via the
     type system. For simplicity, efficiency, and because the module
     is short, it seems better to guarantee it this way. *)

  let return v = Seq.return (return v)

  let lift tree = Seq.return tree

  let make root make_children =
    let tree = make root make_children in
    Seq.return tree

  (* This is just a mere generalisation of the [bind] function defined
     previously. It is somehow surprising that this function cannot be
     defined in term of the other one. *)
  let bind : 'a t -> ('a -> 'b t) -> 'b t =
   fun a f ->
    let rec bind a f =
      let seq_left = Seq.bind a.children (fun tree -> bind tree f) in
      let bs = f a.root in
      Seq.map
        (fun b ->
          let seq_right = b.children in
          let children = b.merge seq_left seq_right in
          {root= b.root; children; merge= b.merge} )
        bs
    in
    Seq.bind a (fun tree -> bind tree f)

  (* This function takes two arguments to ensure the sequence is not empty. *)
  let sequence gen seq = Seq.cons gen seq |> Seq.concat

  let map_tree = Seq.map

  let map f = Seq.map (map f)

  let uncons seq =
    match Seq.uncons seq with
    | None ->
        (* This invariant is ensured by the module itself. *)
        assert false
    | Some (x, seq) ->
        (x, seq)

  let crunch i seq = Seq.map (crunch i) seq

  module Syntax = struct
    let ( let* ) = bind

    let return = return
  end
end

let of_seq seq root =
  { root
  ; children=
      Seq.map
        (fun root -> {root; children= Seq.empty; merge= default_merge})
        seq
  ; merge= default_merge }

let linear_search ~initial ~origin () =
  Seq.ints (Z.to_int origin)
  |> Seq.map Z.of_int
  |> Seq.take_while (fun x -> x < initial)
  |> Fun.flip of_seq initial

(* There is probably a better way to write this function. One cannot
   simply using [make] because we need the two bounds of the
   interval of the binary search. *)
let rec positive_binary_search ~initial ~origin () =
  let open Z in
  let open Z.Compare in
  if initial <= origin then
    {root= origin; children= Seq.empty; merge= default_merge}
  else
    let initials =
      Seq.ints 0
      |> Seq.map (fun power -> Z.one lsl power)
      |> Seq.map (fun power -> origin + power)
      |> Seq.take_while (fun x -> x >= Z.zero && x < initial - Z.one)
      |> Seq.snoc (initial - Z.one)
      |> Seq.cons origin
    in
    let mins = Seq.cons origin (initials |> Seq.map (fun x -> x + Z.one)) in
    let children =
      Seq.zip initials mins
      |> Seq.filter_map (fun (initial, origin) ->
             if origin > initial then None
             else positive_binary_search ~initial ~origin () |> Option.some )
    in
    {root= initial; children; merge= default_merge}

let binary_search : initial:Z.t -> origin:Z.t -> unit -> Z.t t =
 fun ~initial ~origin () ->
  let open Z.Compare in
  let shift = origin in
  let initial = Z.sub initial shift in
  (* Invariant: origin is 0 *)
  let inversion = if initial < Z.zero then Z.neg else Fun.id in
  let initial = inversion initial in
  (* Invariant: initial >= 0 *)
  positive_binary_search ~initial ~origin:Z.zero ()
  |> map (fun x -> Z.add (inversion x) shift)

let fractional_search ~exhaustive_search_digits ~precision_digits ~initial
    ~origin () =
  let rec power i =
    if i = 0 then 1 else if i = 1 then 10 else 10 * power (i - 1)
  in
  (* Reverting digits allows to get first floats closer to [origin]. *)
  let digits = Seq.ints 0 |> Seq.take 10 |> Seq.map (fun i -> 9 - i) in
  (* This function truncates the digits of a fractional number. Only
     [bit] digit are kept. *)
  let simplify f bit digit =
    let factor = power bit |> float_of_int in
    let number = Float.modf (f *. factor) |> snd |> int_of_float in
    let last_digit = Int.rem number 10 in
    if digit > last_digit && last_digit <> 0 then None
    else
      let x = Float.of_int (max 0 (number - digit)) /. factor in
      if x >= origin && x <= initial then Some x else None
  in
  let children =
    let left =
      (* This sequence will compute all floating number whose string
         representation contains at most [bits] digits and are in the
         interval [origin;initial]. The sequence is ordered so that
         float number which are closer to origin and whose string
         representation has fewer digits appear first. *)
      let bits = Seq.ints 1 |> Seq.take exhaustive_search_digits in
      let fold acc bit =
        acc
        |> Seq.map (fun initial ->
               digits
               |> Seq.filter_map (fun digit -> simplify initial bit digit) )
        |> Seq.concat
      in
      Seq.scan fold (Seq.return initial) bits
      |> Seq.fold_left Seq.append Seq.empty
      |> Seq.filter (fun x -> x <> origin && x <> initial)
      |> Seq.cons origin |> Seq.map return
    in
    let start = exhaustive_search_digits + 1 in
    let rec right initial bit precision =
      Seq.ints bit |> Seq.take precision
      |> Seq.map (fun bit ->
             digits |> Seq.filter_map (fun digit -> simplify initial bit digit) )
      |> Seq.concat
      |> Seq.filter (fun x -> x <> origin && x <> initial)
      |> fun seq ->
      (* This sequence will generate number whose digital
         representation is changing one digit at a time the [initial]
         number to get closer to [origin]. *)
      Seq.append (seq |> Seq.map return)
      (* This complicated sequence is useful only with
         [Tree.crunch]. It enables to get more clues. This is very
         similar to an exhaustive search. *)
      @@ ( Seq.map
             (fun root ->
               if bit < precision then
                 { root
                 ; children= right root start (bit + 1)
                 ; merge= default_merge }
               else {root; children= Seq.empty; merge= default_merge} )
             seq
         |> Seq.map (fun {children; _} -> children)
         |> Seq.concat
         |> fun seq ->
         {root= origin; children= seq; merge= default_merge} |> Seq.return )
    in
    Seq.append left (right initial start precision_digits)
  in
  {root= initial; children; merge= default_merge}

let fractional_search ?(exhaustive_search_digits = 0) ?(precision_digits = 20)
    ~initial ~origin () =
  let shift = origin in
  let initial = initial -. shift in
  let inversion = if initial < 0. then Float.neg else Fun.id in
  let initial = inversion initial in
  let precision_digits = max exhaustive_search_digits precision_digits in
  fractional_search ~exhaustive_search_digits ~precision_digits ~initial
    ~origin:0. ()
  |> map (fun x -> inversion x +. shift)

let rec shrink : ('a -> ('ok, 'err) Result.t) -> 'a t -> 'a =
 fun f tree ->
  (* Correctness of this function assumes that the function [f] fails
     when it is called on the root of the tree. This function is
     looking for "smaller" values than the current root tree for which
     the function [f] fails. Children of a tree aims to be smaller
     values than the root. *)
  let find_best_candidate tree =
    (* If we found a smaller value on which the function fails,
       we recursively shrink to find an even better one. *)
    (* This is not tail-recursive, we can limit the depth of the
       search tree to overcome this issue. *)
    match f tree.root with
    | Error _ ->
        shrink f tree |> Option.some
    | Ok _ ->
        None
  in
  Seq.find_map find_best_candidate tree.children
  |> Option.value ~default:tree.root

(* This one is a particular case of [dfs_with_depth]. *)
let rec dfs : 'a t -> 'a Seq.t =
 fun tree -> Seq.cons tree.root (Seq.flat_map dfs tree.children)

let dfs_with_depth : 'a t -> (int * 'a) Seq.t =
 fun tree ->
  let rec loop depth tree =
    Seq.cons (depth, tree.root) (Seq.flat_map (loop (depth + 1)) tree.children)
  in
  loop 0 tree

let rec row : int -> 'a t -> 'a Seq.t =
 fun depth tree ->
  if depth <= 0 then Seq.return tree.root
  else Seq.flat_map (fun tree -> row (depth - 1) tree) tree.children
