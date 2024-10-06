open Ppxlib

type _ ranged =
  | Int : expression ranged
  | Int32 : expression ranged
  | Int64 : expression ranged

let ranged_compare (type a1 a2) : a1 ranged -> a2 ranged -> (a1, a2) Dmap.cmp =
 fun left right ->
  match (left, right) with
  | Int, Int ->
      Dmap.Eq
  | Int32, Int32 ->
      Dmap.Eq
  | Int64, Int64 ->
      Dmap.Eq
  | Int, _ ->
      Lt
  | Int32, Int ->
      Gt
  | Int32, _ ->
      Lt
  | Int64, Int ->
      Gt
  | Int64, Int32 ->
      Gt

type 'continuation sized =
  | String : expression sized
  | Bytes : expression sized
  | List : (expression -> expression) sized
  | Array : (expression -> expression) sized
  | Seq : (expression -> expression) sized

type 'continuation t =
  | Unit : expression t
  | Bool : expression t
  | Char : expression t
  | Ranged : _ ranged -> expression t
  | Sized : 'continuation sized -> 'continuation t
  | Option : (expression -> expression) t
  | Any : expression t

module Ranged_dmap = Dmap.Make (struct
  type 'a t = 'a ranged

  let compare = ranged_compare
end)

type st = E : _ sized -> st

module Sized_map = Map.Make (struct
  type t = st

  let compare = compare
end)

type et = E : _ t -> et

module Map = Map.Make (struct
  type t = et

  let compare = compare
end)
