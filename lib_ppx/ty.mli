open Ppxlib

type _ ranged =
  | Int : expression ranged
  | Int32 : expression ranged
  | Int64 : expression ranged

val ranged_compare : 'a ranged -> 'b ranged -> ('a, 'b) Dmap.cmp

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

module Ranged_dmap : Dmap.S with type 'a key = 'a ranged

type st = E : 'a sized -> st

module Sized_map : Map.S with type key = st

type et = E : 'a t -> et

module Map : Map.S with type key = et
