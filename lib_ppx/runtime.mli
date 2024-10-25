open Ppxlib

type t =
  { limits: Limits.t
  ; override: expression Ty.Map.t
  ; gen: expression option
  ; weight: int option
  ; shrinker: expression option
  ; use_monadic_syntax: bool ref }

val default : t

val get : t -> 'continuation Ty.t -> 'continuation
