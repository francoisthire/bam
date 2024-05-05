open Ty

type t =
  { min: int option
  ; max: int option
  ; size_min: int option
  ; size_max: int option
  ; ranged_min: Ranged_dmap.t
  ; ranged_max: Ranged_dmap.t
  ; sized_min: int Sized_map.t
  ; sized_max: int Sized_map.t }

let default =
  { min= None
  ; max= None
  ; size_min= None
  ; size_max= Some 10
  ; ranged_min= Ranged_dmap.empty
  ; ranged_max= Ranged_dmap.empty
  ; sized_min= Sized_map.empty
  ; sized_max= Sized_map.empty }

let int_min limits =
  match Ranged_dmap.find_opt Int limits.ranged_min with
  | None ->
      limits.min
  | Some i ->
      Some i

let int_max limits =
  match Ranged_dmap.find_opt Int limits.ranged_max with
  | None ->
      limits.max
  | Some i ->
      Some i

let int32_min limits =
  match Ranged_dmap.find_opt Int32 limits.ranged_min with
  | None ->
      limits.min |> Option.map Int32.of_int
  | Some i ->
      Some i

let int32_max limits =
  match Ranged_dmap.find_opt Int32 limits.ranged_max with
  | None ->
      limits.max |> Option.map Int32.of_int
  | Some i ->
      Some i

let int64_min limits =
  match Ranged_dmap.find_opt Int64 limits.ranged_min with
  | None ->
      limits.min |> Option.map Int64.of_int
  | Some i ->
      Some i

let int64_max limits =
  match Ranged_dmap.find_opt Int64 limits.ranged_max with
  | None ->
      limits.max |> Option.map Int64.of_int
  | Some i ->
      Some i

let sized_min limits sized =
  match Sized_map.find_opt sized limits.sized_min with
  | None ->
      limits.size_min
  | Some i ->
      Some i

let sized_max limits sized =
  match Sized_map.find_opt sized limits.sized_max with
  | None ->
      limits.size_max
  | Some i ->
      Some i
