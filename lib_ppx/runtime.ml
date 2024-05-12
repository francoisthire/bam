open Ppxlib

let loc = !Ast_helper.default_loc

module Default = struct
  let unit = [%expr Bam.Std.return ()]

  let bool = [%expr Bam.Std.bool ()]

  let int ~min ~max () =
    let min = Option.map (Ast_builder.Default.eint ~loc) min in
    let max = Option.map (Ast_builder.Default.eint ~loc) max in
    match (min, max) with
    | None, None ->
        [%expr Bam.Std.int ()]
    | Some min, None ->
        [%expr Bam.Std.int ~min:[%e min] ()]
    | None, Some max ->
        [%expr Bam.Std.int ~max:[%e max] ()]
    | Some min, Some max ->
        [%expr Bam.Std.int ~min:[%e min] ~max:[%e max] ()]

  let int32 ~min ~max () =
    let min = Option.map (Ast_builder.Default.eint32 ~loc) min in
    let max = Option.map (Ast_builder.Default.eint32 ~loc) max in
    match (min, max) with
    | None, None ->
        [%expr Bam.Std.int32 ()]
    | Some min, None ->
        [%expr Bam.Std.int32 ~min:[%e min] ()]
    | None, Some max ->
        [%expr Bam.Std.int32 ~max:[%e max] ()]
    | Some min, Some max ->
        [%expr Bam.Std.int32 ~min:[%e min] ~max:[%e max] ()]

  let int64 ~min ~max () =
    let min = Option.map (Ast_builder.Default.eint64 ~loc) min in
    let max = Option.map (Ast_builder.Default.eint64 ~loc) max in
    match (min, max) with
    | None, None ->
        [%expr Bam.Std.int64 ()]
    | Some min, None ->
        [%expr Bam.Std.int64 ~min:[%e min] ()]
    | None, Some max ->
        [%expr Bam.Std.int64 ~max:[%e max] ()]
    | Some min, Some max ->
        [%expr Bam.Std.int64 ~min:[%e min] ~max:[%e max] ()]

  let char () = [%expr Bam.Std.char ()]

  let string ~size_min ~size_max () =
    let size = int ~min:size_min ~max:size_max () in
    [%expr Bam.Std.string ~size:[%e size] ()]

  let bytes ~size_min ~size_max () =
    let size = int ~min:size_min ~max:size_max () in
    [%expr Bam.Std.bytes ~size:[%e size] ()]

  let option gen =
    let none = [%expr return None] in
    let some =
      [%expr
        let* value = [%e gen] in
        return (Some value)]
    in
    [%expr Bam.Std.oneof [(1, [%e none]); (1, [%e some])]]

  let list ~size_min ~size_max gen =
    [%expr Bam.Std.list ~size:[%e int ~min:size_min ~max:size_max ()] [%e gen]]

  let array ~size_min ~size_max gen =
    [%expr
      let* list = [%e list ~size_min ~size_max gen] in
      return (Array.of_list list)]

  let seq ~size_min ~size_max gen =
    [%expr
      let* list = [%e list ~size_min ~size_max gen] in
      return (List.to_seq list)]
end

let pretty_apply f a =
  (* A heuristic that introduce a let in if it can make the output more
     readable. *)
  if Helpers.is_identifier_expr a then f a
  else
    let var = gen_symbol ~prefix:"gen" () in
    let evar = Ast_builder.Default.evar ~loc var in
    let pvar = Ast_builder.Default.pvar ~loc var in
    [%expr
      let [%p pvar] = [%e a] in
      [%e f evar]]

let get_default (type a) ~use_monadic_syntax limits : a Ty.t -> a = function
  | Unit ->
      Default.unit
  | Bool ->
      Default.bool
  | Char ->
      Default.char ()
  | Ranged Int ->
      let min = Limits.int_min limits in
      let max = Limits.int_max limits in
      Default.int ~min ~max ()
  | Ranged Int32 ->
      let min = Limits.int32_min limits in
      let max = Limits.int32_max limits in
      Default.int32 ~min ~max ()
  | Ranged Int64 ->
      let min = Limits.int64_min limits in
      let max = Limits.int64_max limits in
      Default.int64 ~min ~max ()
  | Sized String ->
      let size_min = Limits.sized_min limits (E String) in
      let size_max = Limits.sized_max limits (E String) in
      Default.string ~size_min ~size_max ()
  | Sized Bytes ->
      let size_min = Limits.sized_min limits (E Bytes) in
      let size_max = Limits.sized_max limits (E Bytes) in
      Default.bytes ~size_min ~size_max ()
  | Sized List ->
      let size_min = Limits.sized_min limits (E List) in
      let size_max = Limits.sized_max limits (E List) in
      pretty_apply (Default.list ~size_min ~size_max)
  | Sized Array ->
      let size_min = Limits.sized_min limits (E Array) in
      let size_max = Limits.sized_max limits (E Array) in
      use_monadic_syntax := true ;
      pretty_apply (Default.array ~size_min ~size_max)
  | Sized Seq ->
      let size_min = Limits.sized_min limits (E Seq) in
      let size_max = Limits.sized_max limits (E Seq) in
      use_monadic_syntax := true ;
      pretty_apply (Default.seq ~size_min ~size_max)
  | Option ->
      use_monadic_syntax := true ;
      pretty_apply Default.option
  | Any ->
      failwith "The 'gen' deriver  could not handle this case"

type t =
  { limits: Limits.t
  ; override: expression Ty.Map.t
  ; gen: expression option
  ; weight: int option
  ; use_monadic_syntax: bool ref }

let default =
  { limits= Limits.default
  ; override= Ty.Map.empty
  ; gen= None
  ; weight= None
  ; use_monadic_syntax= ref false }

let get (type a) {limits; override; use_monadic_syntax; gen; _} : a Ty.t -> a =
 fun ty ->
  (* A generator can be overrided twice. Either by specifying a default override
     for a given type, or locally by specifying a generator at the definition
     point. The last one has a higher priority. *)
  let override =
    match gen with
    | None -> (
      match Ty.Map.find_opt (E ty) override with
      | None ->
          None
      | Some gen ->
          Some gen )
    | Some gen ->
        Some gen
  in
  match override with
  | None ->
      get_default ~use_monadic_syntax limits ty
  | Some gen -> (
    match ty with
    | Unit ->
        gen
    | Bool ->
        gen
    | Char ->
        gen
    | Ranged Int ->
        gen
    | Ranged Int32 ->
        gen
    | Ranged Int64 ->
        gen
    | Sized String ->
        gen
    | Sized Bytes ->
        gen
    | Sized List ->
        fun expression -> [%expr [%e gen] [%e expression]]
    | Sized Array ->
        fun expression -> [%expr [%e gen] [%e expression]]
    | Sized Seq ->
        fun expression -> [%expr [%e gen] [%e expression]]
    | Option ->
        fun expression -> [%expr [%e gen] [%e expression]]
    | Any ->
        gen )
