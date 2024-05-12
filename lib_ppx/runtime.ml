open Ppxlib

let loc = !Ast_helper.default_loc

module Default = struct
  let unit = [%expr Bam.Std.return ()]

  let bool ~shrinker =
    match shrinker with
    | None ->
        [%expr Bam.Std.bool ()]
    | Some shrinker ->
        [%expr Bam.Std.bool ~shrinker:[%e shrinker] ()]

  let int ~shrinker ~min ~max () =
    let min = Option.map (Ast_builder.Default.eint ~loc) min in
    let max = Option.map (Ast_builder.Default.eint ~loc) max in
    let gen =
      match shrinker with
      | None ->
          [%expr Bam.Std.int]
      | Some shrinker ->
          [%expr Bam.Std.int ~shrinker:[%e shrinker]]
    in
    match (min, max) with
    | None, None ->
        [%expr [%e gen] ()]
    | Some min, None ->
        [%expr [%e gen] ~min:[%e min] ()]
    | None, Some max ->
        [%expr [%e gen] ~max:[%e max] ()]
    | Some min, Some max ->
        [%expr [%e gen] ~min:[%e min] ~max:[%e max] ()]

  let int32 ~shrinker ~min ~max () =
    let min = Option.map (Ast_builder.Default.eint32 ~loc) min in
    let max = Option.map (Ast_builder.Default.eint32 ~loc) max in
    let gen =
      match shrinker with
      | None ->
          [%expr Bam.Std.int32]
      | Some shrinker ->
          [%expr Bam.Std.int32 ~shrinker:[%e shrinker]]
    in
    match (min, max) with
    | None, None ->
        [%expr [%e gen] ()]
    | Some min, None ->
        [%expr [%e gen] ~min:[%e min] ()]
    | None, Some max ->
        [%expr [%e gen] ~max:[%e max] ()]
    | Some min, Some max ->
        [%expr [%e gen] ~min:[%e min] ~max:[%e max] ()]

  let int64 ~shrinker ~min ~max () =
    let min = Option.map (Ast_builder.Default.eint64 ~loc) min in
    let max = Option.map (Ast_builder.Default.eint64 ~loc) max in
    let gen =
      match shrinker with
      | None ->
          [%expr Bam.Std.int64]
      | Some shrinker ->
          [%expr Bam.Std.int64 ~shrinker:[%e shrinker]]
    in
    match (min, max) with
    | None, None ->
        [%expr [%e gen] ()]
    | Some min, None ->
        [%expr [%e gen] ~min:[%e min] ()]
    | None, Some max ->
        [%expr [%e gen] ~max:[%e max] ()]
    | Some min, Some max ->
        [%expr [%e gen] ~min:[%e min] ~max:[%e max] ()]

  let char ~shrinker =
    match shrinker with
    | None ->
        [%expr Bam.Std.char ()]
    | Some shrinker ->
        [%expr Bam.Std.char ~shrinker:[%e shrinker] ()]

  let string ~shrinker ~size_min ~size_max () =
    let size = int ~shrinker:None ~min:size_min ~max:size_max () in
    let gen =
      match shrinker with
      | None ->
          [%expr Bam.Std.string]
      | Some shrinker ->
          [%expr Bam.Std.string ~shrinker:[%e shrinker]]
    in
    [%expr [%e gen] ~size:[%e size] ()]

  let bytes ~shrinker ~size_min ~size_max () =
    let size = int ~shrinker:None ~min:size_min ~max:size_max () in
    let gen =
      match shrinker with
      | None ->
          [%expr Bam.Std.bytes]
      | Some shrinker ->
          [%expr Bam.Std.bytes ~shrinker:[%e shrinker]]
    in
    [%expr [%e gen] ~size:[%e size] ()]

  let option ~shrinker gen =
    let oneof_gen =
      match shrinker with
      | None ->
          [%expr Bam.Std.oneof]
      | Some shrinker ->
          [%expr Bam.Std.oneof ~shrinker:[%e shrinker]]
    in
    let none = [%expr return None] in
    let some =
      [%expr
        let* value = [%e gen] in
        return (Some value)]
    in
    [%expr [%e oneof_gen] [(1, [%e none]); (1, [%e some])]]

  let list ~shrinker ~size_min ~size_max gen =
    let list_gen =
      match shrinker with
      | None ->
          [%expr Bam.Std.list]
      | Some shrinker ->
          [%expr Bam.Std.list ~shrinker:[%e shrinker]]
    in
    [%expr
      [%e list_gen]
        ~size:[%e int ~shrinker:None ~min:size_min ~max:size_max ()]
        [%e gen]]

  let array ~shrinker ~size_min ~size_max gen =
    [%expr
      let* list = [%e list ~shrinker ~size_min ~size_max gen] in
      return (Array.of_list list)]

  let seq ~shrinker ~size_min ~size_max gen =
    [%expr
      let* list = [%e list ~shrinker ~size_min ~size_max gen] in
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

let get_default (type a) ~use_monadic_syntax shrinker limits : a Ty.t -> a =
  function
  | Unit ->
      Default.unit
  | Bool ->
      Default.bool ~shrinker
  | Char ->
      Default.char ~shrinker
  | Ranged Int ->
      let min = Limits.int_min limits in
      let max = Limits.int_max limits in
      Default.int ~shrinker ~min ~max ()
  | Ranged Int32 ->
      let min = Limits.int32_min limits in
      let max = Limits.int32_max limits in
      Default.int32 ~shrinker ~min ~max ()
  | Ranged Int64 ->
      let min = Limits.int64_min limits in
      let max = Limits.int64_max limits in
      Default.int64 ~shrinker ~min ~max ()
  | Sized String ->
      let size_min = Limits.sized_min limits (E String) in
      let size_max = Limits.sized_max limits (E String) in
      Default.string ~shrinker ~size_min ~size_max ()
  | Sized Bytes ->
      let size_min = Limits.sized_min limits (E Bytes) in
      let size_max = Limits.sized_max limits (E Bytes) in
      Default.bytes ~shrinker ~size_min ~size_max ()
  | Sized List ->
      let size_min = Limits.sized_min limits (E List) in
      let size_max = Limits.sized_max limits (E List) in
      pretty_apply (Default.list ~shrinker ~size_min ~size_max)
  | Sized Array ->
      let size_min = Limits.sized_min limits (E Array) in
      let size_max = Limits.sized_max limits (E Array) in
      use_monadic_syntax := true ;
      pretty_apply (Default.array ~shrinker ~size_min ~size_max)
  | Sized Seq ->
      let size_min = Limits.sized_min limits (E Seq) in
      let size_max = Limits.sized_max limits (E Seq) in
      use_monadic_syntax := true ;
      pretty_apply (Default.seq ~shrinker ~size_min ~size_max)
  | Option ->
      use_monadic_syntax := true ;
      pretty_apply (Default.option ~shrinker)
  | Any ->
      failwith "The 'gen' deriver  could not handle this case"

type t =
  { limits: Limits.t
  ; override: expression Ty.Map.t
  ; gen: expression option
  ; weight: int option
  ; shrinker: expression option
  ; use_monadic_syntax: bool ref }

let default =
  { limits= Limits.default
  ; override= Ty.Map.empty
  ; gen= None
  ; weight= None
  ; shrinker= None
  ; use_monadic_syntax= ref false }

let get (type a) {limits; override; use_monadic_syntax; gen; shrinker; _} :
    a Ty.t -> a =
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
      get_default ~use_monadic_syntax shrinker limits ty
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
