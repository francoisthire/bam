open Ppxlib
include Attribute
open Runtime
open Ty

module State_monad = struct
  type ('node, 'state) t = 'node -> 'state -> 'node * 'state

  module Syntax = struct
    let ( let* ) x f ct state =
      let ct, state = x ct state in
      f () ct state

    let return ct state = (ct, state)
  end
end

let get_attribute attribute node runtime =
  match Attribute.consume_res attribute node with
  | Error _ ->
      (node, runtime)
  | Ok (Some (ct, attribute)) ->
      (ct, attribute runtime)
  | Ok None ->
      (node, runtime)

let update :
    ('node, 'state -> 'state) Attribute.t list -> ('node, 'state) State_monad.t
    =
 fun attributes ->
  let open State_monad.Syntax in
  let base node runtime = (node, runtime) in
  List.fold_left
    (fun acc attr ->
      let* () = acc in
      let* () = get_attribute attr in
      return )
    base attributes

module Generic : sig
  (* This module declares a set of attributes that can be included at any context. Any of those attributes can modify the runtime environment. *)
  val attributes :
    'node Context.t -> ('node, Runtime.t -> Runtime.t) Attribute.t list
end = struct
  let min context =
    Attribute.declare "gen.min" context
      Ast_pattern.(single_expr_payload (eint __))
      (fun min runtime ->
        {runtime with limits= {runtime.limits with min= Some min}} )

  let max context =
    Attribute.declare "gen.max" context
      Ast_pattern.(single_expr_payload (eint __))
      (fun max runtime ->
        {runtime with limits= {runtime.limits with max= Some max}} )

  let int_min context =
    Attribute.declare "gen.int.min" context
      Ast_pattern.(single_expr_payload (eint __))
      (fun min runtime ->
        { runtime with
          limits=
            { runtime.limits with
              ranged_min= Ranged_dmap.add Int min runtime.limits.ranged_min } }
        )

  let int_max context =
    Attribute.declare "gen.int.max" context
      Ast_pattern.(single_expr_payload (eint __))
      (fun max runtime ->
        { runtime with
          limits=
            { runtime.limits with
              ranged_max= Ranged_dmap.add Int max runtime.limits.ranged_max } }
        )

  let int32_min context =
    Attribute.declare "gen.int32.min" context
      Ast_pattern.(single_expr_payload (eint32 __))
      (fun min runtime ->
        { runtime with
          limits=
            { runtime.limits with
              ranged_min= Ranged_dmap.add Int32 min runtime.limits.ranged_min }
        } )

  let int32_max context =
    Attribute.declare "gen.int32.max" context
      Ast_pattern.(single_expr_payload (eint32 __))
      (fun max runtime ->
        { runtime with
          limits=
            { runtime.limits with
              ranged_max= Ranged_dmap.add Int32 max runtime.limits.ranged_max }
        } )

  let int64_min context =
    Attribute.declare "gen.int64.min" context
      Ast_pattern.(single_expr_payload (eint64 __))
      (fun min runtime ->
        { runtime with
          limits=
            { runtime.limits with
              ranged_min= Ranged_dmap.add Int64 min runtime.limits.ranged_min }
        } )

  let int64_max context =
    Attribute.declare "gen.int64.max" context
      Ast_pattern.(single_expr_payload (eint64 __))
      (fun max runtime ->
        { runtime with
          limits=
            { runtime.limits with
              ranged_max= Ranged_dmap.add Int64 max runtime.limits.ranged_max }
        } )

  let size_min context =
    Attribute.declare "gen.size.min" context
      Ast_pattern.(single_expr_payload (eint __))
      (fun size_min runtime ->
        { runtime with
          limits= {runtime.limits with size_min= Some (Int.max 0 size_min)} } )

  let size_max context =
    Attribute.declare "gen.size.max" context
      Ast_pattern.(single_expr_payload (eint __))
      (fun size_max runtime ->
        {runtime with limits= {runtime.limits with size_max= Some size_max}} )

  let string_size_min context =
    Attribute.declare "gen.string.size.min" context
      Ast_pattern.(single_expr_payload (eint __))
      (fun size_min runtime ->
        { runtime with
          limits=
            { runtime.limits with
              sized_min=
                Sized_map.add (E String) (Int.max 0 size_min)
                  runtime.limits.sized_min } } )

  let string_size_max context =
    Attribute.declare "gen.string.size.max" context
      Ast_pattern.(single_expr_payload (eint __))
      (fun size_max runtime ->
        { runtime with
          limits=
            { runtime.limits with
              sized_max=
                Sized_map.add (E String) (Int.max 0 size_max)
                  runtime.limits.sized_max } } )

  let overrides =
    [ ("unit", E Unit)
    ; ("bool", E Bool)
    ; ("char", E Char)
    ; ("int", E (Ranged Int))
    ; ("int32", E (Ranged Int32))
    ; ("int64", E (Ranged Int64))
    ; ("string", E (Sized String))
    ; ("bytes", E (Sized Bytes))
    ; ("list", E (Sized List))
    ; ("array", E (Sized Array))
    ; ("seq", E (Sized Seq))
    ; ("option", E Option)
    ; ("any", E Any) ]

  let gen_override context (name, ty) =
    Attribute.declare ("gen." ^ name) context
      Ast_pattern.(single_expr_payload __)
      (fun gen runtime ->
        {runtime with override= Ty.Map.add ty gen runtime.override} )

  let gen_overrides context = overrides |> List.map (gen_override context)

  let gen context =
    Attribute.declare "gen.gen" context
      Ast_pattern.(single_expr_payload __)
      (fun gen runtime -> {runtime with gen= Some gen})

  let attributes context =
    [ min context
    ; max context
    ; int_min context
    ; int_max context
    ; int32_min context
    ; int32_max context
    ; int64_min context
    ; int64_max context
    ; size_min context
    ; size_max context
    ; string_size_min context
    ; string_size_max context
    ; gen context ]
    @ gen_overrides context
end

module Type_declaration : sig
  val update : (type_declaration, Runtime.t) State_monad.t
end = struct
  let attributes = Generic.attributes Attribute.Context.type_declaration

  (* This one can be used for variants only.*)
  let shrinker =
    Attribute.declare "gen.shrinker" Attribute.Context.type_declaration
      Ast_pattern.(single_expr_payload __)
      (fun shrinker runtime -> {runtime with shrinker= Some shrinker})

  let update = update (shrinker :: attributes)
end

module Label_declaration : sig
  val update : (label_declaration, Runtime.t) State_monad.t
end = struct
  let attributes = Generic.attributes Attribute.Context.label_declaration

  let update = update attributes
end

module Constructor_declaration : sig
  val update : (constructor_declaration, Runtime.t) State_monad.t
end = struct
  let attributes = Generic.attributes Attribute.Context.constructor_declaration

  let weight =
    Attribute.declare "gen.weight" Attribute.Context.constructor_declaration
      Ast_pattern.(single_expr_payload (eint __))
      (fun weight runtime -> {runtime with weight= Some weight})

  let update = update (weight :: attributes)
end

module Core_type : sig
  val update : (core_type, Runtime.t) State_monad.t
end = struct
  let attributes = Generic.attributes Attribute.Context.core_type

  let shrinker =
    Attribute.declare "gen.shrinker" Attribute.Context.core_type
      Ast_pattern.(single_expr_payload __)
      (fun shrinker runtime -> {runtime with shrinker= Some shrinker})

  let update = update (shrinker :: attributes)
end
