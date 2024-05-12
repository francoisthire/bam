(* The following file is used as a regression file but also as a way to document
   by example how the PPX works and how the derived generator can be tuned.

   All the examples use the attributes [@@deriving_inline] [@@@end] so that the
   output of the derived generator is printed.

   This file is generated via the command [dune build]. If a difference is
   spotted when executing the command with the current content of the file, this
   will trigger an error. You can use the CLI option `--auto-promote` to update
   the current file.

   The PPX aims to generate rather readable generators. This way, the output of
   [@@deriving_inline] can directly be used if you don't want to rely on
   generated code.

   Don't mind the boilerplate code added by [ppxlib].
*)

(* Tests are organised into two sections:

   - Basics show use basic use-cases of the PPX

   - Advanced show how to tune the default behaviour of the PPX
*)

module Basics = struct
  (* This example shows basic generators and some default values used. *)

  (* Default values for int generators (and similar types) is the default values
     of the corresponding standard library functions. Also do note that the
     value generated is called [gen]. This is a special case when the name of
     the type is [t]. Otherwise, the name is [gen_] followed by the name of the
     type. *)
  type t = int [@@deriving_inline gen]

  let _ = fun (_ : t) -> ()

  let gen = Bam.Std.int ()

  let _ = gen

  [@@@end]

  (* Default size for types requiring a size argument is 10. This should be
     reasonable in practice and can be overrided by the user in any case *)
  type t2 = string [@@deriving_inline gen]

  let _ = fun (_ : t2) -> ()

  let gen_t2 = Bam.Std.string ~size:(Bam.Std.int ~max:10 ()) ()

  let _ = gen_t2

  [@@@end]

  type t3 = bool [@@deriving_inline gen]

  let _ = fun (_ : t3) -> ()

  let gen_t3 = Bam.Std.bool ()

  let _ = gen_t3

  [@@@end]

  type t4 = unit [@@deriving_inline gen]

  let _ = fun (_ : t4) -> ()

  let gen_t4 = Bam.Std.return ()

  let _ = gen_t4

  [@@@end]

  type t5 = char [@@deriving_inline gen]

  let _ = fun (_ : t5) -> ()

  let gen_t5 = Bam.Std.char ()

  let _ = gen_t5

  [@@@end]

  (* For variants, the default weight for each case is the same by default. *)
  type t6 = int option [@@deriving_inline gen]

  let _ = fun (_ : t6) -> ()

  let gen_t6 =
    let open Bam.Std.Syntax in
    let gen__001_ = Bam.Std.int () in
    Bam.Std.oneof
      [ (1, return None)
      ; ( 1
        , let* value = gen__001_ in
          return (Some value) ) ]

  let _ = gen_t6

  [@@@end]

  (* The PPX supports recursive types. Do note however that a unit argument is
     added because otherwise the code would be ill-typed. *)
  type t7 = {bar1: int; bar2: t8; bar3: string}

  and t8 = {truc: char; bidule: t7} [@@deriving_inline gen]

  let _ = fun (_ : t7) -> ()

  let _ = fun (_ : t8) -> ()

  let rec gen_t7 () =
    let open Bam.Std.Syntax in
    let* bar1 = Bam.Std.int () in
    let* bar2 = gen_t8 () in
    let* bar3 = Bam.Std.string ~size:(Bam.Std.int ~max:10 ()) () in
    return {bar1; bar2; bar3}

  and gen_t8 () =
    let open Bam.Std.Syntax in
    let* truc = Bam.Std.char () in
    let* bidule = gen_t7 () in
    return {truc; bidule}

  let _ = gen_t7

  and _ = gen_t8

  [@@@end]

  (* Objects types are not supported yet. Fortunately, one can override the
     default behaviour to specify ad-hoc generators. *)
  type t9 = < foo: int >

  (* [@@deriving_inline gen]

     let _ = fun (_ : t9) -> ()

     let gen_t9 = failwith "The 'gen' deriver  could not handle this case"

     let _ = gen_t9

     [@@@end] *)

  (* If a type occurs in the definition of another type, by default the deriver
     assume that a value with the name following the nomenclature used by the
     deriver exists.
  *)
  type t10 = t8 [@@deriving_inline gen]

  let _ = fun (_ : t10) -> ()

  let gen_t10 = gen_t8

  let _ = gen_t10

  [@@@end]

  type 'a check = 'a list [@@deriving_inline gen]

  let _ = fun (_ : 'a check) -> ()

  let gen_check gen_a = Bam.Std.list ~size:(Bam.Std.int ~max:10 ()) gen_a

  let _ = gen_check

  [@@@end]

  (* When implementing a polymorphic generator, the deriver expects each of the
     generator associated to type variables to be given in the order they appear in
     the type declaration. *)
  type ('a, 'b) t11 = ('a * 'b) list [@@deriving_inline gen]

  let _ = fun (_ : ('a, 'b) t11) -> ()

  let gen_t11 =
    let open Bam.Std.Syntax in
    fun gen_a gen_b ->
      let gen__002_ =
        let* arg_0 = gen_a in
        let* arg_1 = gen_b in
        return (arg_0, arg_1)
      in
      Bam.Std.list ~size:(Bam.Std.int ~max:10 ()) gen__002_

  let _ = gen_t11

  [@@@end]

  (* Another example with polymorphic variants. *)
  type 'a t12a = 'a list [@@deriving_inline gen]

  let _ = fun (_ : 'a t12a) -> ()

  let gen_t12a gen_a = Bam.Std.list ~size:(Bam.Std.int ~max:10 ()) gen_a

  let _ = gen_t12a

  [@@@end]

  type 'a t12b = A of 'a t12a | B of string [@@deriving_inline gen]

  let _ = fun (_ : 'a t12b) -> ()

  let gen_t12b =
    let open Bam.Std.Syntax in
    fun gen_a ->
      let gen_A =
        let* arg_0 = gen_t12a gen_a in
        return (A arg_0)
      in
      let gen_B =
        let* arg_0 = Bam.Std.string ~size:(Bam.Std.int ~max:10 ()) () in
        return (B arg_0)
      in
      Bam.Std.oneof [(1, gen_A); (1, gen_B)]

  let _ = gen_t12b

  [@@@end]

  type t12c = bool t12b list [@@deriving_inline gen]

  let _ = fun (_ : t12c) -> ()

  let gen_t12c =
    let gen__003_ = gen_t12b (Bam.Std.bool ()) in
    Bam.Std.list ~size:(Bam.Std.int ~max:10 ()) gen__003_

  let _ = gen_t12c

  [@@@end]

  (* A simpler example with arrays.*)
  type t13 = int array [@@deriving_inline gen]

  let _ = fun (_ : t13) -> ()

  let gen_t13 =
    let open Bam.Std.Syntax in
    let gen__004_ = Bam.Std.int () in
    let* list = Bam.Std.list ~size:(Bam.Std.int ~max:10 ()) gen__004_ in
    return (Array.of_list list)

  let _ = gen_t13

  [@@@end]

  (* GADTs are not supported yet. For this example, the deriver produces a
     ill-typed code. For such a use-case we expect the user to provide its own
     generator. *)
  type _ t14 = A : int -> int t14 | B : string -> string t14

  (* [@@deriving_inline gen]

     [@@@end]
  *)

  (* This GADt is not supported too. The deriver will create a not well-scoped
     expression for the type variable ['a]. It is up to the user to provide a generator. *)
  type t15 = A : 'a -> t15 (*
  [@@deriving_inline gen]

  [@@@end] *)

  (* The deriver support to derive a bunch of types at once if they are declared
     in a module type. *)
  module type Example = sig
    (* This example comes from another project. *)
    type key = string * int

    type value = Bytes.t

    type write_payload = {key: key; override: bool; default: bool}

    type action =
      | Write_value of write_payload
      | Read_value of key
      | Read_values of key Seq.t
      | Remove_file of string
      | Count_values of string

    type bind = Sequential | Parallel

    type scenario = action * (bind * action) list
  end
  [@@deriving_inline gen]

  include struct
    [@@@ocaml.warning "-60"]

    module Example = struct
      type key = string * int

      let gen_key =
        let open Bam.Std.Syntax in
        let* arg_0 = Bam.Std.string ~size:(Bam.Std.int ~max:10 ()) () in
        let* arg_1 = Bam.Std.int () in
        return (arg_0, arg_1)

      let _ = gen_key

      type value = Bytes.t

      let gen_value = Bam.Std.bytes ~size:(Bam.Std.int ~max:10 ()) ()

      let _ = gen_value

      type write_payload = {key: key; override: bool; default: bool}

      let gen_write_payload =
        let open Bam.Std.Syntax in
        let* key = gen_key in
        let* override = Bam.Std.bool () in
        let* default = Bam.Std.bool () in
        return {key; override; default}

      let _ = gen_write_payload

      type action =
        | Write_value of write_payload
        | Read_value of key
        | Read_values of key Seq.t
        | Remove_file of string
        | Count_values of string

      let gen_action =
        let open Bam.Std.Syntax in
        let gen_Write_value =
          let* arg_0 = gen_write_payload in
          return (Write_value arg_0)
        in
        let gen_Read_value =
          let* arg_0 = gen_key in
          return (Read_value arg_0)
        in
        let gen_Read_values =
          let* arg_0 =
            let* list = Bam.Std.list ~size:(Bam.Std.int ~max:10 ()) gen_key in
            return (List.to_seq list)
          in
          return (Read_values arg_0)
        in
        let gen_Remove_file =
          let* arg_0 = Bam.Std.string ~size:(Bam.Std.int ~max:10 ()) () in
          return (Remove_file arg_0)
        in
        let gen_Count_values =
          let* arg_0 = Bam.Std.string ~size:(Bam.Std.int ~max:10 ()) () in
          return (Count_values arg_0)
        in
        Bam.Std.oneof
          [ (1, gen_Write_value)
          ; (1, gen_Read_value)
          ; (1, gen_Read_values)
          ; (1, gen_Remove_file)
          ; (1, gen_Count_values) ]

      let _ = gen_action

      type bind = Sequential | Parallel

      let gen_bind =
        let open Bam.Std.Syntax in
        let gen_Sequential = return Sequential in
        let gen_Parallel = return Parallel in
        Bam.Std.oneof [(1, gen_Sequential); (1, gen_Parallel)]

      let _ = gen_bind

      type scenario = action * (bind * action) list

      let gen_scenario =
        let open Bam.Std.Syntax in
        let* arg_0 = gen_action in
        let* arg_1 =
          let gen__005_ =
            let* arg_0 = gen_bind in
            let* arg_1 = gen_action in
            return (arg_0, arg_1)
          in
          Bam.Std.list ~size:(Bam.Std.int ~max:10 ()) gen__005_
        in
        return (arg_0, arg_1)

      let _ = gen_scenario
    end
  end [@@ocaml.doc "@inline"]

  [@@@end]

  type t16 = int32 [@@deriving_inline gen]

  let _ = fun (_ : t16) -> ()

  let gen_t16 = Bam.Std.int32 ()

  let _ = gen_t16

  [@@@end]

  type t17 = int64 [@@deriving_inline gen]

  let _ = fun (_ : t17) -> ()

  let gen_t17 = Bam.Std.int64 ()

  let _ = gen_t17

  [@@@end]

  type t18 = Record of {a: int; b: string} [@@deriving_inline gen]

  let _ = fun (_ : t18) -> ()

  let gen_t18 =
    let open Bam.Std.Syntax in
    let* a = Bam.Std.int () in
    let* b = Bam.Std.string ~size:(Bam.Std.int ~max:10 ()) () in
    return (Record {a; b})

  let _ = gen_t18

  [@@@end]
end

module Advanced = struct
  (* The default weight of a constructor can be overrided via the [@weight] attribute. *)
  type t1 = A of int * string [@weight 100] | B [@@deriving_inline gen]

  let _ = fun (_ : t1) -> ()

  let gen_t1 =
    let open Bam.Std.Syntax in
    let gen_A =
      let* arg_0 = Bam.Std.int () in
      let* arg_1 = Bam.Std.string ~size:(Bam.Std.int ~max:10 ()) () in
      return (A (arg_0, arg_1))
    in
    let gen_B = return B in
    Bam.Std.oneof [(100, gen_A); (1, gen_B)]

  let _ = gen_t1

  [@@@end]

  (* At any point, one can override the generator using the attribute [@gen]. *)
  type t2 = A of (int * string) [@gen Bam.Std.return (A (0, ""))]
  [@@deriving_inline gen]

  let _ = fun (_ : t2) -> ()

  let gen_t2 = Bam.Std.return (A (0, ""))

  let _ = gen_t2

  [@@@end]

  (* The [@max] attribute can be attached at any point and change the maximum generated value for interval-like
     types such as [int],[int32],... It does not affect the maximum value for the size. *)
  type t3 = (int[@max 25]) [@@deriving_inline gen]

  let _ = fun (_ : t3) -> ()

  let gen_t3 = Bam.Std.int ~max:25 ()

  let _ = gen_t3

  [@@@end]

  (* The [@min] attribute works similarly. To modify the size, one can use
     [@size.max] and [@size.min]. *)
  type t4 = (string[@size.max 25]) * (int[@min 10] [@max 20])
  [@@deriving_inline gen]

  let _ = fun (_ : t4) -> ()

  let gen_t4 =
    let open Bam.Std.Syntax in
    let* arg_0 = Bam.Std.string ~size:(Bam.Std.int ~max:25 ()) () in
    let* arg_1 = Bam.Std.int ~min:10 ~max:20 () in
    return (arg_0, arg_1)

  let _ = gen_t4

  [@@@end]

  (* Another example using [@max] and [@@size.max]. Note that the attributes are
     attached to the whole type declaration here. *)
  type t5 = int * string [@@deriving_inline gen] [@@max 20] [@@size.max 40]

  let _ = fun (_ : t5) -> ()

  let gen_t5 =
    let open Bam.Std.Syntax in
    let* arg_0 = Bam.Std.int ~max:20 () in
    let* arg_1 = Bam.Std.string ~size:(Bam.Std.int ~max:40 ()) () in
    return (arg_0, arg_1)

  let _ = gen_t5

  [@@@end]

  type t6 = {a: (int[@max 20]); b: string} [@@deriving_inline gen] [@@min 5]

  let _ = fun (_ : t6) -> ()

  let gen_t6 =
    let open Bam.Std.Syntax in
    let* a = Bam.Std.int ~min:5 ~max:20 () in
    let* b = Bam.Std.string ~size:(Bam.Std.int ~max:10 ()) () in
    return {a; b}

  let _ = gen_t6

  [@@@end]

  (* To prevent that an attribute modify all the interval-like types, once can
     prefix [@max] with the type modified. *)
  type t7 = {a: (int[@min 20] [@size.min 40] [@int.min 30]); b: string}
  [@@deriving_inline gen]

  let _ = fun (_ : t7) -> ()

  let gen_t7 =
    let open Bam.Std.Syntax in
    let* a = Bam.Std.int ~min:30 () in
    let* b = Bam.Std.string ~size:(Bam.Std.int ~max:10 ()) () in
    return {a; b}

  let _ = gen_t7

  [@@@end]

  (* An example where the default generator is overrided with the attribute
     [@gen]. *)
  type t8 = A of (int[@gen Bam.Std.int ~max:20 ()]) [@@deriving_inline gen]

  let _ = fun (_ : t8) -> ()

  let gen_t8 =
    let open Bam.Std.Syntax in
    let* arg_0 = Bam.Std.int ~max:20 () in
    return (A arg_0)

  let _ = gen_t8

  [@@@end]

  (* Another example where the default generator is overrided with the attribute
     [@gen]. For polymorphic types, such generator is expected to take the
     generator associated to type parameters in arguments. *)
  type t9 =
    | Override of
        { a:
            (int option
            [@gen
              fun gen ->
                Bam.Std.oneof
                  [ (1000, Bam.Std.return None)
                  ; ( 2
                    , let* v = gen in
                      return (Some v) ) ]] )
        ; b: int option }
  [@@deriving_inline gen]

  let _ = fun (_ : t9) -> ()

  let gen_t9 =
    let open Bam.Std.Syntax in
    let* a =
      (fun gen ->
        Bam.Std.oneof
          [ (1000, Bam.Std.return None)
          ; ( 2
            , let* v = gen in
              return (Some v) ) ] )
        (Bam.Std.int ())
    in
    let* b =
      let gen__006_ = Bam.Std.int () in
      Bam.Std.oneof
        [ (1, return None)
        ; ( 1
          , let* value = gen__006_ in
            return (Some value) ) ]
    in
    return (Override {a; b})

  let _ = gen_t9

  [@@@end]

  (* If a generator for a type is missing, it can be overrided with [@gen]. *)
  type 'a missing = 'a list

  type 'a t10 = ('a missing[@gen Bam.Std.list ~size:(Bam.Std.int ~max:10 ())])
  [@@deriving_inline gen]

  let _ = fun (_ : 'a t10) -> ()

  let gen_t10 gen_a = Bam.Std.list ~size:(Bam.Std.int ~max:10 ()) gen_a

  let _ = gen_t10

  [@@@end]

  let my_int = Bam.Std.int ()

  (* For a given type, a generator can be overriden globally. The attribute is
     called [@gen.<type>]. In that case, the
     [@max],[@min].[@size.max].[@size.min] and other attributes won't be taken into
      account. *)
  type t11 = {a: (int[@min 5]); b: (int[@max 20])}
  [@@deriving_inline gen] [@@gen.int my_int]

  let _ = fun (_ : t11) -> ()

  let gen_t11 =
    let open Bam.Std.Syntax in
    let* a = my_int in
    let* b = my_int in
    return {a; b}

  let _ = gen_t11

  [@@@end]

  (* Another example where the option generator is override globally. *)
  let my_option gen =
    let open Bam.Std.Syntax in
    let some =
      let* v = gen in
      return (Some v)
    in
    Bam.Std.oneof [(2, Bam.Std.return None); (3, some)]

  type t12 = {a: int option} [@@deriving_inline gen] [@@gen.option my_option]

  let _ = fun (_ : t12) -> ()

  let gen_t12 =
    let open Bam.Std.Syntax in
    let* a = my_option (Bam.Std.int ()) in
    return {a}

  let _ = gen_t12

  [@@@end]

  (* An example o check weights are applied correctly. *)
  type t13 =
    | A of int [@weight 5] [@min 5] [@max 15]
    | B of string [@weight 4]
    | C of int [@weight 3]
  [@@deriving_inline gen]

  let _ = fun (_ : t13) -> ()

  let gen_t13 =
    let open Bam.Std.Syntax in
    let gen_A =
      let* arg_0 = Bam.Std.int ~min:5 ~max:15 () in
      return (A arg_0)
    in
    let gen_B =
      let* arg_0 = Bam.Std.string ~size:(Bam.Std.int ~max:10 ()) () in
      return (B arg_0)
    in
    let gen_C =
      let* arg_0 = Bam.Std.int () in
      return (C arg_0)
    in
    Bam.Std.oneof [(5, gen_A); (4, gen_B); (3, gen_C)]

  let _ = gen_t13

  [@@@end]

  (* An example showing the [@shrinker] attribute allowing to specify a
     shrinker. Such an attribute does not work everywhere. It must be specified
     either along side an atomic type or at a type declaration level for a variant.

     Tuples are not supported yet.
  *)
  type t14 =
    | Foo of {a: int; b: (string[@shrinker Bam.Std.Shrinker.skip_auto])}
    | Bar of int list
  [@@deriving_inline gen]

  let _ = fun (_ : t14) -> ()

  let gen_t14 =
    let open Bam.Std.Syntax in
    let gen_Foo =
      let* a = Bam.Std.int () in
      let* b =
        (Bam.Std.string ~shrinker:Bam.Std.Shrinker.skip_auto)
          ~size:(Bam.Std.int ~max:10 ()) ()
      in
      return (Foo {a; b})
    in
    let gen_Bar =
      let* arg_0 =
        let gen__007_ = Bam.Std.int () in
        Bam.Std.list ~size:(Bam.Std.int ~max:10 ()) gen__007_
      in
      return (Bar arg_0)
    in
    Bam.Std.oneof [(1, gen_Foo); (1, gen_Bar)]

  let _ = gen_t14

  [@@@end]
end

(* Just for linking with the other tests. *)
let register () = ()
