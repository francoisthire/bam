type foo = int [@@deriving_inline gen]

let _ = fun (_ : foo) -> ()

let gen_foo = Bam.Std.int ()

let _ = gen_foo

[@@@end]

type foo2 = string [@@deriving_inline gen]

let _ = fun (_ : foo2) -> ()

let gen_foo2 = Bam.Std.string ~size:(Bam.Std.int ~max:10 ()) ()

let _ = gen_foo2

[@@@end]

type foo3 = bool [@@deriving_inline gen]

let _ = fun (_ : foo3) -> ()

let gen_foo3 = Bam.Std.bool ()

let _ = gen_foo3

[@@@end]

type foo4 = unit [@@deriving_inline gen]

let _ = fun (_ : foo4) -> ()

let gen_foo4 = Bam.Std.return ()

let _ = gen_foo4

[@@@end]

type foo5 = char [@@deriving_inline gen]

let _ = fun (_ : foo5) -> ()

let gen_foo5 = Bam.Std.char ()

let _ = gen_foo5

[@@@end]

type foo6 = int option [@@deriving_inline gen]

let _ = fun (_ : foo6) -> ()

let gen_foo6 =
  let open Bam.Std.Syntax in
  let gen__001_ = Bam.Std.int () in
  Bam.Std.oneof
    [ (1, return None)
    ; ( 1
      , let* value = gen__001_ in
        return (Some value) ) ]

let _ = gen_foo6

[@@@end]

type foo8 = {bar1: int; bar2: foo9; bar3: string}

and foo9 = {truc: char; bidule: foo8} [@@deriving_inline gen]

let _ = fun (_ : foo8) -> ()

let _ = fun (_ : foo9) -> ()

let rec gen_foo8 () =
  let open Bam.Std.Syntax in
  let* bar1 = Bam.Std.int () in
  let* bar2 = gen_foo9 () in
  let* bar3 = Bam.Std.string ~size:(Bam.Std.int ~max:10 ()) () in
  return {bar1; bar2; bar3}

and gen_foo9 () =
  let open Bam.Std.Syntax in
  let* truc = Bam.Std.char () in
  let* bidule = gen_foo8 () in
  return {truc; bidule}

let _ = gen_foo8

and _ = gen_foo9

[@@@end]

type foo10 = A of int * string [@weight 100] | B [@@deriving_inline gen]

let _ = fun (_ : foo10) -> ()

let gen_foo10 =
  let open Bam.Std.Syntax in
  let gen_A =
    let* arg_0 = Bam.Std.int () in
    let* arg_1 = Bam.Std.string ~size:(Bam.Std.int ~max:10 ()) () in
    return (A (arg_0, arg_1))
  in
  let gen_B = return B in
  Bam.Std.oneof [(100, gen_A); (1, gen_B)]

let _ = gen_foo10

[@@@end]

type foo11 = A of (int * string) [@gen Bam.Std.return (A (0, ""))]
[@@deriving_inline gen]

let _ = fun (_ : foo11) -> ()

let gen_foo11 = Bam.Std.return (A (0, ""))

let _ = gen_foo11

[@@@end]

type foo12 = foo11 [@@deriving_inline gen]

let _ = fun (_ : foo12) -> ()

let gen_foo12 = gen_foo11

let _ = gen_foo12

[@@@end]

type ('a, 'b) foo13 = ('a * 'b) list [@@deriving_inline gen]

let _ = fun (_ : ('a, 'b) foo13) -> ()

let gen_foo13 =
  let open Bam.Std.Syntax in
  fun gen_a gen_b ->
    let gen__002_ =
      let* arg_0 = gen_a in
      let* arg_1 = gen_b in
      return (arg_0, arg_1)
    in
    Bam.Std.list ~size:(Bam.Std.int ~max:10 ()) gen__002_

let _ = gen_foo13

[@@@end]

type 'a check = 'a list [@@deriving_inline gen]

let _ = fun (_ : 'a check) -> ()

let gen_check gen_a = Bam.Std.list ~size:(Bam.Std.int ~max:10 ()) gen_a

let _ = gen_check

[@@@end]

type 'a test = A of 'a check | B of string [@@deriving_inline gen]

let _ = fun (_ : 'a test) -> ()

let gen_test =
  let open Bam.Std.Syntax in
  fun gen_a ->
    let gen_A =
      let* arg_0 = gen_check gen_a in
      return (A arg_0)
    in
    let gen_B =
      let* arg_0 = Bam.Std.string ~size:(Bam.Std.int ~max:10 ()) () in
      return (B arg_0)
    in
    Bam.Std.oneof [(1, gen_A); (1, gen_B)]

let _ = gen_test

[@@@end]

type scenario = bool test list [@@deriving_inline gen]

let _ = fun (_ : scenario) -> ()

let gen_scenario =
  let gen__003_ = gen_test (Bam.Std.bool ()) in
  Bam.Std.list ~size:(Bam.Std.int ~max:10 ()) gen__003_

let _ = gen_scenario

[@@@end]

type gadt = A : int -> gadt | B : string -> gadt [@@deriving_inline gen]

let _ = fun (_ : gadt) -> ()

let gen_gadt =
  let open Bam.Std.Syntax in
  let gen_A =
    let* arg_0 = Bam.Std.int () in
    return (A arg_0)
  in
  let gen_B =
    let* arg_0 = Bam.Std.string ~size:(Bam.Std.int ~max:10 ()) () in
    return (B arg_0)
  in
  Bam.Std.oneof [(1, gen_A); (1, gen_B)]

let _ = gen_gadt

[@@@end]

type arr = int array [@@deriving_inline gen]

let _ = fun (_ : arr) -> ()

let gen_arr =
  let open Bam.Std.Syntax in
  let gen__004_ = Bam.Std.int () in
  let* list = Bam.Std.list ~size:(Bam.Std.int ~max:10 ()) gen__004_ in
  return (Array.of_list list)

let _ = gen_arr

[@@@end]

let register () = ()

module Gen = struct
  type key = string * int [@@deriving_inline gen]

  let _ = fun (_ : key) -> ()

  let gen_key =
    let open Bam.Std.Syntax in
    let* arg_0 = Bam.Std.string ~size:(Bam.Std.int ~max:10 ()) () in
    let* arg_1 = Bam.Std.int () in
    return (arg_0, arg_1)

  let _ = gen_key

  [@@@end]

  type value = Bytes.t [@@deriving_inline gen]

  let _ = fun (_ : value) -> ()

  let gen_value = Bam.Std.bytes ~size:(Bam.Std.int ~max:10 ()) ()

  let _ = gen_value

  [@@@end]

  type write_payload = {key: key; override: bool; default: bool}
  [@@deriving_inline gen]

  let _ = fun (_ : write_payload) -> ()

  let gen_write_payload =
    let open Bam.Std.Syntax in
    let* key = gen_key in
    let* override = Bam.Std.bool () in
    let* default = Bam.Std.bool () in
    return {key; override; default}

  let _ = gen_write_payload

  [@@@end]

  type action =
    | Write_value of write_payload
    | Read_value of key
    | Read_values of key Seq.t
    | Remove_file of string
    | Count_values of string
  [@@deriving_inline gen]

  let _ = fun (_ : action) -> ()

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

  [@@@end]

  type bind = Sequential | Parallel [@@deriving_inline gen]

  let _ = fun (_ : bind) -> ()

  let gen_bind =
    let open Bam.Std.Syntax in
    let gen_Sequential = return Sequential in
    let gen_Parallel = return Parallel in
    Bam.Std.oneof [(1, gen_Sequential); (1, gen_Parallel)]

  let _ = gen_bind

  [@@@end]

  type scenario = action * (bind * action) list [@@deriving_inline gen]

  let _ = fun (_ : scenario) -> ()

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

  [@@@end]
end

module type Gen3 = sig
  type t = int

  type t2 = string

  type t4 = t2
end
[@@deriving_inline gen]

include struct
  [@@@ocaml.warning "-60"]

  module Gen3 = struct
    let gen = Bam.Std.int ()

    let _ = gen

    let gen_t2 = Bam.Std.string ~size:(Bam.Std.int ~max:10 ()) ()

    let _ = gen_t2

    let gen_t4 = gen_t2

    let _ = gen_t4
  end
end [@@ocaml.doc "@inline"]

[@@@end]

module type Foo = sig
  type t = (int[@max 25])
end
[@@deriving_inline gen]

include struct
  [@@@ocaml.warning "-60"]

  module Foo = struct
    let gen = Bam.Std.int ~max:25 ()

    let _ = gen
  end
end [@@ocaml.doc "@inline"]

[@@@end]

type c0 = (string[@size.max 25]) * (int[@min 10] [@max 20])
[@@deriving_inline gen]

let _ = fun (_ : c0) -> ()

let gen_c0 =
  let open Bam.Std.Syntax in
  let* arg_0 = Bam.Std.string ~size:(Bam.Std.int ~max:25 ()) () in
  let* arg_1 = Bam.Std.int ~min:10 ~max:20 () in
  return (arg_0, arg_1)

let _ = gen_c0

[@@@end]

type c1 = A of (int[@gen Bam.Std.int ~max:20 ()]) [@@deriving_inline gen]

let _ = fun (_ : c1) -> ()

let gen_c1 =
  let open Bam.Std.Syntax in
  let* arg_0 = Bam.Std.int ~max:20 () in
  return (A arg_0)

let _ = gen_c1

[@@@end]

type c2 = int * string [@@deriving_inline gen] [@@max 20] [@@size.max 40]

let _ = fun (_ : c2) -> ()

let gen_c2 =
  let open Bam.Std.Syntax in
  let* arg_0 = Bam.Std.int ~max:20 () in
  let* arg_1 = Bam.Std.string ~size:(Bam.Std.int ~max:40 ()) () in
  return (arg_0, arg_1)

let _ = gen_c2

[@@@end]

type c3 = {a: (int[@max 20]); b: string} [@@deriving_inline gen] [@@min 5]

let _ = fun (_ : c3) -> ()

let gen_c3 =
  let open Bam.Std.Syntax in
  let* a = Bam.Std.int ~min:5 ~max:20 () in
  let* b = Bam.Std.string ~size:(Bam.Std.int ~max:10 ()) () in
  return {a; b}

let _ = gen_c3

[@@@end]

type c4 = {a: (int[@min 20] [@size.min 40] [@int.min 30]); b: string}
[@@deriving_inline gen]

let _ = fun (_ : c4) -> ()

let gen_c4 =
  let open Bam.Std.Syntax in
  let* a = Bam.Std.int ~min:30 () in
  let* b = Bam.Std.string ~size:(Bam.Std.int ~max:10 ()) () in
  return {a; b}

let _ = gen_c4

[@@@end]

type c5 = int32 [@@deriving_inline gen]

let _ = fun (_ : c5) -> ()

let gen_c5 = Bam.Std.int32 ()

let _ = gen_c5

[@@@end]

type c6 = int64 [@@deriving_inline gen]

let _ = fun (_ : c6) -> ()

let gen_c6 = Bam.Std.int64 ()

let _ = gen_c6

[@@@end]

type c7 = Record of {a: int; b: string} [@@deriving_inline gen]

let _ = fun (_ : c7) -> ()

let gen_c7 =
  let open Bam.Std.Syntax in
  let* a = Bam.Std.int () in
  let* b = Bam.Std.string ~size:(Bam.Std.int ~max:10 ()) () in
  return (Record {a; b})

let _ = gen_c7

[@@@end]

type c8 =
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

let _ = fun (_ : c8) -> ()

let gen_c8 =
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

let _ = gen_c8

[@@@end]

type 'a missing = 'a list

type 'a c9 = ('a missing[@gen Bam.Std.list ~size:(Bam.Std.int ~max:10 ())])
[@@deriving_inline gen]

let _ = fun (_ : 'a c9) -> ()

let gen_c9 gen_a = Bam.Std.list ~size:(Bam.Std.int ~max:10 ()) gen_a

let _ = gen_c9

[@@@end]

let my_int = Bam.Std.int ()

type c10 = {a: (int[@min 5]); b: (int[@max 20])}
[@@deriving_inline gen] [@@gen.int my_int]

let _ = fun (_ : c10) -> ()

let gen_c10 =
  let open Bam.Std.Syntax in
  let* a = my_int in
  let* b = my_int in
  return {a; b}

let _ = gen_c10

[@@@end]

let my_option gen =
  let open Bam.Std.Syntax in
  let some =
    let* v = gen in
    return (Some v)
  in
  Bam.Std.oneof [(2, Bam.Std.return None); (3, some)]

type c11 = {a: int option} [@@deriving_inline gen] [@@gen.option my_option]

let _ = fun (_ : c11) -> ()

let gen_c11 =
  let open Bam.Std.Syntax in
  let* a = my_option (Bam.Std.int ()) in
  return {a}

let _ = gen_c11

[@@@end]

type c12 =
  | A of int [@weight 5] [@min 5] [@max 15]
  | B of string [@weight 4]
  | C of int [@weight 3]
[@@deriving_inline gen]

let _ = fun (_ : c12) -> ()

let gen_c12 =
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

let _ = gen_c12

[@@@end]
