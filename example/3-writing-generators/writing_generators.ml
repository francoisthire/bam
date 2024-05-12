open Tezt_bam

(** The deriver creates a value [val gen : t Bam.Std.t]. *)
type t = Foo of {a: int; b: string} | Bar of int list [@@deriving gen]

let gen =
  let open Bam.Std.Syntax in
  let gen_Foo =
    let* a = Bam.Std.int () in
    let* b = Bam.Std.string ~size:(Bam.Std.int ~max:10 ()) () in
    return (Foo {a; b})
  in
  let gen_Bar =
    let* arg_0 = Bam.Std.list ~size:(Bam.Std.int ~max:10 ()) (Bam.Std.int ()) in
    return (Bar arg_0)
  in
  Bam.Std.oneof [(1, gen_Foo); (1, gen_Bar)]

let pp fmt = function
  | Foo {a; b} ->
      Format.fprintf fmt "Foo {x=%d;%s}" a b
  | Bar list ->
      Format.fprintf fmt "Bar %a"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_int)
        list

let register () =
  let property = function
    | Foo {a; b} ->
        if a > 1_000 && String.contains b 'z' then
          Error (`Fail "A counter-example was found")
        else Ok ()
    | Bar [1; 2; 3; 4] ->
        Error `Bad_value
    | Bar _ ->
        Ok ()
  in
  Pbt.register ~pp ~__FILE__ ~title:"Writing generators"
    ~tags:["bam"; "writing_generators"]
    ~gen ~property ()
