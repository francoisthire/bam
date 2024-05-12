open Tezt_bam

type t = Foo of {a: int; b: string} | Bar of int list
[@@deriving gen] [@@size.min 1] [@@size.max 10]

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
  Pbt.register ~pp ~__FILE__ ~title:"Deriving generators"
    ~tags:["bam"; "deriving_generators"]
    ~gen ~property ()
