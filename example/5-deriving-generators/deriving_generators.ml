open Tezt_bam

type t = A of {x: int; b: bool} | B of string
[@@deriving gen] [@@size.min 1] [@@size.max 10]

let pp fmt = function
  | A {x; b} ->
      Format.fprintf fmt "A {x=%d;%b}" x b
  | B str ->
      Format.fprintf fmt "B %s" str

let register () =
  let property = function
    | B str ->
        if String.get str 0 = 'c' then Ok ()
        else Error (`Fail "string did not start with 'c'")
    | A _ ->
        Ok ()
  in
  Pbt.register ~pp ~__FILE__ ~title:"Deriving generators"
    ~tags:["bam"; "deriving_generators"]
    ~gen ~property ()
