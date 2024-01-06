open Tezt_bam

type t = A of {x: int; b: bool} | B of string

let pp fmt = function
  | A {x; b} ->
      Format.fprintf fmt "A {x=%d;%b}" x b
  | B str ->
      Format.fprintf fmt "B %s" str

let register () =
  let gen =
    let open Std.Syntax in
    let a =
      let* x = Std.int () in
      let* b = Std.bool () in
      return (A {x; b})
    in
    let b =
      let* str = Std.string ~size:(Std.int ~min:1 ~max:10 ()) () in
      return (B str)
    in
    Std.oneof [(1, a); (1, b)]
  in
  let property = function
    | B str ->
        if String.get str 0 = 'c' then Ok ()
        else Error (`Fail "string did not start with 'c'")
    | A _ ->
        Ok ()
  in
  Pbt.register ~pp ~__FILE__ ~title:"Writing generators"
    ~tags:["bam"; "writing_generators"]
    ~gen ~property ()
