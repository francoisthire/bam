module type S = sig
  type 'a m

  type 'a t

  val lift : 'a m -> 'a t

  val return : 'a -> 'a t

  val fmap : ('a m -> 'b m) -> 'a t -> 'b t

  val make : 'a m Seq.t -> 'a m -> 'a t

  val bind : 'a t -> ('a m -> 'b t) -> 'b t

  val default : 'a t -> 'a m

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b m) -> 'b t

    val return : 'a -> 'a m
  end
end

module Make (M : sig
  type 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t
end) : S with type 'a m = 'a M.t = struct
  type 'a m = 'a M.t

  type 'a t = 'a M.t Tree.t

  let lift v = Tree.return v

  let return v = M.return v |> Tree.return

  let bind x f = Tree.bind x f

  let fmap f t = bind t (fun gen -> lift @@ f gen)

  let make seq root = Tree.of_seq seq root

  let default = Tree.root

  module Syntax = struct
    let ( let* ) (x : 'a t) (f : 'a -> 'b M.t) : 'b t =
      fmap (fun gen -> M.bind gen f) x

    let return = M.return
  end
end
