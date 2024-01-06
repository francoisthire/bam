(* This module defines a monad transformer. For any monad ['a m] it
   returns a new monad ['a t] which can be thought as a tree of ['a m]
   (see module [Tree]).
*)

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
end) : S with type 'a m = 'a M.t
