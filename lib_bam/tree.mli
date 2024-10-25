(** {1 Tree} *)

(** This module encodes a non-bounded branching tree. Values are
    generated lazily via the module [Seq].
*)

(** Abstract representation of a tree. *)
type 'a t

(** Convenient alias if ['a t] is shadowed. *)
type 'a tree = 'a t

val make : 'a -> ('a -> 'a Seq.t) -> 'a t
(** [make f root] returns a tree with root [root] and the children are
    recursively produced (lazily) with [f]. *)

val of_seq : 'a Seq.t -> 'a -> 'a t
(** [of_seq seq root] returns a tree whose root is [root] and the
    children are generated via [seq]. Hence, if [seq] is not empty,
    the depth of the tree is [1]. *)

val root : 'a t -> 'a
(** [root tree] returns the root of the tree. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f tree] maps [f] to all the elements of the tree. The merge
    function is reset to [Seq.append]. There is no automatic way to
    preserve the merge behavior with [map]. However, you can save the
    previous behavior with [get_merge] and set a new behavior with
    [with_merge]. *)

(** {2 Searching functions as trees} *)

val binary_search : initial:Z.t -> origin:Z.t -> unit -> Z.t t
(** [binary_search ~initial ~origin] implements a binary search
    enumerating elements. Elements are guaranteed to be in the
    interval [origin;initial].

    Note: If [initial < origin], it returns a tree with the single
    node [origin].
*)

val fractional_search :
     ?exhaustive_search_digits:int
  -> ?precision_digits:int
  -> initial:float
  -> origin:float
  -> unit
  -> float t
(** [fractional_search ?exhaustive_search_digits ?precision_digits
    ~initial ~origin ()] returns a tree of depth one where the root is
    [initial] and children are ordered by the prefix ordering (modulo
    float representations) starting with [origin]. The children are
    always float between [origin] and [initial] starting with floats
    with few digits. *)

val linear_search : initial:Z.t -> origin:Z.t -> unit -> Z.t t
(** [linear_search] returns a tree of depth 1 whose [root] is
    [initial] and children are the number from [origin] included to
    [initial] excluded. *)

(** {2 A monadic interface} *)

val return : 'a -> 'a t
(** [return value] returns a tree containing a single node with [value]. *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [bind tree f] returns a tree where [f] is applied (lazily) to all
    the values of [tree]. Since [f] returns itself a tree, [bind] must
    be able to merge values of [tree] with the ones produced by
    [f]. This can be done via the merging process specified by the
    tree returned by [f]. *)

module Syntax : sig
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  (** Alias for {!bind} *)

  val return : 'a -> 'a t
  (** Alias for {!return} *)
end

(** {2 Merging values} *)

(** By using [bind], one needs to be able to merge sequences of
    trees. To understand why, let's give an informal definition of
    [bind]:

    {[
    let rec bind tree f =
       let {root;children=left} = tree in
       (* children : 'a t Seq.t *)
       let {root; children=right} f root in
       (* How should we combine the sequence of trees denotes by [left] and [right]? *)
    ]}

    A natural way to merging them as denoted by the variable names is
    to use [Seq.append]. However, this is an arbitrary
    choice. Libraries using this module may redefine the default
    merging procedure to enable more complex behaviors.

    Even though [Seq.append] is arbitrary, in practice it leads to
    predictable and easy to unerstand behaviors.

    Notice that in the definition above, there are two trees, hence
    maybe two different merging behaviors. However, by typing, only
    one is allowed: the one resulting of the application of [f root].

    The type of the [merge] prevents ['a t] to be an applicative
    functor.
*)

val with_merge : merge:('a t Seq.t -> 'a t Seq.t -> 'a t Seq.t) -> 'a t -> 'a t
(** [with_merge ~merge tree] sets the merging behavior as [merge]. *)

val get_merge : 'a t -> 'a t Seq.t -> 'a t Seq.t -> 'a t Seq.t
(** [get_merge tree] returns the merge function for this tree. *)

(** {2 Forest} *)

module Forest : sig
  (** A forest can be considered as a non-empty sequence of trees. The
      functions declared in this module transposed naturally the
      functions provided on [tree].

      Function of this modules always ensure that the sequence is not
      empty. This is why the function [sequence] takes two arguments
      instead of one.

      Because of the non-emptiness property, the function [first] is
      total.

      Names of those functions are inspired from Haskell. *)

  (** Datatype for a forest. *)
  type 'a t

  val lift : 'a tree -> 'a t
  (** [lift tree] returns a forest made of a single tree. *)

  val make : 'a -> ('a -> 'a Seq.t) -> 'a t
  (** [make root ] produces a forest with a single tree by using {!make} *)

  val uncons : 'a t -> 'a tree * 'a tree Seq.t
  (** [uncons forest] splits the forest into a tree and a sequence of
      trees. *)

  val sequence : 'a t -> 'a t Seq.t -> 'a t
  (** [seqnece tree forest] returns a new forest made of a sequence of
      forest, namely [Seq.cons tree forest |> Seq.concat]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f forest] applies a function [f] over each value of the forest. *)

  val map_tree : ('a tree -> 'b tree) -> 'a t -> 'b t
  (** [map_tree f forest] applies a function [f] over each tree of the forest. *)

  val crunch : int -> 'a t -> 'a t
  (** [crunch i forest] is the analogue of {!crunch} *)

  (** {3 A monadic interface} *)

  val return : 'a -> 'a t
  (** [return v] returns a forest made of a single value. *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (** [bind forest f] is a generalisation of [bind] over sequence of
      trees. *)

  module Syntax : sig
    (** Monadic operators. *)

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    (** Alias for {!bind} *)

    val return : 'a -> 'a t
    (** Alias for {!return} *)
  end
end

(** {2 Shrinking} *)

val shrink : ('a -> ('ok, 'err) Result.t) -> 'a t -> 'a
(** [shrink tree f] returns a value [a] that has the following
    specification:

    - [f a = Error _]

    - [path(a,tree.root) = true]

    - forall v, if [v \in path(a, tree.root)] then [f v = Error _]

    - if [v' = Left(v)] and [v \in path(a, tree.root)] then [f v = Ok _]

    Assuming that [f tree.root] is [Error _].
*)

val crunch : int -> 'a t -> 'a t
(** [crunch i tree] returns a tree where the row [i] is merged with
    row [i-1]. Hence, when [i <= 1], this is the identity function. *)

(**/**)

val row : int -> 'a t -> 'a Seq.t
(** [row i tree] returns the [i]th row of the tree. If there is no
    node at row [i], the empty sequence is returned. *)

val dfs : 'a t -> 'a Seq.t
(** [dfs tree] enumerates the values of [tree] using a depth-first
    search. *)

val dfs_with_depth : 'a t -> (int * 'a) Seq.t
(** [dfs_with_depth] is the same as [dfs] but returns the depth of each node. *)
