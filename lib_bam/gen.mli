(** This module is similar to the [Random] module exposed by the
    standard library except it contains also a [split] function. This
    function exists in the {!Random} module of the standard library
    since OCaml 5.0. We use this one for compatibility with previous
    OCaml versions. *)
module Random : PRNG.PURE

(** {1:basic Generators} *)

(** Datatype for generators of type ['a]. A generator should be
    thought as a tree (or for some usage a sequence of tree (see
    {!sequence}). The root of the tree is the generated value. Children
    of the tree are "smaller" values that can be used during the
    shrinking.

    While shrinking, it may be interesting to produce multiple
    values. This is why there is a {!sequence} function and this is
    why a generator is actually a sequence of trees. A typical use
    case is a shrinker for lists with the [Skip] strategy. Skipping an
    element of the list during the shrinking process is the same as
    producing two trees: one with the element and one without.

    However, when calling {!run}, the generator should always be a
    single tree (a sequence of length one). It is only during the
    shrinking that sequences of more trees may appear. This invariant
    is not guaranteed by the type system (see {!run}).

    To understand how shrinking works, you should have a look at
    {{!page-shrinking} A primer on shrinking}.
*)
type 'a t

val make : 'a -> ('a -> 'a Seq.t) -> 'a t
(** [make root make_children] builds a generator out of a root value
    [root] and a function to make children from their father. This can
    build an infinite tree if the function [f] never returns an empty
    sequence. *)

val z_range : ?root:Z.t -> ?origin:Z.t -> min:Z.t -> max:Z.t -> unit -> Z.t t
(** [z_range ?root ?shrink min max] returns a generator producing a
    uniform value between [min] (inclusive) and [max] (exclusive). It
    shrinks towards the value [origin] using a binary search (see
    [Tree.binary_search]. By default [origin] is [0] if [0] is in the
    interval [min;max]. Otherwise [origin] is set to [min].

    If [root] is specified, the value returned is [root] with the same
    shrinking tree as if the random generator had produce this value.
*)

val float_range :
     ?root:float
  -> ?exhaustive_search_digits:int
  -> ?precision_digits:int
  -> ?origin:float
  -> min:float
  -> max:float
  -> unit
  -> float t
(** [float_range ?root ?shrink min max] returns a generator producing
    a value between [min] (inclusive) and [max] (exclusive). It
    shrinks towards the value [min] using a binary search (see
    [Tree.binary_search]. The generator is not uniform. In particular
    when the fractional part of [min] and [max] are getting closer to
    [0.5], the generator may tend to create more values equal to [min]
    or [max]. If the fractional part is [0.], it should be uniform.

    If [root] is specified, the value returned is [root] with the same
    shrinking tree as if the random generator had produce this value.
*)

val run : ?on_failure:(string -> 'a Tree.t) -> 'a t -> Random.t -> 'a Tree.t
(** [run ?on_failure gen state] runs the generator [gen] on the
    [state] given in parameter. It is expected that the generator
    returns only a single tree. This is always true except if the
    generator used {!val-sequence}. See its documentation for more
    details. *)

(** {2:monad A monadic interface} *)

val return : 'a -> 'a t
(** [return v] returns a generator generating the value [v]. There is
    no shrinking defined for this generator. *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [bind gen f] returns a new generator out of a generator and a
    function taking a value produce by this generator and return a new
    generator. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f gen] is an alias for [bind gen (fun x -> return (f x))]. *)

val root : 'a t -> ('a -> 'b t) -> 'b t
(** [root gen f] is similar to bind, except that the value given to
    [f] is always the root of [gen] (if the sequence has more than one
    element, it is the root of the first tree which is taken).

    As a result, this new generator forgets completely the other
    values of [gen]. It acts as if it contains only the root. *)

val of_seq : 'a Seq.t -> 'a option t
(** [of_seq seq] returns a generator that will produce successively
    the values of the sequence until the sequence is empty. *)

module Syntax : sig
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  (** Alias for {!bind}. *)

  val ( let*! ) : 'a t -> ('a -> 'b t) -> 'b t
  (** Alias for {!root}. *)

  val return : 'a -> 'a t
  (** Alias for {!return}. *)
end

(** {2:shrinking Shrinking} *)

val shrink : ('a -> ('ok, 'err) Result.t) -> 'a Tree.t -> 'a
(** [shrink tree f] returns a value [a] that has the following
    specification assuming [f] is deterministic and [f tree.root] is
    [Error _]:

    - [f a = Error _]

    - [path(a,tree.root) = true]

    - forall v, if [v \in path(a, tree.root)] then [f v = Error _]

    - if [v' = left(v)] and [v \in path(a, tree.root)] then [f v = Ok _]

    where:
    - [path(a,b)] returns [true] if there is a path in the tree from
    node [a] to node [b].

    - [left a] is the node [b] such that [a] and [b] have the same
    parent, and [a] is the successor of [b] when iterating children.
 *)

val crunch : int -> 'a t -> 'a t
(** [crunch i gen] returns a generator with the same root but a more
    aggressive shrinking strategy. It crunchs the first [i] levels of
    the tree into a single level. Hence, to be effective [i] must be
    greater or equal to [2] since [i=0] is the root and [i=1] is the
    first row that can't be crunched. *)

(** {2:merge Advanced: Merging and shrinking} *)

(** The bind function needs to be able to merge nodes between two
    trees. This library allows you to override the default behaviour
    be defining your own merge function. The function {!with_merge}
    aims to be used just before the next bind.

    By default, {!bind} uses the {!Merge.default} strategy. See
    {!Tree.with_merge} for an explanation of what merging is.  *)
module Merge : sig
  type 'a t

  val default : 'a t
  (** [default] is the merging function used by [bind]. While using
      [bind gen f], values of [gen] always preceeds the values
      produced by [f a] where [a] is either the value produced by the
      generator [gen] or during the shrinking. *)

  val drop_left : 'a t
  (** [drop_left] is the merging function that drops the values of [gen]. *)

  val drop_right : 'a t
  (** [drop_left] is the merging function that drops the values
      produced by [f a]. *)

  val of_compare : compare:('a -> 'a -> int) -> 'a t
  (** [drop_left] is the merging function that, with a [bind], merges
      values of [gen] or [f a] according to [compare]. *)
end

val with_merge : 'a Merge.t -> 'a t -> 'a t
(** [with_merge merge gen] sets a new merging strategy for the
    generator. *)

(** {2:advanced Advanced: Sequence of generators} *)

(** The function {!sequence} takes advantage of the fact that a
    generator can be actually a sequence of trees also known as
    "forest". {!bind} will always try to make a single tree out of two
    trees, however some shrinking strategies require to keep the two
    separetely. This is the case of the [Skip] strategy for [list]
    allowing to skip some elements of the list. While for the initial
    value, we expect a single tree, for example [1;3;5;7], during the
    shrinking we may want to explore the tree associated to sublists
    such as [1;3] or [3;5]. This is cannot be achieved easily with
    {!bind}.

    In that case, during the shrinking process, we need to generate
    multiple trees corresponding to different sublists. This is the
    purpose of the {!sequence} function.
 *)

val sequence : 'a t -> 'a t Seq.t -> 'a t
(** [sequence gen seq] returns a new generator out of a non-empty
    sequence of generators [gen::seq]. This function aims to be used
    to define better shrinking strategies. This means that we should
    never have: [bind gen (fun x -> ... (sequence gen seq))] such that
    [seq] is not empty when [x] is the originate value of the
    generator [gen]. It should only happen when [x] is a smaller value
    generated via the shrinking. *)
