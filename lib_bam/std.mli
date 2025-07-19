(** This module contains the various strinking strategies which can be
    used with the generators below. *)
module Shrinker : sig
  type 'a t =
    | Default : 'a t
        (** Default strategy for all the generators defined in this
          module. Refer to the documentation of each generator to know
          what the default strategy is. *)
    | Manual : ('a -> 'a Seq.t) -> 'a t
        (** Instead of relying on a built-in strategy, anyone can define
          its own shrinking strategy. To do so, given a value, the
          user must provide a sequence of "smaller" values. *)
    | Pair_left : ('a * 'b) t
        (** Shrinking strategy for pairs where the first component is
            shrunk first. *)
    | Pair_right : ('a * 'b) t
        (** Shrinking strategy for pairs where the second component is
            shrunk first. *)
    | Pair_compare : ('a * 'b -> 'a * 'b -> Int.t) -> ('a * 'b) t
        (** Shrinking strategy for pairs where the function comparison
          is given to know which pair to handle first. For example
          [Pair_compare (fun _ _ -> -1)] is equivalent to [Pair_left].
          [Pair_compare (fun left right -> compare (fst left + snd
          left) (fst right + snd right))] handles pair of integers by
          minimizing first the sum of their component. *)
    | Char : Char.t -> Char.t t
        (** Shrinking strategy for chars. The shrinker will try to reduce
      samples to the char given. *)
    | Bool : Bool.t -> Bool.t t
        (** Shrinking strategy for bool. The shrinker will try to reduce
      samples to the bool given. *)
    | Float : Float.t -> Float.t t
        (** Shrinking strategy for float. The shrinker will try to reduce
          samples to the float given.

          The strategy split the initial float into a pair of a
          fractional and integral part. Then the strategy behaves as
          [Pair_left] where the first component is the integral part
          and the second component is the fractional part. The
          shrinking behavior for the integral part behaves as the [Int
          n] strategy where [n] is the integral part of the float
          given. The shrinking behavior for the fractional part tries
          to minimize the number of decimals.

          This strategy is syntactic sugar for [Float_precision] with
          [exhaustive_search_digits=0] and [precision_digits=15]. *)
    | Float_precision :
        {exhaustive_search_digits: int; precision_digits: int; target: Float.t}
        -> Float.t t
        (** This shrinking strategy allows to enumerate the float
            numbers that can be represented with
            [exhaustive_search_digits] digits. While
            [precision_digits] allows the search up to
            [precision_digits] digits with respect to the original
            counter-example. [arget] is the same as
            {!constructor:Float}.  *)
    | Int : Int.t -> Int.t t
        (** Shrinking strategy for int. The shrinker will try to reduce
          samples to the int given. The strategy follows a binary
          search starting with the initial sample to the integer
          provided by the strategy. *)
    | Int32 : Int32.t -> Int32.t t  (** See {!constructor:Int} *)
    | Int64 : Int64.t -> Int64.t t  (** See {!constructor:Int} *)
    | Skip :
        {eq: 'a -> 'a -> bool; skip: [`Auto | `Int of Int.t]}
        -> 'a List.t t
        (** Shrinking strategy for lists. The shrinker will try to
          reduce first on the size on the list and then shrink the
          elements of the lists. This strategy will try to shrink to a
          smaller list by skipping some elements.

          Assuming the initial sample is [a;b;c;d], it can first
          shrink towards a list of size [2] such as [a;c] or [b;d],
          while the prefix strategy can always shrink to a
          prefix. Hence for a list of size [2] it must be [a;b] (and
          then the shrinking will shrink on [a] and [b]
          individually).

          With [`Auto], the tool tries to infer a good value so that
          the shrinking strategy remains short. With [`Int n], the
          user provides the number of elements that can be
          skipped. This can be quite time consuming since such a
          strategy is at least exponential with respect to the initial
          size of the list. *)
    | Prefix : 'a List.t t
        (** Shrinking strategy for lists. The shrinker will try to
          reduce first on the size on the list and then shrink the
          elements of the lists. The strategy will shrink towards a
          prefix of the list. *)
    | Suffix : 'a List.t t
        (** Shrinking strategy for lists. The shrinker will try to
          reduce first on the size on the list and then shrink the
          elements of the lists. The strategy will shrink towards a
          suffix of the list. *)

  val skip_auto : 'a list t
  (** [skip_auto] is an alias for [Skip {eq=(=); skip=`Auto}]*)
end

(** The type of random generators. *)
type 'a t = 'a Gen.t

val return : 'a -> 'a t
(** [return v] is a generator for a constant value [v]. *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [bind gen f] returns a new generator depending on another
    generator. This should be the main building block to construct new
    generator. To understand how shrinking works with this operator
    please read {!page-shrinking}. *)

val root : 'a t -> ('a -> 'b t) -> 'b t
(** [root gen f] applies [f] on the original value generated by [gen]
    and forgets about the shrinking. Hence, [f] is always applied to a
    single value. More details in {!page-shrinking}. *)

val crunch : int -> 'a t -> 'a t
(** [crunch i gen] returns a new generator with a more aggressive
    shrinking. It increases the number of values that will be used
    during the shrinking. More details in {!page-shrinking}. *)

val of_seq : 'a Seq.t -> 'a option t
(** [of_seq seq] returns a generator that will produce successively
    the values of the sequence until the sequence is empty. Those
    values are intended to be used as the root argument of other generators. *)

val int :
  ?root:int -> ?shrinker:int Shrinker.t -> ?min:int -> ?max:int -> unit -> int t
(** [int ?root ?shrinker ?(min=0) ?(max=Int.max_int) ()] is a generator for
    integers. Bounds are inclusive.

    Default strategy is {!constructor:Shrinker.Int}[0].
 *)

val int32 :
     ?root:int32
  -> ?shrinker:int32 Shrinker.t
  -> ?min:int32
  -> ?max:int32
  -> unit
  -> int32 t
(** [int ?root ?shrinker ?(min=0) ?(max=Int.max_int) ()] is a generator for
    integers. Bounds are inclusive.

    Default strategy is {!constructor:Shrinker.Int}[0].
 *)

val int64 :
     ?root:int64
  -> ?shrinker:int64 Shrinker.t
  -> ?min:int64
  -> ?max:int64
  -> unit
  -> int64 t
(** [int ?root ?shrinker ?(min=0) ?(max=Int.max_int) ()] is a generator for
    integers. Bounds are inclusive.

    Default strategy is {!constructor:Shrinker.Int}[0].
 *)

val float :
     ?root:float
  -> ?shrinker:float Shrinker.t
  -> ?min:float
  -> ?max:float
  -> unit
  -> float t
(** [float ?root ?shrinker ?(min=0.) ?(max=Float.max_float) ()] generates
    integers. Bounds are inclusive.

    Default strategy is {!constructor:Shrinker.Float}[0.].
 *)

val pair : ?shrinker:('a * 'b) Shrinker.t -> 'a t -> 'b t -> ('a * 'b) t
(** [pair ?shrinker left right ()] generates a pair using the [left]
    and [right] generators.

    Default strategy is {!constructor:Shrinker.Pair_left}.
 *)

val bool : ?shrinker:bool Shrinker.t -> unit -> bool t
(** [bool ?shrinker ()] generates a boolean.x

    Default strategy is {!constructor:Shrinker.Bool}[false].
 *)

val char :
  ?root:char -> ?shrinker:Char.t Shrinker.t -> ?printable:bool -> unit -> char t
(** [char ?root ?shrinker ?(printable=true) ()] generates a char.

    Default strategy is {!constructor:Shrinker.Char}['a].    
 *)

val list : ?shrinker:'a list Shrinker.t -> size:int t -> 'a t -> 'a list t
(** [list ?shrinker ~size gen ()] generates a list of values using [gen].

    Default strategy is {!constructor:Shrinker.Prefix}. *)

val string :
     ?shrinker:Char.t list Shrinker.t
  -> ?char:Char.t t
  -> size:int t
  -> unit
  -> string t
(** [string ?shrinker ?char ~size ()] is mostly an alias for [list
    ?shrinker ~char:(char ())]. *)

val bytes :
     ?shrinker:Char.t list Shrinker.t
  -> ?char:Char.t t
  -> size:int t
  -> unit
  -> bytes t
(** [bytes ?shrinker ?char ~size ()] is mostly an alias for [list
    ?shrinker ~char:(char ~printable:false ()) ()]. *)

val oneof : ?shrinker:int Shrinker.t -> (int * 'a t) list -> 'a t
(** [oneof ?shrinker list] pick one generator of the list according to
    the distribution defined by the first elements of the list.

    Default strategy is the same as for [int]. This generator can be
    thought as a repeated list of generators. The shrinker will choose
    particular index from the list. *)

val oneofg : ?shrinker:int Shrinker.t -> 'a t list -> 'a t
(** [oneofg ?shrinker list] is an alias for
    [oneof ?shrinker (List.map (fun gen -> (1, gen)) list)] *)

val oneofl : ?shrinker:int Shrinker.t -> 'a list -> 'a t
(** [oneofl ?shrinker list] is an alias for
    [oneofg ?shrinker (List.map Gen.return list] *)

val option :
     ?shrinker:int Shrinker.t
  -> ?none_weight:int
  -> ?some_weight:int
  -> 'a t
  -> 'a option t
(** [option ?choice_shrinker ?none_weight ?some_weight gen] builds a generator
    of type ['a option t] that produces

    - [None] with “relative” weight [none_weight]
    - [Some x] with “relative” weight [some_weight], where [x] ← [gen]

    Both [none_weight] and [some_weight] default to [1].  Internally,
    this is just a two‑case [oneof] (index 0 = None, 1 = Some); the
    [choice_shrinker] (default [Shrinker.Int 0]) will therefore always
    try to shrink towards [None] first.
*)

module Syntax : sig
  val return : 'a -> 'a t
  (** Syntactic sugar {!return} *)

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  (** Syntactic sugar for {!bind} *)

  val ( let*! ) : 'a t -> ('a -> 'b t) -> 'b t
  (** Syntactic sugar for {!root} *)
end
