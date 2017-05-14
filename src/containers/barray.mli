
(* This file is free software, part of containers. See file "license" for more details. *)

type 'a t = 'a array

val empty : 'a t

val isEmpty: 'a t -> bool

external make : int -> 'a -> 'a t = "caml_make_vect"
(** [Barray.make n x] returns a fresh t of length [n],
    initialized with [x].
    All the elements of this new t are initially
    physically equal to [x] (in the sense of the [==] predicate).
    Consequently, if [x] is mutable, it is shared among all elements
    of the t, and modifying [x] through one of the t entries
    will modify all other entries at the same time.
    Raise [Invalid_argument] if [n < 0] or [n > Sys.max_array_length].
    If the value of [x] is a floating-point number, then the maximum
    size is only [Sys.max_array_length / 2].*)

val makeWithInit : int -> (int -> 'a) -> 'a t
(** [Barray.init n f] returns a fresh t of length [n],
    with element number [i] initialized to the result of [f i].
    In other terms, [Array.init n f] tabulates the results of [f]
    applied to the integers [0] to [n-1].
    Raise [Invalid_argument] if [n < 0] or [n > Sys.max_array_length].
    If the return type of [f] is [float], then the maximum
    size is only [Sys.max_array_length / 2].*)

val length: 'a t -> int

val get : int -> 'a t -> 'a option

val set : int -> 'a -> 'a t -> bool

val getOrRaise : int -> 'a t -> 'a

val setOrRaise : int -> 'a -> 'a t -> unit

val unsafeGet : int -> 'a t -> 'a

val unsafeSet : int -> 'a -> 'a t -> unit

val equals : 'a Equality.t -> 'a t -> 'a t -> bool

val compare : ('a -> 'b -> Ordering.t) -> 'a t -> 'b t -> Ordering.t

val append : 'a t -> 'a t -> 'a t
(** [Barray.append v1 v2] returns a fresh t containing the
    concatenation of the arrays [v1] and [v2]. *)

val concat : 'a t list -> 'a t
(** Same as [Barray.append], but concatenates a list of arrays. *)

val slice : int -> int -> 'a t -> 'a t option
(** [Barray.slice start end a] returns a fresh t of length [len],
    containing the elements number [start] to [end - 1]
    of t [a].
    Return None if [start] and [end] do not
    designate a valid subarray of [a]; that is, if
    [start < 0], or [end < 0], or [end > Array.length a]. *)

val copy : 'a t -> 'a t
(** [Barray.copy a] returns a copy of [a], that is, a fresh t
    containing the same elements as [a]. *)

val toList : 'a t -> 'a list
(** [Array.toList a] returns the list of all the elements of [a]. *)

val fromList : 'a list -> 'a t
(** [Array.fromList l] returns a fresh t containing the elements
    of [l]. *)

val toSequence: 'a t -> 'a Bsequence.t

val fromSequence: 'a Bsequence.t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val mapWithIndex : (int -> 'a -> 'b) -> 'a t -> 'b t
(** Same as {!Array.map}, but the
    function is applied to the index of the element as first argument,
    and the element itself as second argument. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t option
(** Map on two arrays stepwise.
      @raise Invalid_argument if they have distinct lengths *)

val reduce : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [Barray.reduce f x a] computes
    [f (... (f (f x a.(0)) a.(1)) ...) a.(n-1)],
    where [n] is the length of the t [a]. *)

val reduceReversed : ('b -> 'a -> 'a) -> 'a -> 'b t -> 'a
(** [Baray.reduceReversed f a x] computes
    [f a.(0) (f a.(1) ( ... (f a.(n-1) x) ...))],
    where [n] is the length of the t [a]. *)

val reduceWithIndex : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** Fold left on t, with index *)

val reduceWhile : ('a -> 'b -> 'a * bool) -> 'a -> 'b t -> 'a
(** Fold left on t until a stop condition is
    indicated by the accumulator *)

val blit : 'a t -> int -> 'a t -> int -> int -> unit
(** [Barray.blit from i into j len] copies [len] elements from the first t
    to the second. See {!Array.blit}. *)

val reverse : 'a t -> 'a t
(** Copy + reverse in place *)

val reverseInPlace : 'a t -> unit
(** Reverse the t in place *)

val sort : ('a Comparator.t) -> 'a t -> unit
(** Sort an t in increasing order according to a comparison
    function. After calling [Barray.sort], the
    t is sorted in place in increasing order.
    [Barray.sort] is guaranteed to run in constant heap space
    and (at most) logarithmic stack space.
    The current implementation uses Heap Sort.  It runs in constant
    stack space.
    When [Barray.sort] returns, [a] contains the same elements as before,
    reordered in such a way that for all i and j valid indices of [a] :
    -   [cmp a.(i) a.(j)] == Ordering.Greater if and only if i >= j
*)

val stableSort : ('a -> 'a -> int) -> 'a t-> unit
(** Same as {!Barray.sort}, but the sorting algorithm is stable (i.e.
    elements that compare equal are kept in their original order) and
    not guaranteed to run in constant heap space.
    The current implementation uses Merge Sort. It uses [n/2]
    words of heap space, where [n] is the length of the t.
    It is usually faster than the current implementation of {!Barray.sort}.
*)

val fastSort : ('a -> 'a -> int) -> 'a t -> unit
(** Same as {!Barray.sort} or {!Barray.stableSort}, whichever is faster
    on typical input.
*)

val sorted : ('a Comparator.t) -> 'a t -> 'a t
(** [sorted cmp a] makes a copy of [a] and sorts it with [cmp]. *)

val sortIndices : ('a Comparator.t) -> 'a t -> int t
(** [sortIndices cmp a] returns a new t [b], with the same length as [a],
    such that [b.(i)] is the index of the [i]-th element of [a] in [sort cmp a].
    In other words, [map (fun i -> a.(i)) (sortIndices a) = sorted cmp a].
    [a] is not modified. *)

val sortRanking : ('a Comparator.t) -> 'a t -> int t
(** [sortRanking cmp a] returns a new t [b], with the same length as [a],
    such that [b.(i)] is the position in [sorted cmp a] of the [i]-th
    element of [a].
    [a] is not modified.

    In other words, [map (fun i -> (sorted cmp a).(i)) (sortRanking cmp a) = a].
*)

val find : ('a -> 'b option) -> 'a t -> 'b option

val findWithIndex : (int -> 'a -> 'b option) -> 'a t -> 'b option
(** Like {!find}, but also pass the index to the predicate function. *)

val findIndex : ('a -> bool) -> 'a t -> (int * 'a) option
(** [Barray.findIndex p x] returns [Some (i,x)] where [x] is the [i]-th element of [l],
    and [p x] holds. Otherwise returns [None] *)

val bsearch : 'a Comparator.t -> 'a -> 'a t ->
  [ `All_lower | `All_bigger | `Just_after of int | `Empty | `At of int ]
(** [Barray.bsearch cmp x arr] finds the index of the object [x] in the t [arr],
    provided [arr] is {b sorted} using [cmp]. If the t is not sorted,
    the result is not specified (may raise Invalid_argument).

    Complexity: O(log n) where n is the length of the t
    (dichotomic search).

    @return
    - [`At i] if [cmp arr.(i) x = 0] (for some i)
    - [`All_lower] if all elements of [arr] are lower than [x]
    - [`All_bigger] if all elements of [arr] are bigger than [x]
    - [`Just_after i] if [arr.(i) < x < arr.(i+1)]
    - [`Empty] if the t is empty *)

val forAll: ('a -> bool) -> 'a t -> bool

val exists: ('a -> bool) -> 'a t -> bool

val count: ('a -> bool) -> 'a t -> int

val forEach: ('a -> unit) -> 'a t -> unit

val forEachWithIndex : ('a -> int -> unit) -> 'a t -> unit

val shuffle : 'a t -> unit
(** Shuffle randomly the t, in place *)

val shuffleWith : Random.State.t -> 'a t -> unit
(** Like shuffle but using a specialized random state *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** Filter elements out of the t. Only the elements satisfying
    the given predicate will be kept. *)

val filterMap : ('a -> 'b option) -> 'a t -> 'b t
(** Map each element into another value, or discard it *)

val flatMap : ('a -> 'b t) -> 'a t -> 'b t
(** Transform each element into an t, then flatten *)

module Infix : sig
  val (--) : int -> int -> int t
  (** Range t *)

  val (--^) : int -> int -> int t
  (** Range t, excluding right bound *)
end