
(* This file is free software, part of containers. See file "license" for more details. *)

type 'a t = 'a array

val empty : 'a t

val isEmpty: 'a t -> bool

external make : int -> 'a -> 'a array = "caml_make_vect"
(** [Array.make n x] returns a fresh array of length [n],
    initialized with [x].
    All the elements of this new array are initially
    physically equal to [x] (in the sense of the [==] predicate).
    Consequently, if [x] is mutable, it is shared among all elements
    of the array, and modifying [x] through one of the array entries
    will modify all other entries at the same time.
    Raise [Invalid_argument] if [n < 0] or [n > Sys.max_array_length].
    If the value of [x] is a floating-point number, then the maximum
    size is only [Sys.max_array_length / 2].*)

val init : int -> (int -> 'a) -> 'a array
(** [Array.init n f] returns a fresh array of length [n],
    with element number [i] initialized to the result of [f i].
    In other terms, [Array.init n f] tabulates the results of [f]
    applied to the integers [0] to [n-1].
    Raise [Invalid_argument] if [n < 0] or [n > Sys.max_array_length].
    If the return type of [f] is [float], then the maximum
    size is only [Sys.max_array_length / 2].*)

val makeMatrix : int -> int -> 'a -> 'a array array
(** [Array.make_matrix dimx dimy e] returns a two-dimensional array
    (an array of arrays) with first dimension [dimx] and
    second dimension [dimy]. All the elements of this new matrix
    are initially physically equal to [e].
    The element ([x,y]) of a matrix [m] is accessed
    with the notation [m.(x).(y)].
    Raise [Invalid_argument] if [dimx] or [dimy] is negative or
    greater than [Sys.max_array_length].
    If the value of [e] is a floating-point number, then the maximum
    size is only [Sys.max_array_length / 2]. *)

external makeFloat: int -> float array = "caml_make_float_vect"
(** [Array.makeFloat n] returns a fresh float array of length [n],
    with uninitialized data. *)

val length: 'a t -> int

val get : int -> 'a t -> 'a option

val set : int -> 'a t -> 'a -> unit

val getOrRaise : 'a t -> int -> 'a

val setOrRaise : 'a t -> int -> 'a -> unit

val equals : 'a Equality.t -> 'a t -> 'a t -> bool

val compare : ('a -> 'b -> Ordering.t) -> 'a t -> 'b t -> Ordering.t

val append : 'a array -> 'a array -> 'a array
(** [Array.append v1 v2] returns a fresh array containing the
    concatenation of the arrays [v1] and [v2]. *)

val concat : 'a array list -> 'a array
(** Same as [Array.append], but concatenates a list of arrays. *)

val slice : ?start:int -> ?end_:int -> 'a array -> 'a array option
(** [Array.slice a start end] returns a fresh array of length [len],
    containing the elements number [start] to [end - 1]
    of array [a].
    Return None if [start] and [end] do not
    designate a valid subarray of [a]; that is, if
    [start < 0], or [end < 0], or [end > Array.length a]. *)

val copy : 'a array -> 'a array
(** [Array.copy a] returns a copy of [a], that is, a fresh array
    containing the same elements as [a]. *)

val toList : 'a array -> 'a list
(** [Array.to_list a] returns the list of all the elements of [a]. *)

val fromList : 'a list -> 'a array
(** [Array.of_list l] returns a fresh array containing the elements
    of [l]. *)

val toSequence: 'a t -> 'a Sequence.t

val fromSequence: 'a Sequence.t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val mapi : (int -> 'a -> 'b) -> 'a array -> 'b array
(** Same as {!Array.map}, but the
    function is applied to the index of the element as first argument,
    and the element itself as second argument. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Map on two arrays stepwise.
      @raise Invalid_argument if they have distinct lengths *)
val reduce : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
(** [Array.reduce f x a] computes
    [f (... (f (f x a.(0)) a.(1)) ...) a.(n-1)],
    where [n] is the length of the array [a]. *)

val reduceReversed : ('b -> 'a -> 'a) -> 'b array -> 'a -> 'a
(** [Array.reduceReversed f a x] computes
    [f a.(0) (f a.(1) ( ... (f a.(n-1) x) ...))],
    where [n] is the length of the array [a]. *)

val reduceWithIndex : f:('a -> int -> 'b -> 'a) -> acc:'a -> 'b t -> 'a
(** Fold left on array, with index *)

val reduceWhile : ('a -> 'b -> 'a * bool) -> 'a -> 'b t -> 'a
(** Fold left on array until a stop condition via [('a, `Stop)] is
    indicated by the accumulator *)

val forEachWithIndex : (int -> 'a -> unit) -> 'a t -> unit

val blit : 'a t -> int -> 'a t -> int -> int -> unit
(** [blit from i into j len] copies [len] elements from the first array
    to the second. See {!Array.blit}. *)

val reverse : 'a t -> 'a t
(** Copy + reverse in place *)

val reverseInPlace : 'a t -> unit
(** Reverse the array in place *)

val sort : ('a Comparator.t) -> 'a array -> unit
(** Sort an array in increasing order according to a comparison
    function.  The comparison function must return 0 if its arguments
    compare as equal, a positive integer if the first is greater,
    and a negative integer if the first is smaller (see below for a
    complete specification).  For example, {!Pervasives.compare} is
    a suitable comparison function, provided there are no floating-point
    NaN values in the data.  After calling [Array.sort], the
    array is sorted in place in increasing order.
    [Array.sort] is guaranteed to run in constant heap space
    and (at most) logarithmic stack space.
    The current implementation uses Heap Sort.  It runs in constant
    stack space.
    Specification of the comparison function:
    Let [a] be the array and [cmp] the comparison function.  The following
    must be true for all x, y, z in a :
    -   [cmp x y] > 0 if and only if [cmp y x] < 0
    -   if [cmp x y] >= 0 and [cmp y z] >= 0 then [cmp x z] >= 0
    When [Array.sort] returns, [a] contains the same elements as before,
    reordered in such a way that for all i and j valid indices of [a] :
    -   [cmp a.(i) a.(j)] >= 0 if and only if i >= j
*)

val stableSort : ('a -> 'a -> int) -> 'a t-> unit
(** Same as {!Array.sort}, but the sorting algorithm is stable (i.e.
    elements that compare equal are kept in their original order) and
    not guaranteed to run in constant heap space.
    The current implementation uses Merge Sort. It uses [n/2]
    words of heap space, where [n] is the length of the array.
    It is usually faster than the current implementation of {!Array.sort}.
*)

val fastSort : ('a -> 'a -> int) -> 'a t -> unit
(** Same as {!Array.sort} or {!Array.stableSort}, whichever is faster
    on typical input.
*)

val sorted : ('a Comparator.t) -> 'a t -> 'a array
(** [sorted cmp a] makes a copy of [a] and sorts it with [cmp]. *)

val sortIndices : ('a Comparator.t) -> 'a t -> int array
(** [sortIndices cmp a] returns a new array [b], with the same length as [a],
    such that [b.(i)] is the index of the [i]-th element of [a] in [sort cmp a].
    In other words, [map (fun i -> a.(i)) (sortIndices a) = sorted cmp a].
    [a] is not modified. *)

val sortRanking : ('a Comparator.t) -> 'a t -> int array
(** [sortRanking cmp a] returns a new array [b], with the same length as [a],
    such that [b.(i)] is the position in [sorted cmp a] of the [i]-th
    element of [a].
    [a] is not modified.

    In other words, [map (fun i -> (sorted cmp a).(i)) (sortRanking cmp a) = a].

    Without duplicates, we also have
    [lookup_exn a.(i) (sorted a) = (sorted_ranking a).(i)] *)

val find : ('a -> 'b option) -> 'a t -> 'b option

val findWithIndex : (int -> 'a -> 'b option) -> 'a t -> 'b option
(** Like {!find}, but also pass the index to the predicate function. *)

val findIndex : ('a -> bool) -> 'a t -> (int * 'a) option
(** [find_idx p x] returns [Some (i,x)] where [x] is the [i]-th element of [l],
    and [p x] holds. Otherwise returns [None] *)

val bsearch : ?cmp:('a Comparator.t) -> 'a -> 'a t ->
  [ `All_lower | `All_bigger | `Just_after of int | `Empty | `At of int ]
(** [bsearch ?cmp x arr] finds the index of the object [x] in the array [arr],
    provided [arr] is {b sorted} using [cmp]. If the array is not sorted,
    the result is not specified (may raise Invalid_argument).

    Complexity: O(log n) where n is the length of the array
    (dichotomic search).

    @return
    - [`At i] if [cmp arr.(i) x = 0] (for some i)
    - [`All_lower] if all elements of [arr] are lower than [x]
    - [`All_bigger] if all elements of [arr] are bigger than [x]
    - [`Just_after i] if [arr.(i) < x < arr.(i+1)]
    - [`Empty] if the array is empty

    @raise Invalid_argument if the array is found to be unsorted w.r.t [cmp] *)

val forAll: ('a -> bool) -> 'a t -> bool

val forAll2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** Forall on pairs of arrays.
    @raise Invalid_argument if they have distinct lengths
    allow different types *)

val exists: ('a -> bool) -> 'a t -> bool

val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** Exists on pairs of arrays.
    @raise Invalid_argument if they have distinct lengths
    allow different types *)

val count: ('a -> bool) -> 'a t -> int

val iter : ('a -> unit) -> 'a t -> unit

val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** Iterate on two arrays stepwise.
    @raise Invalid_argument if they have distinct lengths *)

val shuffle : 'a t -> unit
(** Shuffle randomly the array, in place *)

val shuffleWith : Random.State.t -> 'a t -> unit
(** Like shuffle but using a specialized random state *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** Filter elements out of the array. Only the elements satisfying
    the given predicate will be kept. *)

val filterMap : ('a -> 'b option) -> 'a t -> 'b t
(** Map each element into another value, or discard it *)

val flatMap : ('a -> 'b t) -> 'a t -> 'b array
(** Transform each element into an array, then flatten *)

val exceptIndex : 'a t -> int -> 'a list
(** Remove given index, obtaining the list of the other elements *)

val (--) : int -> int -> int t
(** Range array *)

val (--^) : int -> int -> int t
(** Range array, excluding right bound *)