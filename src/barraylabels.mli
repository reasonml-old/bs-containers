
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Array utils} *)


(*-- Start stdlib arrayLabels, from https://github.com/ocaml/ocaml/blob/4.02.3/stdlib/arrayLabels.mli --*)

external length : 'a array -> int = "%array_length"
(** Return the length (number of elements) of the given array. *)

external get : 'a array -> int -> 'a = "%array_safe_get"
(** [ArrayLabels.get a n] returns the element number [n] of array [a].
    The first element has number 0.
    The last element has number [ArrayLabels.length a - 1].
    You can also write [a.(n)] instead of [ArrayLabels.get a n].
    Raise [Invalid_argument "index out of bounds"]
    if [n] is outside the range 0 to [(ArrayLabels.length a - 1)]. *)

external set : 'a array -> int -> 'a -> unit = "%array_safe_set"
(** [ArrayLabels.set a n x] modifies array [a] in place, replacing
    element number [n] with [x].
    You can also write [a.(n) <- x] instead of [ArrayLabels.set a n x].
    Raise [Invalid_argument "index out of bounds"]
    if [n] is outside the range 0 to [ArrayLabels.length a - 1]. *)

external make : int -> 'a -> 'a array = "caml_make_vect"
(** [ArrayLabels.make n x] returns a fresh array of length [n],
    initialized with [x].
    All the elements of this new array are initially
    physically equal to [x] (in the sense of the [==] predicate).
    Consequently, if [x] is mutable, it is shared among all elements
    of the array, and modifying [x] through one of the array entries
    will modify all other entries at the same time.
    Raise [Invalid_argument] if [n < 0] or [n > Sys.max_array_length].
    If the value of [x] is a floating-point number, then the maximum
    size is only [Sys.max_array_length / 2].*)

val init : int -> f:(int -> 'a) -> 'a array
(** [ArrayLabels.init n f] returns a fresh array of length [n],
    with element number [i] initialized to the result of [f i].
    In other terms, [ArrayLabels.init n f] tabulates the results of [f]
    applied to the integers [0] to [n-1].
    Raise [Invalid_argument] if [n < 0] or [n > Sys.max_array_length].
    If the return type of [f] is [float], then the maximum
    size is only [Sys.max_array_length / 2].*)

val make_matrix : dimx:int -> dimy:int -> 'a -> 'a array array
(** [ArrayLabels.make_matrix dimx dimy e] returns a two-dimensional array
    (an array of arrays) with first dimension [dimx] and
    second dimension [dimy]. All the elements of this new matrix
    are initially physically equal to [e].
    The element ([x,y]) of a matrix [m] is accessed
    with the notation [m.(x).(y)].
    Raise [Invalid_argument] if [dimx] or [dimy] is negative or
    greater than [Sys.max_array_length].
    If the value of [e] is a floating-point number, then the maximum
    size is only [Sys.max_array_length / 2]. *)

val append : 'a array -> 'a array -> 'a array
(** [ArrayLabels.append v1 v2] returns a fresh array containing the
    concatenation of the arrays [v1] and [v2]. *)

val concat : 'a array list -> 'a array
(** Same as [ArrayLabels.append], but concatenates a list of arrays. *)

val sub : 'a array -> pos:int -> len:int -> 'a array
(** [ArrayLabels.sub a start len] returns a fresh array of length [len],
    containing the elements number [start] to [start + len - 1]
    of array [a].
    Raise [Invalid_argument "Array.sub"] if [start] and [len] do not
    designate a valid subarray of [a]; that is, if
    [start < 0], or [len < 0], or [start + len > ArrayLabels.length a]. *)

val copy : 'a array -> 'a array
(** [ArrayLabels.copy a] returns a copy of [a], that is, a fresh array
    containing the same elements as [a]. *)

val fill : 'a array -> pos:int -> len:int -> 'a -> unit
(** [ArrayLabels.fill a ofs len x] modifies the array [a] in place,
    storing [x] in elements number [ofs] to [ofs + len - 1].
    Raise [Invalid_argument "Array.fill"] if [ofs] and [len] do not
    designate a valid subarray of [a]. *)

val blit :
  src:'a array -> src_pos:int -> dst:'a array -> dst_pos:int -> len:int ->
  unit
(** [ArrayLabels.blit v1 o1 v2 o2 len] copies [len] elements
    from array [v1], starting at element number [o1], to array [v2],
    starting at element number [o2]. It works correctly even if
    [v1] and [v2] are the same array, and the source and
    destination chunks overlap.
    Raise [Invalid_argument "Array.blit"] if [o1] and [len] do not
    designate a valid subarray of [v1], or if [o2] and [len] do not
    designate a valid subarray of [v2]. *)

val to_list : 'a array -> 'a list
(** [ArrayLabels.to_list a] returns the list of all the elements of [a]. *)

val of_list : 'a list -> 'a array
(** [ArrayLabels.of_list l] returns a fresh array containing the elements
    of [l]. *)

val iter : f:('a -> unit) -> 'a array -> unit
(** [ArrayLabels.iter f a] applies function [f] in turn to all
    the elements of [a].  It is equivalent to
    [f a.(0); f a.(1); ...; f a.(ArrayLabels.length a - 1); ()]. *)

val map : f:('a -> 'b) -> 'a array -> 'b array
(** [ArrayLabels.map f a] applies function [f] to all the elements of [a],
    and builds an array with the results returned by [f]:
    [[| f a.(0); f a.(1); ...; f a.(ArrayLabels.length a - 1) |]]. *)

val iteri : f:(int -> 'a -> unit) -> 'a array -> unit
(** Same as {!ArrayLabels.iter}, but the
    function is applied to the index of the element as first argument,
    and the element itself as second argument. *)

val mapi : f:(int -> 'a -> 'b) -> 'a array -> 'b array
(** Same as {!ArrayLabels.map}, but the
    function is applied to the index of the element as first argument,
    and the element itself as second argument. *)

val fold_left : f:('a -> 'b -> 'a) -> init:'a -> 'b array -> 'a
(** [ArrayLabels.fold_left f x a] computes
    [f (... (f (f x a.(0)) a.(1)) ...) a.(n-1)],
    where [n] is the length of the array [a]. *)

val fold_right : f:('b -> 'a -> 'a) -> 'b array -> init:'a -> 'a
(** [ArrayLabels.fold_right f a x] computes
    [f a.(0) (f a.(1) ( ... (f a.(n-1) x) ...))],
    where [n] is the length of the array [a]. *)


(** {6 Sorting} *)


val sort : cmp:('a -> 'a -> int) -> 'a array -> unit
(** Sort an array in increasing order according to a comparison
    function.  The comparison function must return 0 if its arguments
    compare as equal, a positive integer if the first is greater,
    and a negative integer if the first is smaller (see below for a
    complete specification).  For example, {!Pervasives.compare} is
    a suitable comparison function, provided there are no floating-point
    NaN values in the data.  After calling [ArrayLabels.sort], the
    array is sorted in place in increasing order.
    [ArrayLabels.sort] is guaranteed to run in constant heap space
    and (at most) logarithmic stack space.
    The current implementation uses Heap Sort.  It runs in constant
    stack space.
    Specification of the comparison function:
    Let [a] be the array and [cmp] the comparison function.  The following
    must be true for all x, y, z in a :
    -   [cmp x y] > 0 if and only if [cmp y x] < 0
    -   if [cmp x y] >= 0 and [cmp y z] >= 0 then [cmp x z] >= 0
    When [ArrayLabels.sort] returns, [a] contains the same elements as before,
    reordered in such a way that for all i and j valid indices of [a] :
    -   [cmp a.(i) a.(j)] >= 0 if and only if i >= j
*)

val stable_sort : cmp:('a -> 'a -> int) -> 'a array -> unit
(** Same as {!ArrayLabels.sort}, but the sorting algorithm is stable (i.e.
    elements that compare equal are kept in their original order) and
    not guaranteed to run in constant heap space.
    The current implementation uses Merge Sort. It uses [n/2]
    words of heap space, where [n] is the length of the array.
    It is usually faster than the current implementation of {!ArrayLabels.sort}.
*)

val fast_sort : cmp:('a -> 'a -> int) -> 'a array -> unit
(** Same as {!ArrayLabels.sort} or {!ArrayLabels.stable_sort}, whichever is
    faster on typical input.
*)


(**/**)

(** {6 Undocumented functions} *)

(* The following is for system use only. Do not call directly. *)

external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"

(*-- End stdlib arrayLabels --*)


type 'a sequence = ('a -> unit) -> unit
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a gen = unit -> 'a option
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a random_gen = Random.State.t -> 'a
type 'a printer = Format.formatter -> 'a -> unit

(** {2 Arrays} *)

type 'a t = 'a array

val empty : 'a t

val equal : 'a equal -> 'a t equal

val compare : 'a ord -> 'a t ord

val get : 'a t -> int -> 'a

val get_safe : 'a t -> int -> 'a option
(** [get_safe a i] returns [Some a.(i)] if [i] is a valid index
    @since 0.18 *)

val set : 'a t -> int -> 'a -> unit

val length : _ t -> int

val fold : f:('a -> 'b -> 'a) -> init:'a -> 'b t -> 'a

val foldi : f:('a -> int -> 'b -> 'a) -> init:'a -> 'b t -> 'a
(** Fold left on array, with index *)

val fold_while : f:('a -> 'b -> 'a * [`Stop | `Continue]) -> init:'a -> 'b t -> 'a
(** Fold left on array until a stop condition via [('a, `Stop)] is
    indicated by the accumulator
    @since 0.8 *)

val iter : f:('a -> unit) -> 'a t -> unit

val iteri : f:(int -> 'a -> unit) -> 'a t -> unit

val blit : 'a t -> int -> 'a t -> int -> int -> unit
(** [blit from i into j len] copies [len] elements from the first array
    to the second. See {!Array.blit}. *)

val reverse_in_place : 'a t -> unit
(** Reverse the array in place *)

val sorted : cmp:('a -> 'a -> int) -> 'a t -> 'a array
(** [sorted cmp a] makes a copy of [a] and sorts it with [cmp].
    @since 1.0 *)

val sort_indices : cmp:('a -> 'a -> int) -> 'a t -> int array
(** [sort_indices cmp a] returns a new array [b], with the same length as [a],
    such that [b.(i)] is the index of the [i]-th element of [a] in [sort cmp a].
    In other words, [map (fun i -> a.(i)) (sort_indices a) = sorted cmp a].
    [a] is not modified.
    @since 1.0 *)

val sort_ranking : cmp:('a -> 'a -> int) -> 'a t -> int array
(** [sort_ranking cmp a] returns a new array [b], with the same length as [a],
    such that [b.(i)] is the position in [sorted cmp a] of the [i]-th
    element of [a].
    [a] is not modified.

    In other words, [map (fun i -> (sorted cmp a).(i)) (sort_ranking cmp a) = a].

    Without duplicates, we also have
    [lookup_exn a.(i) (sorted a) = (sorted_ranking a).(i)]
    @since 1.0 *)

val find : f:('a -> 'b option) -> 'a t -> 'b option
(** [find f a] returns [Some y] if there is an element [x] such
    that [f x = Some y], else it returns [None] *)

val findi : f:(int -> 'a -> 'b option) -> 'a t -> 'b option
(** Like {!find}, but also pass the index to the predicate function.
    @since 0.3.4 *)

val find_idx : f:('a -> bool) -> 'a t -> (int * 'a) option
(** [find_idx p x] returns [Some (i,x)] where [x] is the [i]-th element of [l],
    and [p x] holds. Otherwise returns [None]
    @since 0.3.4 *)

val lookup : ?cmp:'a ord -> key:'a -> 'a t -> int option
(** Lookup the index of some value in a sorted array.
    @return [None] if the key is not present, or
      [Some i] ([i] the index of the key) otherwise *)

val lookup_exn : ?cmp:'a ord -> key:'a -> 'a t -> int
(** Same as {!lookup_exn}, but
    @raise Not_found if the key is not present *)

val bsearch : ?cmp:('a -> 'a -> int) -> key:'a -> 'a t ->
  [ `All_lower | `All_bigger | `Just_after of int | `Empty | `At of int ]
(** [bsearch ?cmp key arr] finds the index of the object [key] in the array [arr],
    provided [arr] is {b sorted} using [cmp]. If the array is not sorted,
    the result is not specified (may raise Invalid_argument).

    Complexity: O(log n) where n is the length of the array
    (dichotomic search).

    @return
    - [`At i] if [cmp arr.(i) key = 0] (for some i)
    - [`All_lower] if all elements of [arr] are lower than [key]
    - [`All_bigger] if all elements of [arr] are bigger than [key]
    - [`Just_after i] if [arr.(i) < key < arr.(i+1)]
    - [`Empty] if the array is empty

    @raise Invalid_argument if the array is found to be unsorted w.r.t [cmp]
    @since 0.13 *)

val for_all : f:('a -> bool) -> 'a t -> bool

val for_all2 : f:('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** Forall on pairs of arrays.
    @raise Invalid_argument if they have distinct lengths
    allow different types @since 0.20 *)

val exists : f:('a -> bool) -> 'a t -> bool

val exists2 : f:('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** Exists on pairs of arrays.
    @raise Invalid_argument if they have distinct lengths
    allow different types @since 0.20 *)

val fold2 : f:('acc -> 'a -> 'b -> 'acc) -> init:'acc -> 'a t -> 'b t -> 'acc
(** Fold on two arrays stepwise.
    @raise Invalid_argument if they have distinct lengths
    @since 0.20 *)

val iter2 : f:('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** Iterate on two arrays stepwise.
    @raise Invalid_argument if they have distinct lengths
    @since 0.20 *)

val shuffle : 'a t -> unit
(** Shuffle randomly the array, in place *)

val shuffle_with : Random.State.t -> 'a t -> unit
(** Like shuffle but using a specialized random state *)

val random_choose : 'a t -> 'a random_gen
(** Choose an element randomly.
    @raise Not_found if the array/slice is empty *)

val to_seq : 'a t -> 'a sequence
val to_gen : 'a t -> 'a gen
val to_klist : 'a t -> 'a klist

(** {2 IO} *)

val pp: ?sep:string -> 'a printer -> 'a t printer
(** Print an array of items with printing function *)

val pp_i: ?sep:string -> (int -> 'a printer) -> 'a t printer
(** Print an array, giving the printing function both index and item *)

val map : f:('a -> 'b) -> 'a t -> 'b t

val map2 : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Map on two arrays stepwise.
      @raise Invalid_argument if they have distinct lengths
      @since 0.20 *)

val rev : 'a t -> 'a t
(** Copy + reverse in place
    @since 0.20 *)

val filter : f:('a -> bool) -> 'a t -> 'a t
(** Filter elements out of the array. Only the elements satisfying
    the given predicate will be kept. *)

val filter_map : f:('a -> 'b option) -> 'a t -> 'b t
(** Map each element into another value, or discard it *)

val flat_map : f:('a -> 'b t) -> 'a t -> 'b array
(** Transform each element into an array, then flatten *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
(** Infix version of {!flat_map} *)

val (>>|) : 'a t -> ('a -> 'b) -> 'b t
(** Infix version of {!map}
    @since 0.8 *)

val (>|=) : 'a t -> ('a -> 'b) -> 'b t
(** Infix version of {!map}
    @since 0.8 *)

val except_idx : 'a t -> int -> 'a list
(** Remove given index, obtaining the list of the other elements *)

val (--) : int -> int -> int t
(** Range array *)

val (--^) : int -> int -> int t
(** Range array, excluding right bound
    @since 0.17 *)

val random : 'a random_gen -> 'a t random_gen
val random_non_empty : 'a random_gen -> 'a t random_gen
val random_len : int -> 'a random_gen -> 'a t random_gen

(** {2 Generic Functions} *)

module type MONO_ARRAY = sig
  type elt
  type t

  val length : t -> int

  val get : t -> int -> elt

  val set : t -> int -> elt -> unit
end

val sort_generic :
  (module MONO_ARRAY with type t = 'arr and type elt = 'elt) ->
  ?cmp:('elt -> 'elt -> int) -> 'arr -> unit
(** Sort the array, without allocating (eats stack space though). Performance
    might be lower than {!Array.sort}.
    @since 0.14 *)
