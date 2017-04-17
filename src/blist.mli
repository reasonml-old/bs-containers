
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 complements to list} *)


(*-- Start stdlib list, from https://github.com/ocaml/ocaml/blob/4.02.3/stdlib/list.mli --*)

val length : 'a list -> int
(** Return the length (number of elements) of the given list. *)

val hd : 'a list -> 'a
(** Return the first element of the given list. Raise
    [Failure "hd"] if the list is empty. *)

val tl : 'a list -> 'a list
(** Return the given list without its first element. Raise
    [Failure "tl"] if the list is empty. *)

val nth : 'a list -> int -> 'a
(** Return the [n]-th element of the given list.
    The first element (head of the list) is at position 0.
    Raise [Failure "nth"] if the list is too short.
    Raise [Invalid_argument "List.nth"] if [n] is negative. *)

val rev : 'a list -> 'a list
(** List reversal. *)

val rev_append : 'a list -> 'a list -> 'a list
(** [List.rev_append l1 l2] reverses [l1] and concatenates it to [l2].
    This is equivalent to {!List.rev}[ l1 @ l2], but [rev_append] is
    tail-recursive and more efficient. *)

val concat : 'a list list -> 'a list
(** Concatenate a list of lists.  The elements of the argument are all
    concatenated together (in the same order) to give the result.
    Not tail-recursive
    (length of the argument + length of the longest sub-list). *)


(** {6 Iterators} *)


val iter : ('a -> unit) -> 'a list -> unit
(** [List.iter f [a1; ...; an]] applies function [f] in turn to
    [a1; ...; an]. It is equivalent to
    [begin f a1; f a2; ...; f an; () end]. *)

val rev_map : ('a -> 'b) -> 'a list -> 'b list
(** [List.rev_map f l] gives the same result as
    {!List.rev}[ (]{!List.map}[ f l)], but is tail-recursive and
    more efficient. *)

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
(** [List.fold_left f a [b1; ...; bn]] is
    [f (... (f (f a b1) b2) ...) bn]. *)


(** {6 Iterators on two lists} *)


val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
(** [List.iter2 f [a1; ...; an] [b1; ...; bn]] calls in turn
    [f a1 b1; ...; f an bn].
    Raise [Invalid_argument] if the two lists have
    different lengths. *)

val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
(** [List.map2 f [a1; ...; an] [b1; ...; bn]] is
    [[f a1 b1; ...; f an bn]].
    Raise [Invalid_argument] if the two lists have
    different lengths.  Not tail-recursive. *)

val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
(** [List.rev_map2 f l1 l2] gives the same result as
    {!List.rev}[ (]{!List.map2}[ f l1 l2)], but is tail-recursive and
    more efficient. *)

val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
(** [List.fold_left2 f a [b1; ...; bn] [c1; ...; cn]] is
    [f (... (f (f a b1 c1) b2 c2) ...) bn cn].
    Raise [Invalid_argument] if the two lists have
    different lengths. *)

val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
(** [List.fold_right2 f [a1; ...; an] [b1; ...; bn] c] is
    [f a1 b1 (f a2 b2 (... (f an bn c) ...))].
    Raise [Invalid_argument] if the two lists have
    different lengths.  Not tail-recursive. *)


(** {6 List scanning} *)


val for_all : ('a -> bool) -> 'a list -> bool
(** [for_all p [a1; ...; an]] checks if all elements of the list
    satisfy the predicate [p]. That is, it returns
    [(p a1) && (p a2) && ... && (p an)]. *)

val exists : ('a -> bool) -> 'a list -> bool
(** [exists p [a1; ...; an]] checks if at least one element of
    the list satisfies the predicate [p]. That is, it returns
    [(p a1) || (p a2) || ... || (p an)]. *)

val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
(** Same as {!List.for_all}, but for a two-argument predicate.
    Raise [Invalid_argument] if the two lists have
    different lengths. *)

val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
(** Same as {!List.exists}, but for a two-argument predicate.
    Raise [Invalid_argument] if the two lists have
    different lengths. *)

val memq : 'a -> 'a list -> bool
(** Same as {!List.mem}, but uses physical equality instead of structural
    equality to compare list elements. *)


(** {6 List searching} *)


val find : ('a -> bool) -> 'a list -> 'a
(** [find p l] returns the first element of the list [l]
    that satisfies the predicate [p].
    Raise [Not_found] if there is no value that satisfies [p] in the
    list [l]. *)

val find_all : ('a -> bool) -> 'a list -> 'a list
(** [find_all] is another name for {!List.filter}. *)

val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
(** [partition p l] returns a pair of lists [(l1, l2)], where
    [l1] is the list of all the elements of [l] that
    satisfy the predicate [p], and [l2] is the list of all the
    elements of [l] that do not satisfy [p].
    The order of the elements in the input list is preserved. *)


(** {6 Association lists} *)


val assoc : 'a -> ('a * 'b) list -> 'b
(** [assoc a l] returns the value associated with key [a] in the list of
    pairs [l]. That is,
    [assoc a [ ...; (a,b); ...] = b]
    if [(a,b)] is the leftmost binding of [a] in list [l].
    Raise [Not_found] if there is no value associated with [a] in the
    list [l]. *)

val assq : 'a -> ('a * 'b) list -> 'b
(** Same as {!List.assoc}, but uses physical equality instead of structural
    equality to compare keys. *)

val mem_assoc : 'a -> ('a * 'b) list -> bool
(** Same as {!List.assoc}, but simply return true if a binding exists,
    and false if no bindings exist for the given key. *)

val mem_assq : 'a -> ('a * 'b) list -> bool
(** Same as {!List.mem_assoc}, but uses physical equality instead of
    structural equality to compare keys. *)

val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
(** [remove_assoc a l] returns the list of
    pairs [l] without the first pair with key [a], if any.
    Not tail-recursive. *)

val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
(** Same as {!List.remove_assoc}, but uses physical equality instead
    of structural equality to compare keys.  Not tail-recursive. *)


(** {6 Lists of pairs} *)


val split : ('a * 'b) list -> 'a list * 'b list
(** Transform a list of pairs into a pair of lists:
    [split [(a1,b1); ...; (an,bn)]] is [([a1; ...; an], [b1; ...; bn])].
    Not tail-recursive.
*)

val combine : 'a list -> 'b list -> ('a * 'b) list
(** Transform a pair of lists into a list of pairs:
    [combine [a1; ...; an] [b1; ...; bn]] is
    [[(a1,b1); ...; (an,bn)]].
    Raise [Invalid_argument] if the two lists
    have different lengths.  Not tail-recursive. *)


(** {6 Sorting} *)


val sort : ('a -> 'a -> int) -> 'a list -> 'a list
(** Sort a list in increasing order according to a comparison
    function.  The comparison function must return 0 if its arguments
    compare as equal, a positive integer if the first is greater,
    and a negative integer if the first is smaller (see Array.sort for
    a complete specification).  For example,
    {!Pervasives.compare} is a suitable comparison function.
    The resulting list is sorted in increasing order.
    [List.sort] is guaranteed to run in constant heap space
    (in addition to the size of the result list) and logarithmic
    stack space.
    The current implementation uses Merge Sort. It runs in constant
    heap space and logarithmic stack space.
*)

val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
(** Same as {!List.sort}, but the sorting algorithm is guaranteed to
    be stable (i.e. elements that compare equal are kept in their
    original order) .
    The current implementation uses Merge Sort. It runs in constant
    heap space and logarithmic stack space.
*)

val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
(** Same as {!List.sort} or {!List.stable_sort}, whichever is faster
    on typical input. *)

val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
(** Merge two lists:
    Assuming that [l1] and [l2] are sorted according to the
    comparison function [cmp], [merge cmp l1 l2] will return a
    sorted list containting all the elements of [l1] and [l2].
    If several elements compare equal, the elements of [l1] will be
    before the elements of [l2].
    Not tail-recursive (sum of the lengths of the arguments).
*)

(*-- End stdlib list -*)


type 'a t = 'a list

val empty : 'a t

val is_empty : _ t -> bool
(** [is_empty l] returns [true] iff [l = []]
    @since 0.11 *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Safe version of map *)

val (>|=) : 'a t -> ('a -> 'b) -> 'b t
(** Infix version of [map] with reversed arguments
    @since 0.5 *)

val cons : 'a -> 'a t -> 'a t
(** [cons x l] is [x::l]
    @since 0.12 *)

val append : 'a t -> 'a t -> 'a t
(** Safe version of append *)

val cons_maybe : 'a option -> 'a t -> 'a t
(** [cons_maybe (Some x) l] is [x :: l]
    [cons_maybe None l] is [l]
    @since 0.13 *)

val (@) : 'a t -> 'a t -> 'a t

val filter : ('a -> bool) -> 'a t -> 'a t
(** Safe version of {!List.filter} *)

val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** Safe version of [fold_right] *)

val fold_while : ('a -> 'b -> 'a * [`Stop | `Continue]) -> 'a -> 'b t -> 'a
(** Fold until a stop condition via [('a, `Stop)] is
    indicated by the accumulator
    @since 0.8 *)

val fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a list -> 'acc * 'b list
(** [fold_map f acc l] is a [fold_left]-like function, but it also maps the
    list to another list.
    @since 0.14 *)

val fold_map2 : ('acc -> 'a -> 'b -> 'acc * 'c) -> 'acc -> 'a list -> 'b list -> 'acc * 'c list
(** [fold_map2] is to [fold_map] what [List.map2] is to [List.map].
    @raise Invalid_argument if the lists do not have the same length
    @since 0.16 *)

val fold_filter_map : ('acc -> 'a -> 'acc * 'b option) -> 'acc -> 'a list -> 'acc * 'b list
(** [fold_filter_map f acc l] is a [fold_left]-like function, but also
    generates a list of output in a way similar to {!filter_map}
    @since 0.17 *)

val fold_flat_map : ('acc -> 'a -> 'acc * 'b list) -> 'acc -> 'a list -> 'acc * 'b list
(** [fold_flat_map f acc l] is a [fold_left]-like function, but it also maps the
    list to a list of lists that is then [flatten]'d..
    @since 0.14 *)

val init : int -> (int -> 'a) -> 'a t
(** Similar to {!Array.init}
    @since 0.6 *)

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** Map and flatten at the same time (safe). Evaluation order is not guaranteed. *)

val flatten : 'a t t -> 'a t
(** Safe flatten *)

val product : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Cartesian product of the two lists, with the given combinator *)

val fold_product : ('c -> 'a -> 'b -> 'c) -> 'c -> 'a t -> 'b t -> 'c
(** Fold on the cartesian product *)

val diagonal : 'a t -> ('a * 'a) t
(** All pairs of distinct positions of the list. [list_diagonal l] will
    return the list of [List.nth i l, List.nth j l] if [i < j]. *)

val partition_map : ('a -> [<`Left of 'b | `Right of 'c | `Drop]) ->
  'a list -> 'b list * 'c list
(** [partition_map f l] maps [f] on [l] and gather results in lists:
    - if [f x = `Left y], adds [y] to the first list
    - if [f x = `Right z], adds [z] to the second list
    - if [f x = `Drop], ignores [x]
    @since 0.11 *)

val sublists_of_len :
  ?last:('a list -> 'a list option) ->
  ?offset:int ->
  int ->
  'a list ->
  'a list list
(** [sublists_of_len n l] returns sub-lists of [l] that have length [n].
    By default, these sub-lists are non overlapping:
    [sublists_of_len 2 [1;2;3;4;5;6]] returns [[1;2]; [3;4]; [5;6]]

    Examples:

    - [sublists_of_len 2 [1;2;3;4;5;6] = [[1;2]; [3;4]; [5;6]]]
    - [sublists_of_len 2 ~offset:3 [1;2;3;4;5;6] = [1;2];[4;5]]
    - [sublists_of_len 3 ~last:CCOpt.return [1;2;3;4] = [1;2;3];[4]]
    - [sublists_of_len 2 [1;2;3;4;5] = [[1;2]; [3;4]]]

    @param offset the number of elements skipped between two consecutive
      sub-lists. By default it is [n]. If [offset < n], the sub-lists
      will overlap; if [offset > n], some elements will not appear at all.
    @param last if provided and the last group of elements [g] is such
      that [length g < n], [last g] is called. If [last g = Some g'],
      [g'] is appended; otherwise [g] is dropped.
      If [last = CCOpt.return], it will simply keep the last group.
      By default, [last = fun _ -> None], i.e. the last group is dropped if shorter than [n].
    @raise Invalid_argument if [offset <= 0] or [n <= 0]
    @since 1.0 *)

val pure : 'a -> 'a t

val (<*>) : ('a -> 'b) t -> 'a t -> 'b t

val (<$>) : ('a -> 'b) -> 'a t -> 'b t

val return : 'a -> 'a t

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

val take : int -> 'a t -> 'a t
(** Take the [n] first elements, drop the rest *)

val drop : int -> 'a t -> 'a t
(** Drop the [n] first elements, keep the rest *)

val hd_tl : 'a t -> 'a * 'a t
(** [hd_tl (x :: l)] returns [hd, l].
    @raise Failure if the list is empty
    @since 0.16 *)

val take_drop : int -> 'a t -> 'a t * 'a t
(** [take_drop n l] returns [l1, l2] such that [l1 @ l2 = l] and
    [length l1 = min (length l) n] *)

val take_while : ('a -> bool) -> 'a t -> 'a t
(** @since 0.13 *)

val drop_while : ('a -> bool) -> 'a t -> 'a t
(** @since 0.13 *)

val last : int -> 'a t -> 'a t
(** [last n l] takes the last [n] elements of [l] (or less if
    [l] doesn't have that many elements *)

val head_opt : 'a t -> 'a option
(** First element.
    @since 0.20 *)

val last_opt : 'a t -> 'a option
(** Last element.
    @since 0.20 *)

val find_pred : ('a -> bool) -> 'a t -> 'a option
(** [find_pred p l] finds the first element of [l] that satisfies [p],
    or returns [None] if no element satisfies [p]
    @since 0.11 *)

val find_pred_exn : ('a -> bool) -> 'a t -> 'a
(** Unsafe version of {!find_pred}
    @raise Not_found if no such element is found
    @since 0.11 *)

val find_map : ('a -> 'b option) -> 'a t -> 'b option
(** [find_map f l] traverses [l], applying [f] to each element. If for
    some element [x], [f x = Some y], then [Some y] is returned. Otherwise
    the call returns [None]
    @since 0.11 *)

val find_mapi : (int -> 'a -> 'b option) -> 'a t -> 'b option
(** Like {!find_map}, but also pass the index to the predicate function.
    @since 0.11 *)

val find_idx : ('a -> bool) -> 'a t -> (int * 'a) option
(** [find_idx p x] returns [Some (i,x)] where [x] is the [i]-th element of [l],
    and [p x] holds. Otherwise returns [None] *)

val remove : ?eq:('a -> 'a -> bool) -> x:'a -> 'a t -> 'a t
(** [remove ~x l] removes every instance of [x] from [l]. Tailrec.
    @param eq equality function
    @since 0.11 *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** Map and remove elements at the same time *)

val sorted_merge : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
(** Merges elements from both sorted list *)

val sort_uniq : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list
(** Sort the list and remove duplicate elements *)

val sorted_merge_uniq : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
(** [sorted_merge_uniq l1 l2] merges the sorted lists [l1] and [l2] and
    removes duplicates
    @since 0.10 *)

val is_sorted : ?cmp:('a -> 'a -> int) -> 'a list -> bool
(** [is_sorted l] returns [true] iff [l] is sorted (according to given order)
    @param cmp the comparison function (default [Pervasives.compare])
    @since 0.17 *)

val sorted_insert : ?cmp:('a -> 'a -> int) -> ?uniq:bool -> 'a -> 'a list -> 'a list
(** [sorted_insert x l] inserts [x] into [l] such that, if [l] was sorted,
    then [sorted_insert x l] is sorted too.
    @param uniq if true and [x] is already in sorted position in [l], then
      [x] is not duplicated. Default [false] ([x] will be inserted in any case).
    @since 0.17 *)

(*$Q
    Q.(pair small_int (list small_int)) (fun (x,l) -> \
      let l = List.sort Pervasives.compare l in \
      is_sorted (sorted_insert x l))
*)

val uniq_succ : ?eq:('a -> 'a -> bool) -> 'a list -> 'a list
(** [uniq_succ l] removes duplicate elements that occur one next to the other.
    Examples:
    [uniq_succ [1;2;1] = [1;2;1]]
    [uniq_succ [1;1;2] = [1;2]]
    @since 0.10 *)

val group_succ : ?eq:('a -> 'a -> bool) -> 'a list -> 'a list list
(** [group_succ ~eq l] groups together consecutive elements that are equal
    according to [eq]
    @since 0.11 *)

(** {2 Indices} *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

val iteri : (int -> 'a -> unit) -> 'a t -> unit

val foldi : ('b -> int -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** Fold on list, with index *)

val get_at_idx : int -> 'a t -> 'a option

val get_at_idx_exn : int -> 'a t -> 'a
(** Get the i-th element, or
    @raise Not_found if the index is invalid *)

val set_at_idx : int -> 'a -> 'a t -> 'a t
(** Set i-th element (removes the old one), or does nothing if
    index is too high *)

val insert_at_idx : int -> 'a -> 'a t -> 'a t
(** Insert at i-th position, between the two existing elements. If the
    index is too high, append at the end of the list *)

val remove_at_idx : int -> 'a t -> 'a t
(** Remove element at given index. Does nothing if the index is
    too high. *)

(** {2 Set Operators}

    Those operations maintain the invariant that the list does not
    contain duplicates (if it already satisfies it) *)

val add_nodup : ?eq:('a -> 'a -> bool) -> 'a -> 'a t -> 'a t
(** [add_nodup x set] adds [x] to [set] if it was not already present. Linear time.
    @since 0.11 *)

val remove_one : ?eq:('a -> 'a -> bool) -> 'a -> 'a t -> 'a t
(** [remove_one x set] removes one occurrence of [x] from [set]. Linear time.
    @since 0.11 *)

val mem : ?eq:('a -> 'a -> bool) -> 'a -> 'a t -> bool
(** Membership to the list. Linear time *)

val subset : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Test for inclusion *)

val uniq : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t
(** Remove duplicates w.r.t the equality predicate.
    Complexity is quadratic in the length of the list, but the order
    of elements is preserved. If you wish for a faster de-duplication
    but do not care about the order, use {!sort_uniq}*)

val union : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t
(** List union. Complexity is product of length of inputs. *)

val inter : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t
(** List intersection. Complexity is product of length of inputs. *)

(** {2 Other Constructors} *)

val range_by : step:int -> int -> int -> int t
(** [range_by ~step i j] iterates on integers from [i] to [j] included,
    where the difference between successive elements is [step].
    use a negative [step] for a decreasing list.
    @raise Invalid_argument if [step=0]
    @since 0.18 *)

val range : int -> int -> int t
(** [range i j] iterates on integers from [i] to [j] included . It works
    both for decreasing and increasing ranges *)

val range' : int -> int -> int t
(** Same as {!range} but the second bound is excluded.
    For instance [range' 0 5 = [0;1;2;3;4]] *)

val (--) : int -> int -> int t
(** Infix alias for [range] *)

val (--^) : int -> int -> int t
(** Infix alias for [range']
    @since 0.17 *)

val replicate : int -> 'a -> 'a t
(** Replicate the given element [n] times *)

val repeat : int -> 'a t -> 'a t
(** Concatenate the list with itself [n] times *)

(** {2 Association Lists} *)

module Assoc : sig
  type ('a, 'b) t = ('a*'b) list

  val get : ?eq:('a->'a->bool) -> 'a -> ('a,'b) t -> 'b option
  (** Find the element *)

  val get_exn : ?eq:('a->'a->bool) -> 'a -> ('a,'b) t -> 'b
  (** Same as [get], but unsafe
      @raise Not_found if the element is not present *)

  val set : ?eq:('a->'a->bool) -> 'a -> 'b -> ('a,'b) t -> ('a,'b) t
  (** Add the binding into the list (erase it if already present) *)

  val mem : ?eq:('a->'a->bool) -> 'a -> ('a,_) t -> bool
  (** [mem x l] returns [true] iff [x] is a key in [l]
      @since 0.16 *)

  val update :
    ?eq:('a->'a->bool) -> f:('b option -> 'b option) -> 'a -> ('a,'b) t -> ('a,'b) t
  (** [update k ~f l] updates [l] on the key [k], by calling [f (get l k)]
      and removing [k] if it returns [None], mapping [k] to [v'] if it
      returns [Some v']
      @since 0.16 *)

  val remove : ?eq:('a->'a->bool) -> 'a -> ('a,'b) t -> ('a,'b) t
  (** [remove x l] removes the first occurrence of [k] from [l].
      @since 0.17 *)
end

(** {2 References on Lists}
    @since 0.3.3 *)

module Ref : sig
  type 'a t = 'a list ref

  val push : 'a t -> 'a -> unit

  val pop : 'a t -> 'a option

  val pop_exn : 'a t -> 'a
  (** Unsafe version of {!pop}.
      @raise Failure if the list is empty *)

  val create : unit -> 'a t
  (** Create a new list reference *)

  val clear : _ t -> unit
  (** Remove all elements *)

  val lift : ('a list -> 'b) -> 'a t -> 'b
  (** Apply a list function to the content *)

  val push_list : 'a t -> 'a list -> unit
  (** Add elements of the list at the beginning of the list ref. Elements
      at the end of the list will be at the beginning of the list ref *)
end

(** {2 Monadic Operations} *)
module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module Traverse(M : MONAD) : sig
  val sequence_m : 'a M.t t -> 'a t M.t

  val fold_m : ('b -> 'a -> 'b M.t) -> 'b -> 'a t -> 'b M.t

  val map_m : ('a -> 'b M.t) -> 'a t -> 'b t M.t

  val map_m_par : ('a -> 'b M.t) -> 'a t -> 'b t M.t
  (** Same as {!map_m} but [map_m_par f (x::l)] evaluates [f x] and
      [f l] "in parallel" before combining their result (for instance
      in Lwt). *)
end

(** {2 Conversions} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]
type 'a printer = Format.formatter -> 'a -> unit
type 'a random_gen = Random.State.t -> 'a

val random : 'a random_gen -> 'a t random_gen
val random_non_empty : 'a random_gen -> 'a t random_gen
val random_len : int -> 'a random_gen -> 'a t random_gen

val random_choose : 'a t -> 'a random_gen
(** Randomly choose an element in the list.
    @raise Not_found if the list is empty *)

val random_sequence : 'a random_gen t -> 'a t random_gen

val to_seq : 'a t -> 'a sequence
val of_seq : 'a sequence -> 'a t

val to_gen : 'a t -> 'a gen
val of_gen : 'a gen -> 'a t

val to_klist : 'a t -> 'a klist
val of_klist : 'a klist -> 'a t

(** {2 Infix Operators}
    It is convenient to {!open CCList.Infix} to access the infix operators
    without cluttering  the scope too much.

    @since 0.16 *)

module Infix : sig
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  val (@) : 'a t -> 'a t -> 'a t
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  val (<$>) : ('a -> 'b) -> 'a t -> 'b t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (--) : int -> int -> int t

  val (--^) : int -> int -> int t
  (** @since 0.17 *)
end

(** {2 IO} *)

val pp : ?start:string -> ?stop:string -> ?sep:string ->
  'a printer -> 'a t printer
