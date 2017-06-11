
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 complements to list} *)

val reverseAndAppend : 'a list -> 'a list -> 'a list
(** [List.rev_append l1 l2] reverses [l1] and concatenates it to [l2].
    This is equivalent to {!List.rev}[ l1 @ l2], but [rev_append] is
    tail-recursive and more efficient. *)


(*-- Start stdlib list, from https://github.com/ocaml/ocaml/blob/4.02.3/stdlib/list.mli --*)

val length : 'a list -> int
(** Return the length (number of elements) of the given list. *)

val head : 'a list -> 'a
(** Return the first element of the given list. Raise
    [Failure "hd"] if the list is empty. *)

val tail : 'a list -> 'a list
(** Return the given list without its first element. Raise
    [Failure "tl"] if the list is empty. *)

val nth : 'a list -> int -> 'a
(** Return the [n]-th element of the given list.
    The first element (head of the list) is at position 0.
    Raise [Failure "nth"] if the list is too short.
    Raise [Invalid_argument "List.nth"] if [n] is negative. *)

val reverse : 'a list -> 'a list
(** List reversal. *)

val concat : 'a list list -> 'a list
(** Concatenate a list of lists.  The elements of the argument are all
    concatenated together (in the same order) to give the result.
    Not tail-recursive
    (length of the argument + length of the longest sub-list). *)


(** {6 Iterators} *)


val forEach : ('a -> unit) -> 'a list -> unit
(** [List.iter f [a1; ...; an]] applies function [f] in turn to
    [a1; ...; an]. It is equivalent to
    [begin f a1; f a2; ...; f an; () end]. *)

val reduceLeft : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
(** [List.fold_left f a [b1; ...; bn]] is
    [f (... (f (f a b1) b2) ...) bn]. *)


(** {6 Iterators on two lists} *)



(** {6 List scanning} *)


val forAll : ('a -> bool) -> 'a list -> bool
(** [for_all p [a1; ...; an]] checks if all elements of the list
    satisfy the predicate [p]. That is, it returns
    [(p a1) && (p a2) && ... && (p an)]. *)

val exists : ('a -> bool) -> 'a list -> bool
(** [exists p [a1; ...; an]] checks if at least one element of
    the list satisfies the predicate [p]. That is, it returns
    [(p a1) || (p a2) || ... || (p an)]. *)


(** {6 List searching} *)

val findOrRaise : ('a -> bool) -> 'a list -> 'a
(** [find p l] returns the first element of the list [l]
    that satisfies the predicate [p].
    Raise [Not_found] if there is no value that satisfies [p] in the
    list [l]. *)

val findAll : ('a -> bool) -> 'a list -> 'a list
(** [find_all] is another name for {!List.filter}. *)

val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
(** [partition p l] returns a pair of lists [(l1, l2)], where
    [l1] is the list of all the elements of [l] that
    satisfy the predicate [p], and [l2] is the list of all the
    elements of [l] that do not satisfy [p].
    The order of the elements in the input list is preserved. *)



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
type 'a sequence = ('a -> unit) -> unit

val empty : 'a t

val isEmpty : _ t -> bool
(** [is_empty l] returns [true] iff [l = []] *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Safe version of map *)

val cons : 'a -> 'a t -> 'a t
(** [cons x l] is [x::l] *)

val append : 'a t -> 'a t -> 'a t
(** Safe version of append *)

val (@) : 'a t -> 'a t -> 'a t

val filter : ('a -> bool) -> 'a t -> 'a t
(** Safe version of {!List.filter} *)

val reduceRight : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** Safe version of [fold_right] *)

val reduceWhile : ('a -> 'b -> 'a * [`Stop | `Continue]) -> 'a -> 'b t -> 'a
(** Fold until a stop condition via [('a, `Stop)] is
    indicated by the accumulator
    @since 0.8 *)

val init : int -> (int -> 'a) -> 'a t
(** Similar to {!Array.init} *)

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val flatMap : ('a -> 'b t) -> 'a t -> 'b t
(** Map and flatten at the same time (safe). Evaluation order is not guaranteed. *)

val flatten : 'a t t -> 'a t
(** Safe flatten *)

val of_ : 'a -> 'a t

val take : int -> 'a t -> 'a t
(** Take the [n] first elements, drop the rest *)

val skip : int -> 'a t -> 'a t
(** Drop the [n] first elements, keep the rest *)

val splitAt : int -> 'a t -> 'a t * 'a t
(** [take_drop n l] returns [l1, l2] such that [l1 @ l2 = l] and
    [length l1 = min (length l) n] *)

val takeWhile : ('a -> bool) -> 'a t -> 'a t

val skipWhile : ('a -> bool) -> 'a t -> 'a t

val last : int -> 'a t -> 'a t
(** [last n l] takes the last [n] elements of [l] (or less if
    [l] doesn't have that many elements *)

val headOption : 'a t -> 'a option
(** First element. *)

val lastOption : 'a t -> 'a option
(** Last element. *)

val find : ('a -> bool) -> 'a t -> 'a option
(** [find_pred p l] finds the first element of [l] that satisfies [p],
    or returns [None] if no element satisfies [p] *)

val removeAll : ?eq:('a -> 'a -> bool) -> x:'a -> 'a t -> 'a t
(** [remove ~x l] removes every instance of [x] from [l]. Tailrec.
    @param eq equality function *)

(** {2 Indices} *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

val forEachi : (int -> 'a -> unit) -> 'a t -> unit

val reducei : ('b -> int -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** Fold on list, with index *)

val get : int -> 'a t -> 'a option

val getOrRaise : int -> 'a t -> 'a
(** Get the i-th element, or
    @raise Not_found if the index is invalid *)

val set : int -> 'a -> 'a t -> 'a t
(** Set i-th element (removes the old one), or does nothing if
    index is too high *)

val insert : int -> 'a -> 'a t -> 'a t
(** Insert at i-th position, between the two existing elements. If the
    index is too high, append at the end of the list *)

val removeAt : int -> 'a t -> 'a t
(** Remove element at given index. Does nothing if the index is
    too high. *)

(** {2 Other Constructors} *)

val rangeBy : step:int -> int -> int -> int t
(** [range_by ~step i j] iterates on integers from [i] to [j] included,
    where the difference between successive elements is [step].
    use a negative [step] for a decreasing list.
    @raise Invalid_argument if [step=0] *)

val range : int -> int -> int t
(** [range i j] iterates on integers from [i] to [j] included . It works
    both for decreasing and increasing ranges *)

val range' : int -> int -> int t
(** Same as {!range} but the second bound is excluded.
    For instance [range' 0 5 = [0;1;2;3;4]] *)

val (--) : int -> int -> int t
(** Infix alias for [range] *)

val (--^) : int -> int -> int t
(** Infix alias for [range'] *)

val replicate : int -> 'a -> 'a t
(** Replicate the given element [n] times *)

val repeat : int -> 'a t -> 'a t
(** Concatenate the list with itself [n] times *)

(** {2 Conversions} *)

val toSequence : 'a t -> 'a sequence
val fromSequence : 'a sequence -> 'a t

(** {2 Infix Operators}
    It is convenient to {!open CCList.Infix} to access the infix operators
    without cluttering  the scope too much. *)

module Infix : sig
  val (@) : 'a t -> 'a t -> 'a t
  val (--) : int -> int -> int t
  val (--^) : int -> int -> int t
end
