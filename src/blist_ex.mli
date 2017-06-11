
type 'a t = 'a list

val reverseAndMap : ('a -> 'b) -> 'a list -> 'b list
(** [List.rev_map f l] gives the same result as
    {!List.rev}[ (]{!List.map}[ f l)], but is tail-recursive and
    more efficient. *)

val forEach2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
(** [List.iter2 f [a1; ...; an] [b1; ...; bn]] calls in turn
    [f a1 b1; ...; f an bn].
    Raise [Invalid_argument] if the two lists have
    different lengths. *)

val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
(** [List.map2 f [a1; ...; an] [b1; ...; bn]] is
    [[f a1 b1; ...; f an bn]].
    Raise [Invalid_argument] if the two lists have
    different lengths.  Not tail-recursive. *)

val reverseAndMap2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
(** [List.rev_map2 f l1 l2] gives the same result as
    {!List.rev}[ (]{!List.map2}[ f l1 l2)], but is tail-recursive and
    more efficient. *)

val reduceLeft2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
(** [List.fold_left2 f a [b1; ...; bn] [c1; ...; cn]] is
    [f (... (f (f a b1 c1) b2 c2) ...) bn cn].
    Raise [Invalid_argument] if the two lists have
    different lengths. *)

val reduceRight2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
(** [List.fold_right2 f [a1; ...; an] [b1; ...; bn] c] is
    [f a1 b1 (f a2 b2 (... (f an bn c) ...))].
    Raise [Invalid_argument] if the two lists have
    different lengths.  Not tail-recursive. *)

val forAll2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
(** Same as {!List.for_all}, but for a two-argument predicate.
    Raise [Invalid_argument] if the two lists have
    different lengths. *)

val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
(** Same as {!List.exists}, but for a two-argument predicate.
    Raise [Invalid_argument] if the two lists have
    different lengths. *)

val includesByReference : 'a -> 'a list -> bool
(** Same as {!List.mem}, but uses physical equality instead of structural
    equality to compare list elements. *)

val stableSort : ('a -> 'a -> int) -> 'a list -> 'a list
(** Same as {!List.sort}, but the sorting algorithm is guaranteed to
    be stable (i.e. elements that compare equal are kept in their
    original order) .
    The current implementation uses Merge Sort. It runs in constant
    heap space and logarithmic stack space.
*)

val fastSort : ('a -> 'a -> int) -> 'a list -> 'a list
(** Same as {!List.sort} or {!List.stable_sort}, whichever is faster
    on typical input. *)

val consMaybe : 'a option -> 'a t -> 'a t
(** [cons_maybe (Some x) l] is [x :: l]
    [cons_maybe None l] is [l] *)

val reduceAndMap : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a list -> 'acc * 'b list
(** [fold_map f acc l] is a [fold_left]-like function, but it also maps the
    list to another list. *)

val reduceAndMap2 : ('acc -> 'a -> 'b -> 'acc * 'c) -> 'acc -> 'a list -> 'b list -> 'acc * 'c list
(** [fold_map2] is to [fold_map] what [List.map2] is to [List.map].
    @raise Invalid_argument if the lists do not have the same length *)

val reduceAndFilterAndMap : ('acc -> 'a -> 'acc * 'b option) -> 'acc -> 'a list -> 'acc * 'b list
(** [fold_filter_map f acc l] is a [fold_left]-like function, but also
    generates a list of output in a way similar to {!filter_map} *)

val reduceAndFlatmap : ('acc -> 'a -> 'acc * 'b list) -> 'acc -> 'a list -> 'acc * 'b list
(** [fold_flat_map f acc l] is a [fold_left]-like function, but it also maps the
    list to a list of lists that is then [flatten]'d.. *)

val product : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Cartesian product of the two lists, with the given combinator *)

val reduceProduct : ('c -> 'a -> 'b -> 'c) -> 'c -> 'a t -> 'b t -> 'c
(** Fold on the cartesian product *)

val diagonal : 'a t -> ('a * 'a) t
(** All pairs of distinct positions of the list. [list_diagonal l] will
    return the list of [List.nth i l, List.nth j l] if [i < j]. *)

val partitionMap : ('a -> [<`Left of 'b | `Right of 'c | `Drop]) ->
  'a list -> 'b list * 'c list
(** [partition_map f l] maps [f] on [l] and gather results in lists:
    - if [f x = `Left y], adds [y] to the first list
    - if [f x = `Right z], adds [z] to the second list
    - if [f x = `Drop], ignores [x] *)

val sublistsOfLength :
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
    @raise Invalid_argument if [offset <= 0] or [n <= 0] *)

val headAndTail : 'a t -> 'a * 'a t
(** [hd_tl (x :: l)] returns [hd, l].
    @raise Failure if the list is empty *)

val findAndMap : ('a -> 'b option) -> 'a t -> 'b option
(** [find_map f l] traverses [l], applying [f] to each element. If for
    some element [x], [f x = Some y], then [Some y] is returned. Otherwise
    the call returns [None] *)

val findAndMapi : (int -> 'a -> 'b option) -> 'a t -> 'b option
(** Like {!find_map}, but also pass the index to the predicate function. *)

val findWithIndex : ('a -> bool) -> 'a t -> (int * 'a) option
(** [find_idx p x] returns [Some (i,x)] where [x] is the [i]-th element of [l],
    and [p x] holds. Otherwise returns [None] *)

val filterMap : ('a -> 'b option) -> 'a t -> 'b t
(** Map and remove elements at the same time *)

val sortedMerge : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
(** Merges elements from both sorted list *)

val sortUnique : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list
(** Sort the list and remove duplicate elements *)

val sortedMergeUnique : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
(** [sorted_merge_uniq l1 l2] merges the sorted lists [l1] and [l2] and
    removes duplicates *)

val isSorted : ?cmp:('a -> 'a -> int) -> 'a list -> bool
(** [is_sorted l] returns [true] iff [l] is sorted (according to given order)
    @param cmp the comparison function (default [Pervasives.compare]) *)

val sortedInsert : ?cmp:('a -> 'a -> int) -> ?uniq:bool -> 'a -> 'a list -> 'a list
(** [sorted_insert x l] inserts [x] into [l] such that, if [l] was sorted,
    then [sorted_insert x l] is sorted too.
    @param uniq if true and [x] is already in sorted position in [l], then
      [x] is not duplicated. Default [false] ([x] will be inserted in any case). *)

(*$Q
    Q.(pair small_int (list small_int)) (fun (x,l) -> \
      let l = List.sort Pervasives.compare l in \
      is_sorted (sorted_insert x l))
*)

val uniqueSuccessors : ?eq:('a -> 'a -> bool) -> 'a list -> 'a list
(** [uniq_succ l] removes duplicate elements that occur one next to the other.
    Examples:
    [uniq_succ [1;2;1] = [1;2;1]]
    [uniq_succ [1;1;2] = [1;2]] *)

val groupDuplicateSuccessors : ?eq:('a -> 'a -> bool) -> 'a list -> 'a list list
(** [group_succ ~eq l] groups together consecutive elements that are equal
    according to [eq] *)

(** {2 Set Operators}

    Those operations maintain the invariant that the list does not
    contain duplicates (if it already satisfies it) *)

val addUnlessDuplicate : ?eq:('a -> 'a -> bool) -> 'a -> 'a t -> 'a t
(** [add_nodup x set] adds [x] to [set] if it was not already present. Linear time. *)

val removeFirst : ?eq:('a -> 'a -> bool) -> 'a -> 'a t -> 'a t
(** [remove_one x set] removes one occurrence of [x] from [set]. Linear time. *)

val includes : ?eq:('a -> 'a -> bool) -> 'a -> 'a t -> bool
(** Membership to the list. Linear time *)

val subset : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Test for inclusion *)

val unique : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t
(** Remove duplicates w.r.t the equality predicate.
    Complexity is quadratic in the length of the list, but the order
    of elements is preserved. If you wish for a faster de-duplication
    but do not care about the order, use {!sort_uniq}*)

val union : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t
(** List union. Complexity is product of length of inputs. *)

val intersection : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t
(** List intersection. Complexity is product of length of inputs. *)
