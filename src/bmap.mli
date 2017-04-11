(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Map}
    Provide a balanced tree as map container
*)

module type OrderedType = Map.OrderedType

module type S = sig

  type key
  (** The type of the map keys. *)

  type (+'a) t
  (** The type of maps from type [key] to type ['a]. *)

  val empty: 'a t
  (** The empty map. *)

  val is_empty: 'a t -> bool
  (** Test whether a map is empty or not. *)

  val mem: key -> 'a t -> bool
  (** [mem x m] returns [true] if [m] contains a binding for [x],
      and [false] otherwise. *)

  val add: key -> 'a -> 'a t -> 'a t
  (** [add x y m] returns a map containing the same bindings as
      [m], plus a binding of [x] to [y]. If [x] was already bound
      in [m], its previous binding disappears. *)

  val singleton: key -> 'a -> 'a t
  (** [singleton x y] returns the one-element map that contains a binding [y]
      for [x].
  *)

  val remove: key -> 'a t -> 'a t
  (** [remove x m] returns a map containing the same bindings as
      [m], except for [x] which is unbound in the returned map. *)

  val merge:
    (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  (** [merge f m1 m2] computes a map whose keys is a subset of keys of [m1]
      and of [m2]. The presence of each such binding, and the corresponding
      value, is determined with the function [f].
  *)

  val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
  (** Total ordering between maps.  The first argument is a total ordering
      used to compare data associated with equal keys in the two maps. *)

  val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  (** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are
      equal, that is, contain equal keys and associate them with
      equal data.  [cmp] is the equality predicate used to compare
      the data associated with the keys. *)

  val iter: (key -> 'a -> unit) -> 'a t -> unit
  (** [iter f m] applies [f] to all bindings in map [m].
      [f] receives the key as first argument, and the associated value
      as second argument.  The bindings are passed to [f] in increasing
      order with respect to the ordering over the type of the keys. *)

  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
      where [k1 ... kN] are the keys of all bindings in [m]
      (in increasing order), and [d1 ... dN] are the associated data. *)

  val exists: (key -> 'a -> bool) -> 'a t -> bool
  (** [exists p m] checks if at least one binding of the map
      satisfy the predicate [p].
  *)

  val filter: (key -> 'a -> bool) -> 'a t -> 'a t
  (** [filter p m] returns the map with all the bindings in [m]
      that satisfy predicate [p].
  *)

  val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  (** [partition p m] returns a pair of maps [(m1, m2)], where
      [m1] contains all the bindings of [s] that satisfy the
      predicate [p], and [m2] is the map with all the bindings of
      [s] that do not satisfy [p].
  *)

  val cardinal: 'a t -> int
  (** Return the number of bindings of a map.
  *)

  val bindings: 'a t -> (key * 'a) list
  (** Return the list of all bindings of the given map.
      The returned list is sorted in increasing order with respect
      to the ordering [Ord.compare], where [Ord] is the argument
      given to {!Map.Make}.
  *)

  val choose: 'a t -> (key * 'a)
  (** Return one binding of the given map, or raise [Not_found] if
      the map is empty. Which binding is chosen is unspecified,
      but equal bindings will be chosen for equal maps.
  *)

  val split: key -> 'a t -> 'a t * 'a option * 'a t
  (** [split x m] returns a triple [(l, data, r)], where
        [l] is the map with all the bindings of [m] whose key
      is strictly less than [x];
        [r] is the map with all the bindings of [m] whose key
      is strictly greater than [x];
        [data] is [None] if [m] contains no binding for [x],
        or [Some v] if [m] binds [v] to [x].
  *)

  val find: key -> 'a t -> 'a
  (** [find x m] returns the current binding of [x] in [m],
      or raises [Not_found] if no such binding exists. *)

  val map: ('a -> 'b) -> 'a t -> 'b t
  (** [map f m] returns a map with same domain as [m], where the
      associated value [a] of all bindings of [m] has been
      replaced by the result of the application of [f] to [a].
      The bindings are passed to [f] in increasing order
      with respect to the ordering over the type of the keys. *)

  val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
  (** Same as {!BMap.S.map}, but the function receives as arguments both the
      key and the associated value for each binding of the map. *)

  (** {2 Standard Library Replacement APIs } *)

  val isEmpty: 'a t -> bool

  val forAll: (key -> 'a -> bool) -> 'a t -> bool

  val minBinding: 'a t -> key * 'a

  val maxBinding: 'a t -> key * 'a 

  (** {2 Additional APIs } *)

  val size: 'a t -> int

  val get : key -> 'a t -> 'a option
  (** Safe version of {!find} *)

  val getOr : key -> 'a t -> default:'a -> 'a
  (** [getOr k m ~default] returns the value associated to [k] if present,
      and returns [default] otherwise (if [k] doesn't belong in [m]). *)

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  (** [update k f m] calls [f (Some v)] if [find k m = v],
      otherwise it calls [f None]. In any case, if the result is [None]
      [k] is removed from [m], and if the result is [Some v'] then
      [add k v' m] is returned. *)

  val mergeSafe :
    f:(key -> [`Left of 'a | `Right of 'b | `Both of 'a * 'b] -> 'c option) ->
    'a t -> 'b t -> 'c t
  (** [mergeSafe ~f a b] merges the maps [a] and [b] together. *)

  val fromList : (key * 'a) list -> 'a t
  (** Build a map from the given list of bindings [k_i -> v_i],
      added in order using {!add}.
      If a key occurs several times, only its last binding
      will be present in the result. *)

  val addList : 'a t -> (key * 'a) list -> 'a t

  val toList : 'a t -> (key * 'a) list

  val fromSeq: (key * 'a) Sequence.t -> 'a t

  val toSeq: 'a t -> (key * 'a) Sequence.t

  val keysList : _ t -> key list
  (** List of keys *)

  val valuesList : 'a t -> 'a list
  (** List of values *)
end

module Make(O : Map.OrderedType) : S
  with type 'a t = 'a Map.Make(O).t
   and type key = O.t
