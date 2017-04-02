(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Extensions of Standard Map} *)

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
      @since 3.12.0
  *)

  val remove: key -> 'a t -> 'a t
  (** [remove x m] returns a map containing the same bindings as
      [m], except for [x] which is unbound in the returned map. *)

  val merge:
    (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  (** [merge f m1 m2] computes a map whose keys is a subset of keys of [m1]
      and of [m2]. The presence of each such binding, and the corresponding
      value, is determined with the function [f].
      @since 3.12.0
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

  val for_all: (key -> 'a -> bool) -> 'a t -> bool
  (** [for_all p m] checks if all the bindings of the map
      satisfy the predicate [p].
      @since 3.12.0
  *)

  val exists: (key -> 'a -> bool) -> 'a t -> bool
  (** [exists p m] checks if at least one binding of the map
      satisfy the predicate [p].
      @since 3.12.0
  *)

  val filter: (key -> 'a -> bool) -> 'a t -> 'a t
  (** [filter p m] returns the map with all the bindings in [m]
      that satisfy predicate [p].
      @since 3.12.0
  *)

  val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  (** [partition p m] returns a pair of maps [(m1, m2)], where
      [m1] contains all the bindings of [s] that satisfy the
      predicate [p], and [m2] is the map with all the bindings of
      [s] that do not satisfy [p].
      @since 3.12.0
  *)

  val cardinal: 'a t -> int
  (** Return the number of bindings of a map.
      @since 3.12.0
  *)

  val bindings: 'a t -> (key * 'a) list
  (** Return the list of all bindings of the given map.
      The returned list is sorted in increasing order with respect
      to the ordering [Ord.compare], where [Ord] is the argument
      given to {!Map.Make}.
      @since 3.12.0
  *)

  val min_binding: 'a t -> (key * 'a)
  (** Return the smallest binding of the given map
      (with respect to the [Ord.compare] ordering), or raise
      [Not_found] if the map is empty.
      @since 3.12.0
  *)

  val max_binding: 'a t -> (key * 'a)
  (** Same as {!Map.S.min_binding}, but returns the largest binding
      of the given map.
      @since 3.12.0
  *)

  val choose: 'a t -> (key * 'a)
  (** Return one binding of the given map, or raise [Not_found] if
      the map is empty. Which binding is chosen is unspecified,
      but equal bindings will be chosen for equal maps.
      @since 3.12.0
  *)

  val split: key -> 'a t -> 'a t * 'a option * 'a t
  (** [split x m] returns a triple [(l, data, r)], where
        [l] is the map with all the bindings of [m] whose key
      is strictly less than [x];
        [r] is the map with all the bindings of [m] whose key
      is strictly greater than [x];
        [data] is [None] if [m] contains no binding for [x],
        or [Some v] if [m] binds [v] to [x].
      @since 3.12.0
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
  (** Same as {!Map.S.map}, but the function receives as arguments both the
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

  val keys : _ t -> key list
  (** Iterate on keys only *)

  val values : 'a t -> 'a list
  (** Iterate on values only *)
end


module Make(O : Map.OrderedType) = struct
  module IMap = Map.Make(O)

  type key = IMap.key

  type 'a t = 'a IMap.t

  let empty = IMap.empty

  let is_empty = IMap.is_empty

  let mem = IMap.mem

  let add = IMap.add

  let singleton = IMap.singleton

  let remove = IMap.remove

  let merge = IMap.merge

  let compare = IMap.compare

  let equal = IMap.equal

  let iter = IMap.iter

  let fold = IMap.fold

  let for_all = IMap.for_all

  let exists = IMap.exists

  let filter = IMap.filter

  let partition = IMap.partition

  let cardinal = IMap.cardinal

  let bindings = IMap.bindings

  let min_binding = IMap.min_binding

  let max_binding = IMap.max_binding

  let choose = IMap.choose

  let split = IMap.split

  let find = IMap.find

  let map = IMap.map

  let mapi = IMap.mapi

  let isEmpty = IMap.is_empty

  let forAll = IMap.for_all

  let minBinding = IMap.min_binding

  let maxBinding = IMap.max_binding

  let size m = IMap.fold (fun _ _ v -> v + 1) m 0

  let get k m =
    try Some (IMap.find k m)
    with Not_found -> None

  let getOr k m ~default =
    try IMap.find k m
    with Not_found -> default

  let update k f m =
    let x =
      try f (Some (IMap.find k m))
      with Not_found -> f None
    in
    match x with
    | None -> IMap.remove k m
    | Some v' -> IMap.add k v' m

  let mergeSafe ~f a b =
    IMap.merge
      (fun k v1 v2 -> match v1, v2 with
         | None, None -> assert false
         | Some v1, None -> f k (`Left v1)
         | None, Some v2 -> f k (`Right v2)
         | Some v1, Some v2 -> f k (`Both (v1,v2)))
      a b

  let addList m l = List.fold_left (fun m (k,v) -> IMap.add k v m) m l

  let fromList l = addList IMap.empty l

  let toList m =
    IMap.fold (fun k v acc -> (k,v)::acc) m []

  let addSeq m s =
    let m = ref m in
    s (fun (k,v) -> m := IMap.add k v !m);
    !m

  let fromSeq s = addSeq IMap.empty s

  let toSeq m yield = IMap.iter (fun k v -> yield (k,v)) m

  let keys m = fold (fun k _ list -> k::list) m []

  let values m = fold (fun _ v list -> v::list) m []
end
