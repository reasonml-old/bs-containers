(* This file is free software, part of containers. See file "license" for more details. *)

module type S = sig

  type key

  type (+'a) t

  val empty: 'a t

  val is_empty: 'a t -> bool

  val mem: key -> 'a t -> bool

  val add: key -> 'a -> 'a t -> 'a t

  val singleton: key -> 'a -> 'a t

  val remove: key -> 'a t -> 'a t

  val merge:
    (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t

  val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int

  val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val iter: (key -> 'a -> unit) -> 'a t -> unit

  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val exists: (key -> 'a -> bool) -> 'a t -> bool

  val filter: (key -> 'a -> bool) -> 'a t -> 'a t

  val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t

  val cardinal: 'a t -> int

  val bindings: 'a t -> (key * 'a) list

  val choose: 'a t -> (key * 'a)

  val split: key -> 'a t -> 'a t * 'a option * 'a t

  val find: key -> 'a t -> 'a

  val map: ('a -> 'b) -> 'a t -> 'b t

  val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t

  val isEmpty: 'a t -> bool

  val forAll: (key -> 'a -> bool) -> 'a t -> bool

  val minBinding: 'a t -> key * 'a

  val maxBinding: 'a t -> key * 'a 

  val size: 'a t -> int

  val get : key -> 'a t -> 'a option

  val getOr : key -> 'a t -> default:'a -> 'a

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t

  val mergeSafe :
    f:(key -> [`Left of 'a | `Right of 'b | `Both of 'a * 'b] -> 'c option) ->
    'a t -> 'b t -> 'c t

  val fromList : (key * 'a) list -> 'a t

  val addList : 'a t -> (key * 'a) list -> 'a t

  val toList : 'a t -> (key * 'a) list

  val fromSeq: (key * 'a) Sequence.t -> 'a t

  val toSeq: 'a t -> (key * 'a) Sequence.t

  val keysList : _ t -> key list

  val valuesList : 'a t -> 'a list
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

  let exists = IMap.exists

  let filter = IMap.filter

  let partition = IMap.partition

  let cardinal = IMap.cardinal

  let bindings = IMap.bindings

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

  let keysList m = fold (fun k _ list -> k::list) m []

  let valuesList m = fold (fun _ v list -> v::list) m []
end
