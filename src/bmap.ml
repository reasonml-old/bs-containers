(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Extensions of Standard Map} *)

module type S = sig
  include Map.S

  val isEmpty: 'a t -> bool

  val forAll: (key -> 'a -> bool) -> 'a t -> bool

  val minBinding: 'a t -> key * 'a

  val maxBinding: 'a t -> key * 'a 

  val size: 'a t -> int

  val get : key -> 'a t -> 'a option
  (** Safe version of {!find} *)

  val getOr : key -> 'a t -> default:'a -> 'a
  (** [get_or k m ~default] returns the value associated to [k] if present,
      and returns [default] otherwise (if [k] doesn't belong in [m])
      @since 0.16 *)

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  (** [update k f m] calls [f (Some v)] if [find k m = v],
      otherwise it calls [f None]. In any case, if the result is [None]
      [k] is removed from [m], and if the result is [Some v'] then
      [add k v' m] is returned. *)

  val mergeSafe :
    f:(key -> [`Left of 'a | `Right of 'b | `Both of 'a * 'b] -> 'c option) ->
    'a t -> 'b t -> 'c t
  (** [mergeSafe ~f a b] merges the maps [a] and [b] together.
      @since 0.17 *)

  val ofList : (key * 'a) list -> 'a t

  val addList : 'a t -> (key * 'a) list -> 'a t
  (** @since 0.14 *)

 val toList : 'a t -> (key * 'a) list

end

module Make(O : Map.OrderedType) = struct
  include Map.Make(O)

  let isEmpty = is_empty

  let forAll = for_all

  let minBinding = min_binding

  let maxBinding = max_binding

  let size m = fold (fun _ _ v -> v + 1) m 0

  let get k m =
    try Some (find k m)
    with Not_found -> None

  let getOr k m ~default =
    try find k m
    with Not_found -> default

  let update k f m =
    let x =
      try f (Some (find k m))
      with Not_found -> f None
    in
    match x with
      | None -> remove k m
      | Some v' -> add k v' m

  let mergeSafe ~f a b =
    merge
      (fun k v1 v2 -> match v1, v2 with
         | None, None -> assert false
         | Some v1, None -> f k (`Left v1)
         | None, Some v2 -> f k (`Right v2)
         | Some v1, Some v2 -> f k (`Both (v1,v2)))
      a b

  let addList m l = List.fold_left (fun m (k,v) -> add k v m) m l

  let ofList l = addList empty l

  let toList m =
    fold (fun k v acc -> (k,v)::acc) m []
end
