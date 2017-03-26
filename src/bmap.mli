(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Extensions of Standard Map}

    Provide useful functions and iterators on [Map.S]
    @since 0.5 *)

module type S = sig
  include Map.S
  
  (* {2 {b stdlib} Function Replacements *)

  val isEmpty: 'a t -> bool

  val forAll: (key -> 'a -> bool) -> 'a t -> bool

  val minBinding: 'a t -> key * 'a

  val maxBinding: 'a t -> key * 'a 

  (* {2 {b stdlib} Unconforming Functions} *)
  val size: 'a t -> int

  val get : key -> 'a t -> 'a option
  (** Safe version of {!find} *)

  val getOr : key -> 'a t -> default:'a -> 'a
  (** [getOr k m ~default] returns the value associated to [k] if present,
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
  (** Build a map from the given list of bindings [k_i -> v_i],
      added in order using {!add}.
      If a key occurs several times, only its last binding
      will be present in the result. *)

  val addList : 'a t -> (key * 'a) list -> 'a t
  (** @since 0.14 *)

  val toList : 'a t -> (key * 'a) list
end

module Make(O : Map.OrderedType) : S
  with type 'a t = 'a Map.Make(O).t
   and type key = O.t
