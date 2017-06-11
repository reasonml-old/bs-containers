
type ('k, 'v) t = ('k * 'v) list

val get : ?eq:('k -> 'k -> bool) -> 'k -> ('k, 'v) t -> 'v option
(** Find the element *)

val getOrRaise : ?eq:('k -> 'k -> bool) -> 'k -> ('k, 'v) t -> 'v
(** Same as [get], but unsafe
    @raise Not_found if the element is not present *)

val set : ?eq:('k -> 'k -> bool) -> 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
(** Add the binding into the list (erase it if already present) *)

val update :
  ?eq:('k -> 'k -> bool) -> f:('v option -> 'v option) -> 'k -> ('k, 'v) t -> ('k, 'v) t
(** [update k ~f l] updates [l] on the key [k], by calling [f (get l k)]
    and removing [k] if it returns [None], mapping [k] to [v'] if it
    returns [Some v'] *)

val find : 'k -> ('k, 'v) t -> 'v
(** [assoc a l] returns the value associated with key [a] in the list of
    pairs [l]. That is,
    [assoc a [ ...; (a,b); ...] = b]
    if [(a,b)] is the leftmost binding of [a] in list [l].
    Raise [Not_found] if there is no value associated with [a] in the
    list [l]. *)

val findByReference : 'k -> ('k, 'v) t -> 'v
(** Same as {!List.assoc}, but uses physical equality instead of structural
    equality to compare keys. *)

  val includes : ?eq:('a->'a->bool) -> 'a -> ('a,_) t -> bool
  (** [mem x l] returns [true] iff [x] is a key in [l] *)

val includesByReference : 'k -> ('k, 'v) t -> bool
(** Same as {!List.mem_assoc}, but uses physical equality instead of
    structural equality to compare keys. *)

  val remove : ?eq:('a->'a->bool) -> 'a -> ('a,'b) t -> ('a,'b) t
  (** [remove x l] removes the first occurrence of [k] from [l]. *)

val removeByReference : 'k -> ('k, 'v) t -> ('k, 'v) t
(** Same as {!List.remove_assoc}, but uses physical equality instead
    of structural equality to compare keys.  Not tail-recursive. *)
