
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Options} *)

type +'a t = 'a option
(** alias of the standard [option] type *)

(** {2 Construction} *)

(* was return *)
val make : 'a -> 'a t
(** [make x] is [Some x] *)

val fromList : 'a list -> 'a t
(** [fromList l] is [Some x] is [l] is [x :: _], [None] otherwise (ie. if [l] is
    [\[\]]) *)

val if_ : ('a -> bool) -> 'a -> 'a t
(** [if_ f x] is [Some x] if [f x], [None] otherwise *)

val wrap : ?handler:(exn -> bool) -> ('a -> 'b) -> 'a -> 'b t
(** [wrap f x] is [Some (f x)] if [f x] does not raise an exception, [None] if
    it does and [handler e] is [true], otherwise reraises the exception.

    @param handler the exception handler, which returns [true] if the
        exception is to be caught.
*)

val wrap2 : ?handler:(exn -> bool) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c t
(** [wrap2 f x y] is [Some (f x y)] if [f x y] does not raise an exception,
    [None] if it does and [handler e] is [true], otherwise reraises the exception.

    @param handler the exception handler, which returns [true] if the
        exception is to be caught.
*)

(** {2 Comparison} *)

val isSome : _ t -> bool
(** [isSome a] is [true] if [a] is [Some _], [false] otherwise *)

val isNone : _ t -> bool
(** [isNone a] is [true] if [a] is [None], [false] otherwise *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** [equal f a b] is [true] if both are [None], [f x y] if both are [Some _],
    [false] otherwise *)

val compare : ('a -> 'a -> Comparison.comparison) -> 'a t -> 'a t -> Comparison.comparison
(** [compare f a b] is [Equal] if both are [None], [f x y] if both are [Some _],
    [Greater] if [a] is [Some _] and [b] is [None], [Less] if vice versa. *)

(** {2 Access} *)

val get : 'a -> 'a t -> 'a
(** [get default a] is [x] if [a] is [Some x], [default] otherwise *)

val getOr : default:'a -> 'a t -> 'a
(** [getOr ~default a] is [x] if [a] is [Some x], [default] otherwise *)

val getOrRaise : 'a t -> 'a
(** [getOrRaise a] is [x] if [a] is [Some x], raises [Invalid_argument] otherwise

    @raise Invaliud_argument if given option is None    
*)

val getLazy : (unit -> 'a) -> 'a t -> 'a
(** [getLazy defaultFn a] is [x] if [a] is [Some x], [defaultFn ()] otherwise *)

(** {2 Iteration} *)

val forEach : ('a -> unit) -> 'a t -> unit
(** [forEach f a] calls [f x] if [a] is [Some x] *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f a] is [Some (f x)] if [a] is [Some x], [None] otherwise *)

val mapOr : default:'b -> ('a -> 'b) -> 'a t -> 'b
(** [mapOr ~default f a] is [Some (f x)] if [a] is [Some x], [default] otherwise *)

(* new *)
val mapOrLazy : default:(unit -> 'b) -> ('a -> 'b) -> 'a t -> 'b
(** [mapOrLazy ~default f a] is [Some (f x)] if [a] is [Some x], [default ()]
    otherwise *)

val maybe : ('a -> 'b) -> 'b -> 'a t -> 'b
(** [maybe f default a] is [Some (f x)] if [a] is [Some x], [default] otherwise *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [map2 f a b] is [Some (f x y)] if [a] is [Some x], and [b] is [Some y],
    [None] otherwise *)

(* alternative name: andThen *)
val flatMap : ('a -> 'b t) -> 'a t -> 'b t
(** [flatMap f a] is [f x] is [a] is [Some x], [None] otherwise *)

val reduce : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [reduce f initial a] is [f inital x] if [a] is [Some x], [initial] otherwise *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** [filter f a] is [a] if [a] is [Some x] and [f x] is [true], [None] otherwise *)

(** {2 Applicative} *)

(* was (<*>) *)
val apply : ('a -> 'b) t -> 'a t -> 'b t
(** [apply maybeF a] is [Some (f x)] if [maybeF) is [Some f] and [a] is [Some x],
    [None] otherwise *)

(** {2 Composition} *)

(* new *)
val and_ : 'b t -> 'a t -> 'b t
(** [and_ a b] is [b] if [a] is [Some _], [None] otherwise *)

(* was (<+>) *)
val or_ : else_:'a t -> 'a t -> 'a t
(** [or_ ~else_ a] is [a] if [a] is [Some _], [else_] otherwise *)

(* new *)
val orLazy : else_:(unit -> 'a t) -> 'a t -> 'a t
(** [orLazy ~else_ a] is [a] if [a] is [Some _], [else_ ()] otherwise *)

(* was choice *)
val any : 'a t list -> 'a t
(** [choice l] returns the first non-[None] element of the list if it exists,
    [None] otherwise *)

(** {2 Quantification} *)
(** TODO: too mathy? *)

val exists : ('a -> bool) -> 'a t -> bool
(** [exists f a] is [f x] if [a] is [Some x], [false] otherwise *)

val forAll : ('a -> bool) -> 'a t -> bool
(** [forAll f a] is [f x] if [a] is [Some x], [true] otherwise *)

(** {2 Conversion} *)

(* new *)
val okOr : 'e -> 'a t -> ('a, 'e) Result.result
(** [okOr e a] is [Ok x] if [a] is [Some x], [Error e] otherwise *)

(* new *)
val okOrLazy : (unit -> 'e) -> 'a t -> ('a, 'e) Result.result
(** [okOr errorFn a] is [Ok x] if [a] is [Some x], [Error (errorFn ())] otherwise *)

val toList : 'a t -> 'a list
(** [toList a] is [\[x\]] if [a] is [Some x], [\[\]] otherwise *)

type 'a sequence = ('a -> unit) -> unit
val toSeq : 'a t -> 'a sequence
(** [toSeq a] returns a sequence of [x] if a is [Some x], empty sequence otherwise *)
