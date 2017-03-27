
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Options} *)

type +'a t = 'a option

(** {2 Construction} *)

(* was return *)
val make : 'a -> 'a t
(** Monadic return, that is [return x = Some x] *)

val fromList : 'a list -> 'a t
(** Head of list, or [None] *)

val if_ : ('a -> bool) -> 'a -> 'a option
(** [if_ f x] is [Some x] if [f x], [None] otherwise
    @since 0.17 *)

val wrap : ?handler:(exn -> bool) -> ('a -> 'b) -> 'a -> 'b option
(** [wrap f x] calls [f x] and returns [Some y] if [f x = y]. If [f x] raises
    any exception, the result is [None]. This can be useful to wrap functions
    such as [Map.S.find].
    @param handler the exception handler, which returns [true] if the
        exception is to be caught. *)

val wrap2 : ?handler:(exn -> bool) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c option
(** [wrap2 f x y] is similar to {!wrap1} but for binary functions. *)

(** {2 Comparison} *)

val isSome : _ t -> bool

val isNone : _ t -> bool
(** @since 0.11 *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val compare : ('a -> 'a -> Comparison.comparison) -> 'a t -> 'a t -> Comparison.comparison

(** {2 Access} *)

val get : 'a -> 'a option -> 'a

val getOr : default:'a -> 'a t -> 'a
(** [getOr ~default o] extracts the value from [o], or
    returns [default] if [o = None].
    @since 0.18 *)

val getOrRaise : 'a t -> 'a
(** Open the option, possibly failing if it is [None]
    @raise Invalid_argument if the option is [None] *)

val getLazy : (unit -> 'a) -> 'a t -> 'a
(** [get_lazy defaultn x] unwraps [x], but if [x = None] it returns [default_fn ()] instead.
    @since 0.6.1 *)

(** {2 Iteration} *)

val forEach : ('a -> unit) -> 'a t -> unit
(** Iterate on 0 or 1 element *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Transform the element inside, if any *)

val mapOr : default:'b -> ('a -> 'b) -> 'a t -> 'b
(** [map_or ~default f o] is [f x] if [o = Some x], [default otherwise]
    @since 0.16 *)

(* new *)
val mapOrLazy : default:(unit -> 'b) -> ('a -> 'b) -> 'a t -> 'b

val maybe : ('a -> 'b) -> 'b -> 'a option -> 'b

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

(* alternative name: andThen *)
val flatMap : ('a -> 'b t) -> 'a t -> 'b t
(** Flip version of {!>>=} *)

val reduce : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** reduce on 0 or 1 element *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** Filter on 0 or 1 element
    @since 0.5 *)

(** {2 Applicative} *)

(* was (<*>) *)
val apply : ('a -> 'b) t -> 'a t -> 'b t

(** {2 Sequencing} *)

(* new *)
val and_ : 'b t -> 'a t -> 'b t

(** {2 Alternatives} *)

(* was (<+>) *)
val or_ : else_:'a t -> 'a t -> 'a t
(** [or_ ~else_ a] is [a] if [a] is [Some _], [b] otherwise *)

(* new *)
val orLazy : else_:(unit -> 'a t) -> 'a t -> 'a t

(* was choice *)
val any : 'a t list -> 'a t
(** [choice] returns the first non-[None] element of the list, or [None] *)

(** {2 Quantification} *)
(** TODO: too mathy? *)

val exists : ('a -> bool) -> 'a t -> bool
(** @since 0.17 *)

val forAll : ('a -> bool) -> 'a t -> bool
(** @since 0.17 *)

(** {2 Conversion and IO} *)

(* new *)
val okOr : 'e -> 'a t -> ('a, 'e) Result.result

(* new *)
val okOrLazy : (unit -> 'e) -> 'a t -> ('a, 'e) Result.result

val toList : 'a t -> 'a list

type 'a sequence = ('a -> unit) -> unit
val toSeq : 'a t -> 'a sequence
