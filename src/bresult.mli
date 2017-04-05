
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Result Monad}

    Uses the new "result" type from OCaml 4.03. *)

type 'a sequence = ('a -> unit) -> unit
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> Ordering.t

type (+'good, +'bad) t = ('good, 'bad) Result.result =
  | Ok of 'good
  | Error of 'bad
  (** alias of the standard or polyfilled [result] type *)

(** {2 Construction} *)

(* was return *)
val make : 'a -> ('a, 'err) t
(** [make x] is [Ok x] *)

val fail : 'err -> ('a, 'err) t
(** [fail e] is [Error e] *)

val fromException : exn -> ('a, string) t
(** [fromException e] is [Error s] where [s] is the string representation of [e] *)

val fromExceptionTrace : exn -> ('a, string) t
(** [fromExceptionTrace e] is [Error s] whre [s] is the string representation of
    [e] along with the stacktrace

    Remember to call [Printexc.record_backtrace true] and compile with the
    debug flag for this to work. *)

val guard : (unit -> 'a) -> ('a, exn) t
(** [guard f] is [Ok (f ())] if [f ()] does not raise an exxeption, [Error e]
    if it does, where [e] is the exception raised *)

val guardToString : (unit -> 'a) -> ('a, string) t
(** [guardToString f] is [Ok (f ())] if [f ()] does not raise an exxeption,
    [Error s] if it does, where [s] is the string representation of the
    exception raised *)

val guardToStringTrace : (unit -> 'a) -> ('a, string) t
(** [guardToStringTrace f] is [Ok (f ())] if [f ()] does not raise an exxeption,
    [Error s] if it does, where [s] is the string representation and stacktrace
    of the exception raised *)

val wrap : ('a -> 'b) -> 'a -> ('b, exn) t
(** [wrap f x] is [Ok (f x)] if [f x] does not raise an exception, [Error] if
    it does, where [e] is the exception raised *)

val wrap2 : ('a -> 'b -> 'c) -> 'a -> 'b -> ('c, exn) t
(** [wrap2 f x y] is [Ok (f x y)] if [f x y] does not raise an exception,
    [Error] if it does, where [e] is the exception raised *)

val wrap3 : ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> ('d, exn) t
(** [wrap3 f x y z] is [Ok (f x y z)] if [f x y z] does not raise an exception,
    [Error] if it does, where [e] is the exception raised *)

(** {2 Comparsion} *)

val isOk : ('a, _) t -> bool
(** [isOk a] is [true] if [a] is [Ok _], [false] otherwise *)

val isError : ('a, _) t -> bool
(** [isError a] is [true] if [a] is [Error], [false] otherwise *)

val equal : ?err:'err equal -> 'a equal -> ('a, 'err) t equal
(** [equal ~err p a b] is [err e1 e2] if both are [Error _], [f x y] if both are
    [Ok _], [false] otherwise

    @param err is the equality function used when both [a] and [b] are
               [Error _]. It default to [Pervasives.(=)]. *)

val compare : ?err:'err ord -> 'a ord -> ('a, 'err) t ord
(** [compare ~err f a b] is [err e1 e2] if both are [Error _], [f x y] if both
    are [Ok _], [Greater] if [a] is [Ok _] and [b] is [Error _], [Less] if vice
    versa.

    @param err is the comparison function used when both [a] and [b] are
               [Error _]. It default to [Comparison.compare]. *)

(** {2 Access} *)

exception GetError

(* new *)
val get : 'a -> ('a, _) t -> 'a
(** [get default a] is [x] if [a] is [Ok x], [default] otherwise *)

val getOr : default:'a -> ('a, _) t -> 'a
(** [getOr ~default a] is [x] if [a] is [Ok x], [default] otherwise *)

val getOrRaise : ('a, _) t -> 'a
(** [getOrRaise a] is [x] if [a] is [Ok x], raises [GetError] otherwise

    @raise GetError if given option is None    
*)

(* new *)
val getLazy : (unit -> 'a) -> ('a, _) t -> 'a
(** [getLazy defaultFn a] is [x] if [a] is [Ok x], [defaultFn ()] otherwise *)

(** {2 Iteration} *)

val forEach : ('a -> unit) -> ('a, _) t -> unit
(** [forEach f a] calls [f x] if [a] is [Ok x] *)

val map : ('a -> 'b) -> ('a, 'err) t -> ('b, 'err) t
(** [map f a] is [Ok (f x)] if [a] is [Ok x], [Error _] otherwise *)

val mapOr : default:'b -> ('a -> 'b) ->  ('a, _) t -> 'b
(** [mapOr ~default f a] is [Ok (f x)] if [a] is [Ok x], [default] otherwise *)

(* new *)
val mapOrLazy : default:(unit -> 'b) -> ('a -> 'b) ->  ('a, _) t -> 'b
(** [mapOrLazy ~default f a] is [Ok (f x)] if [a] is [Ok x], [default ()]
    otherwise *)

(* new *)
val maybe : ('a -> 'b) -> 'b -> ('a, _) t -> 'b
(** [maybe f default a] is [Ok (f x)] if [a] is [Ok x], [default] otherwise *)

val mapError : ('err1 -> 'err2) -> ('a, 'err1) t -> ('a, 'err2) t
(** Map on the error variant *)

(* new *)
val map2 : ('a -> 'b -> 'c) -> ('a, 'err) t -> ('b, 'err) t -> ('c, 'err) t
(** [map2 f a b] is [Ok (f x y)] if [a] is [Ok x], and [b] is [Ok y],
    [Error _] otherwise *)

(* was map2, TODO: remove? *)
val mapEither : ('a -> 'b) -> ('err1 -> 'err2) -> ('a, 'err1) t -> ('b, 'err2) t
(** [mapEither f errFn a] is [Ok (f x)] if [a] is [Ok x], [errFn e] if [a] is
    [Error e] *)

val catch :  ok:('a -> 'b) -> err:('err -> 'b) -> ('a, 'err) t-> 'b
(** [catch ~ok ~err a] is [Ok (ok x)] if [a] is [Ok x], [err e] if [a] is
    [Error e] *)

val flatMap : ('a -> ('b, 'err) t) -> ('a, 'err) t -> ('b, 'err) t

(* new-ish *)
val reduce : ('b -> 'a -> 'b) -> 'b -> ('a, _) t -> 'b

(* new, TODO: remove? *)
val filter : ('a -> bool) -> ('a, _) t -> ('a, unit) t

(** {2 Applicative} *)

(* was <*> *)
val apply : ('a -> 'b, 'err) t -> ('a, 'err) t -> ('b, 'err) t
(** [apply maybeF a] is [Ok (f x)] if [maybeF] is [Ok f] and [a] is [Ok x],
    [Error _] otherwise *)

(** {2 Composition} *)

(* new *)
val and_ : ('b, 'err) t -> ('a, 'err) t -> ('b, 'err) t
(** [and_ a b] is [b] if [a] is [Ok _], [Error _] otherwise *)

(* new *)
val or_ : else_:('a, 'e) t -> ('a, 'e) t -> ('a, 'e) t
(** [or_ ~else_ a] is [a] if [a] is [Ok _], [else_] otherwise *)

(* new *)
val orLazy : else_:(unit -> ('a, 'e) t) -> ('a, 'e) t -> ('a, 'e) t
(** [orLazy ~else_ a] is [a] if [a] is [Ok _], [else_ ()] otherwise *)

val flatten : (('a, 'err) t, 'err) t -> ('a, 'err) t
(** [flatten a] is [Ok x] if [a] is [Ok (Ok x)], [Error _] otherwise *)

val zip : ('a, 'err) t  -> ('b, 'err) t -> (('a * 'b), 'err) t
(** [zip a b] is [Ok (x, y)] if [a, b] is [Ok x, Ok y], [Error _] otherwise *)

val any : ('a, 'err) t list -> ('a, 'err list) t
(** [any l] returns the first non-[Error] element of the list if it exists,
    [Error es] otherwise, where [es] is the list of errors *)

(** {2 Quantification} *)
(** TODO: too mathy? *)

(* new *)
val exists : ('a -> bool) -> ('a, _) t -> bool
(** [exists f a] is [f x] if [a] is [Ok x], [false] otherwise *)

(* new *)
val forAll : ('a -> bool) -> ('a, _) t -> bool
(** [forAll f a] is [f x] if [a] is [Ok x], [true] otherwise *)

(** {2 Collections} *)

val mapList : ('a -> ('b, 'err) t) -> 'a list -> ('b list, 'err) t
(** [mapList f l] will return [Ok l'] where [l'] is a list of the elements of
    [l] mapped by [f] if [f] returns [Ok _] for every element of [l], otherwise
    it returns the [Error _] returned by [f] *)

val reduceList : ('b -> 'a -> ('b, 'err) t) -> 'b -> 'a list -> ('b, 'err) t
(** [reduceList f initial l] will return [Ok x] where [x] is a the result of
    reducing [l] by [f] from [initial] if [f] returns [Ok _] for every element
    of [l], otherwise it returns [Error _] returned by [f] *)

val reduceSeq : ('b -> 'a -> ('b, 'err) t) -> 'b -> 'a sequence -> ('b, 'err) t
(** [reduceSeq f initial s] will return [Ok x] where [x] is a the result of
    reducing [s] by [f] from [initial] if [f] returns [Ok _] for every element
    of [s], otherwise it returns [Error _] returned by [f] *)

(** {2 Misc} *)

val retry : int -> (unit -> ('a, 'err) t) -> ('a, 'err list) t
(** [retry n f] calls [f] at most [n] times, returning the first result
    of [f ()] that is [Ok _]. Otherwise return [Error es] where [es] is the list
    of errors from the failed attempts. *)

(** {2 Conversion} *)

val toOption : ('a, _) t -> 'a option
(** [toOption a] is [Some x] if [a] is [Ok x], [None] otherwise *)

val fromOption : 'a option -> ('a, unit) t
(** [fromOption a] is [Ok x] if [a] is [Some x], [Error ()] otherwise *)

val toList : ('a, _) t -> 'a list
(** [toList a] is [\[x\]] if [a] is [Ok x], [\[\]] otherwise *)

val toSeq : ('a, _) t -> 'a sequence
(** [toSeq a] returns a sequence of [x] if a is [Ok x], empty sequence otherwise *)