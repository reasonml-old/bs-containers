
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Error Monad}

    Uses the new "result" type from OCaml 4.03.

    @since 0.16 *)

type 'a sequence = ('a -> unit) -> unit
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> Comparison.comparison

type (+'good, +'bad) t = ('good, 'bad) Result.result =
  | Ok of 'good
  | Error of 'bad

(** {2 Construction} *)

(* was return *)
val make : 'a -> ('a, 'err) t
(** Successfully return a value *)

val fail : 'err -> ('a, 'err) t
(** Fail with an error *)

val fromException : exn -> ('a, string) t
(** [of_exn e] uses {!Printexc} to print the exception as a string *)

val fromExceptionTrace : exn -> ('a, string) t
(** [of_exn_trace e] is similar to [of_exn e], but it adds the stacktrace
    to the error message.

    Remember to call [Printexc.record_backtrace true] and compile with the
    debug flag for this to work. *)

val guard : (unit -> 'a) -> ('a, exn) t
(** [guard f] runs [f ()] and returns its result wrapped in [Ok]. If
    [f ()] raises some exception [e], then it fails with [Error e] *)

val guardToString : (unit -> 'a) -> ('a, string) t
(** Same as {!guard} but uses {!of_exn} to print the exception. *)

val guardToStringTrace : (unit -> 'a) -> ('a, string) t
(** Same as {!guard_str} but uses {!of_exn_trace} instead of {!of_exn} so
    that the stack trace is printed. *)

val wrap : ('a -> 'b) -> 'a -> ('b, exn) t
(** Same as {!guard} but gives the function one argument. *)

val wrap2 : ('a -> 'b -> 'c) -> 'a -> 'b -> ('c, exn) t
(** Same as {!guard} but gives the function two arguments. *)

val wrap3 : ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> ('d, exn) t
(** Same as {!guard} but gives the function three arguments. *)

(** {2 Comparsion} *)

val isOk : ('a, _) t -> bool
(** Return true if Ok *)

val isError : ('a, _) t -> bool
(** Return true if Error *)

val equal : ?err:'err equal -> 'a equal -> ('a, 'err) t equal

val compare : ?err:'err ord -> 'a ord -> ('a, 'err) t ord

(** {2 Access} *)

exception GetError

(* new *)
val get : 'a -> ('a, _) t -> 'a

val getOr : default:'a -> ('a, _) t -> 'a
(** [get_or e ~default] returns [x] if [e = Ok x], [default] otherwise *)

val getOrRaise : ('a, _) t -> 'a
(** Extract the value [x] from [Ok x], fails otherwise.
    You should be careful with this function, and favor other combinators
    whenever possible.
    @raise Get_error if the value is an error. *)

(* new *)
val getLazy : (unit -> 'a) -> ('a, _) t -> 'a

(** {2 Iteration} *)

val forEach : ('a -> unit) -> ('a, _) t -> unit
(** Apply the function only in case of Ok *)

val map : ('a -> 'b) -> ('a, 'err) t -> ('b, 'err) t
(** Map on success *)

val mapOr : default:'b -> ('a -> 'b) ->  ('a, _) t -> 'b
(** [map_or f e ~default] returns [f x] if [e = Ok x], [default] otherwise *)

(* new *)
val mapOrLazy : default:(unit -> 'b) -> ('a -> 'b) ->  ('a, _) t -> 'b

(* new *)
val maybe : ('a -> 'b) -> 'b -> ('a, _) t -> 'b

val mapError : ('err1 -> 'err2) -> ('a, 'err1) t -> ('a, 'err2) t
(** Map on the error variant *)

(* new *)
val map2 : ('a -> 'b -> 'c) -> ('a, 'err) t -> ('b, 'err) t -> ('c, 'err) t

(* was map2, TODO: remove? *)
val mapEither : ('a -> 'b) -> ('err1 -> 'err2) -> ('a, 'err1) t -> ('b, 'err2) t
(** Same as {!map}, but also with a function that can transform
    the error message in case of failure *)

val catch : ('a, 'err) t -> ok:('a -> 'b) -> err:('err -> 'b) -> 'b
(** [catch e ~ok ~err] calls either [ok] or [err] depending on
    the value of [e]. *)

val flatMap : ('a -> ('b, 'err) t) -> ('a, 'err) t -> ('b, 'err) t

(* new-ish *)
val reduce : ('b -> 'a -> 'b) -> 'b -> ('a, _) t -> 'b

(* new, TODO: remove? *)
val filter : ('a -> bool) -> ('a, _) t -> ('a, unit) t

(** {2 Composition} *)

(* new *)
val and_ : ('b, 'err) t -> ('a, 'err) t -> ('b, 'err) t

val flatten : (('a, 'err) t, 'err) t -> ('a, 'err) t
(** [flatten t], in case of success, returns [Ok o] from [Ok (Ok o)]. Otherwise,
    it fails with [Error e] where [e] is the unwrapped error of [t]. *)

val zip : ('a, 'err) t  -> ('b, 'err) t -> (('a * 'b), 'err) t
(** [zip a b], in case of success, returns [Ok (o, o')] with the ok values
    of [a] and [b]. Otherwise, it fails, and the error of [a] is chosen over the
    error of [b] if both fail. *)

(** {2 Alternatives} *)

(* new *)
val or_ : else_:('a, 'e) t -> ('a, 'e) t -> ('a, 'e) t
(** [or_ ~else_ a] is [a] if [a] is [Ok _], [else_] otherwise *)

(* new *)
val orLazy : else_:(unit -> ('a, 'e) t) -> ('a, 'e) t -> ('a, 'e) t
(** [orLazy ~else_ a] is [a] if [a] is [Ok _], [else_ ()] otherwise *)

val any : ('a, 'err) t list -> ('a, 'err list) t
(** [choose l] selects a member of [l] that is a [Ok _] value,
    or returns [Error l] otherwise, where [l] is the list of errors. *)

(** {2 Applicative} *)

(* was <*> *)
val apply : ('a -> 'b, 'err) t -> ('a, 'err) t -> ('b, 'err) t
(** [a <*> b] evaluates [a] and [b], and, in case of success, returns
    [Ok (a b)]. Otherwise, it fails, and the error of [a] is chosen
    over the error of [b] if both fail. *)

(** {2 Quantification} *)
(** TODO: too mathy? *)

(* new *)
val exists : ('a -> bool) -> ('a, _) t -> bool
(** [exists f a] is [f x] if [a] is [Some x], [false] otherwise *)

(* new *)
val forAll : ('a -> bool) -> ('a, _) t -> bool
(** [forAll f a] is [f x] if [a] is [Some x], [true] otherwise *)

(** {2 Collections} *)

val mapList : ('a -> ('b, 'err) t) -> 'a list -> ('b list, 'err) t

val reduceList : ('b -> 'a -> ('b, 'err) t) -> 'b -> 'a list -> ('b, 'err) t

val reduceSeq : ('b -> 'a -> ('b, 'err) t) -> 'b -> 'a sequence -> ('b, 'err) t

(** {2 Misc} *)

val retry : int -> (unit -> ('a, 'err) t) -> ('a, 'err list) t
(** [retry n f] calls [f] at most [n] times, returning the first result
    of [f ()] that doesn't fail. If [f] fails [n] times, [retry n f] fails
    with the list of successive errors. *)

(** {2 Conversion} *)

val toOption : ('a, _) t -> 'a option

val fromOption : 'a option -> ('a, unit) t

val toList : ('a, _) t -> 'a list
(** [toList a] is [\[x\]] if [a] is [Some x], [\[\]] otherwise *)

val toSeq : ('a, _) t -> 'a sequence