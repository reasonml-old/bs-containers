(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Utils around char} *)


(*-- Start stdlib char, from https://github.com/ocaml/ocaml/blob/4.02.3/stdlib/char.mli --*)

external code : char -> int = "%identity"
(** Return the ASCII code of the argument. *)

val chr : int -> char
(** Return the character with the given ASCII code.
   Raise [Invalid_argument "Char.chr"] if the argument is
   outside the range 0--255. *)

val escaped : char -> string
(** Return a string representing the given character,
   with special characters escaped following the lexical conventions
   of OCaml. *)

val lowercase : char -> char
(** Convert the given character to its equivalent lowercase character. *)

val uppercase : char -> char
(** Convert the given character to its equivalent uppercase character. *)

type t = char
(** An alias for the type of characters. *)

val compare: t -> t -> int
(** The comparison function for characters, with the same specification as
    {!Pervasives.compare}.  Along with the type [t], this function [compare]
    allows the module [Char] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

(**/**)

(* The following is for system use only. Do not call directly. *)
external unsafe_chr : int -> char = "%identity"

(*-- End stdlib char --*)


val equal : t -> t -> bool
val compare : t -> t -> int

val lowercase_ascii : t -> t
(** See {!Char}
    @since 0.20 *)

val uppercase_ascii : t -> t
(** See {!Char}
    @since 0.20 *)

val of_int_exn : int -> t
(** Alias to {!Char.chr}
    @raise Invalid_argument if the int is not within [0,...,255]
    @since 1.0 *)

val of_int : int -> t option
(** Safe version of {!of_int}
    @since 1.0 *)

val to_int : t -> int
(** Alias to {!Char.code}
    @since 1.0 *)

val pp : Buffer.t -> t -> unit
val print : Format.formatter -> t -> unit
