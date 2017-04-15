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

(*-- End stdlib char --*)

include Comparable.S with type t := t

val lowerCaseIfAscii : t -> t

val upperCaseIfAscii : t -> t

val fromIntOrRaise : int -> t
(** Alias to {!Char.chr}
    @raise Invalid_argument if the int is not within [0,...,255] *)

val fromInt : int -> t option
(** Safe version of {!of_int} *)

val toInt : t -> int
(** Alias to {!Char.code} *)

(** '0' - '9' *)
val isDigit : t -> bool

(** 'a' - 'z' *)
val isLowerCase : t -> bool

(** 'A' - 'Z' *)
val isUpperCase : t -> bool

(** 'a' - 'z' or 'A' - 'Z' *)
val isLetter : t -> bool

(** 'a' - 'z' or 'A' - 'Z' or '0' - '9' *)
val isLetterOrDigit : t -> bool

(** ' ' - '~' *)
val isPrint : t -> bool

(** ' ' or '\t' or '\r' or '\n' *)
val isWhitespace : t -> bool

(** Return [Some i] if [is_digit c] and [None] otherwise. *)
val getDigit : t -> int option

(** Return [i] if [is_digit c].  Raises [Failure] otherwise. *)
val getDigitOrRaise : t -> int

(** Return 0 *)
val minValue : t

(** Return 255 *)
val maxValue : t