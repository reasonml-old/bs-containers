(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Utils around char}

    @since 0.14 *)

include Char

let equals (a:char) b = a=b
let compare = Comparator.make Char.compare

let lowerCaseIfAscii c =
  if c >= 'A' && c <= 'Z'
  then Char.unsafe_chr (Char. code c + 32)
  else c

let upperCaseIfAscii c =
  if c >= 'a' && c <= 'z'
  then Char.unsafe_chr (Char.code c - 32)
  else c

let fromIntOrRaise = Char.chr
let fromInt c = try Some (fromIntOrRaise c) with _ -> None
let toInt = Char.code

let isLowerCase = function
  | 'a' .. 'z' -> true
  | _ -> false

let isUpperCase = function
  | 'A' .. 'Z' -> true
  | _ -> false

let isPrint = function
  | ' ' .. '~' -> true
  | _ -> false

let isWhitespace = function
  | '\t'
  | '\n'
  | '\011' (* vertical tab *)
  | '\012' (* form feed *)
  | '\r'
  | ' '
    -> true
  | _
    -> false
;;

let isDigit = function
  | '0' .. '9' -> true
  | _ -> false

let isLetter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

(* Writing these out, instead of calling [is_alpha] and [is_digit], reduces
   runtime by approx. 30% *)
let isLetterOrDigit = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false

let getDigitUnsafe t = toInt t - toInt '0'

let getDigitOrRaise t =
  if isDigit t
  then getDigitUnsafe t
  else failwith "Not a digit"
;;

let getDigit t = if isDigit t then Some (getDigitUnsafe t) else None

let minValue = chr 0
let maxValue = chr 255

let toString c = String.make 1 c