(* This file is free software, part of containers. See file "license" for more details. *)

type t = int64

let (+) = Int64.add

let (-) = Int64.sub

let (~-) = Int64.neg

let ( * ) = Int64.mul

let (/) = Int64.div

let (mod) = Int64.rem

let (land) = Int64.logand

let (lor) = Int64.logor

let (lxor) = Int64.logxor

let lnot = Int64.lognot

let (lsl) = Int64.shift_left

let (lsr) = Int64.shift_right_logical

let (asr) = Int64.shift_right

let equal (x:t) y = Pervasives.compare x y = 0

let hash x = Pervasives.abs (Int64.to_int x)

let maxInt = Int64.max_int

let minInt = Int64.min_int

let abs = Int64.abs

(** {2 Conversion} *)

let ofIntExn = Int64.of_int

let ofInt x = try Some (ofIntExn x) with Failure _ -> None

let toInt = Int64.to_int

let ofNativeintExn = Int64.of_nativeint

let ofNativeint x = try Some (ofNativeintExn x) with Failure _ -> None

let toNativeint = Int64.to_nativeint

let ofInt32Exn = Int64.of_int32

let ofInt32 x = try Some (ofInt32Exn x) with Failure _ -> None

let toInt32 = Int64.to_int32

let ofFloatExn = Int64.of_float

let ofFloat x = try Some (ofFloatExn x) with Failure _ -> None

let toFloat = Int64.to_float

let ofStringExn = Int64.of_string

let ofString x = try Some (ofStringExn x) with Failure _ -> None

let toString = Int64.to_string

let compare a b = Int64.compare a b
