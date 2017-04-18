
module type S = sig
    type t
    val hash: t -> int
end