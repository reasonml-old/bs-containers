
module type S = sig
  type t

  include Equatable.S with type t := t

  val compare:t Comparator.t
end

module Default (T: sig type t end) : S with type t = T.t