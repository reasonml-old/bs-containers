module type S = sig
  type t

  include Equatable.S with type t := t

  val compare:t Comparator.t
end

module Default(T: sig type t end) : S = struct
  type t = T.t
  let equals = Pervasives.(=)
  let compare = Comparator.make(Pervasives.compare)
end 