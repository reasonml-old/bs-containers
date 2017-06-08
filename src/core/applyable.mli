(*
  Gramatically correct: Applicable
  Haskell: Applicative
  Fantasyland: Applicative
*)

module type S = sig
  type 'a t

  include Mappable.S with type 'a t := 'a t

  (*
    Haskell: ??
    Fantasyland: ap (Apply category)
  *)
  val apply : ('a -> 'b) t -> 'a t -> 'b t

  (*
    Alternative names: ofValue, fromValue, from
    Haskell: pure (return)
    Fantasyland: of
  *)
  val of_ : 'a -> 'a t
end