
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Drop-In replacement to Stdlib}

    This module is meant to be opened if one doesn't want to use both, say,
    [List] and [BList]. Instead, [List] is now an alias to
    {[struct
      include List
      include BList
    end
    ]}
*)

module Array = struct
  include Array
  include Barray
end
module ArrayLabels = struct
  include ArrayLabels
  include Barraylabels
end
module Array_slice = Barray_slice
module Bool = Bbool
module Char = struct
  include Char
  include (Bchar : module type of Bchar with type t := t)
end
module Float = Bfloat
module Format = struct
  include Format
  include Bformat
end
module Fun = Bfun
module Hash = Bhash
module Int = Bint
module Int64 = Bint64

(** @since 0.14 *)
module Hashtbl = struct
  include (Hashtbl : module type of Hashtbl
    with type statistics = Hashtbl.statistics
     and module Make = Hashtbl.Make
     and type ('a,'b) t = ('a,'b) Hashtbl.t
  )
  include Bhashtbl.Poly
  module type S' = Bhashtbl.S
  module Make' = Bhashtbl.Make
end
module Heap = Bheap
module List = struct
  include List
  include Blist
end
module ListLabels = struct
  include ListLabels
  include Blistlabels
end
module Map = struct
  module type OrderedType = Map.OrderedType
  include Bmap
end
module Option = Bopt
module Ord = Bord
module Pair = Bpair
module Parse = Bparse
module Random = struct
  include Random
  include Brandom
end
module Ref = Bref
module Result = struct
  include Result
  include Bresult
end
module Set = struct
  module type OrderedType = Set.OrderedType
  include Bset
end
module String = struct
  include String
  include Bstring
end
module Vector = Bvector
