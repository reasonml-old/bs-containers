(** Basic comparator module  *)

type 'a t = 'a -> 'a -> Ordering.t

let make (compare: 'a -> 'a -> int) : 'a t = fun this that ->
    let r = compare this that in 
    if r = 0 then Ordering.Equal
    else begin
        if r > 0 then Ordering.Greater
        else Ordering.Less
    end 

let bytes = make Bytes.compare
let char = make Char.compare
let int (this : int) (that : int) =
    (if this < that
     then Ordering.Less
     else if this > that then Ordering.Greater else Ordering.Equal : 
                                                        Ordering.t)
let int32 = make Int32.compare
let int64 = make Int64.compare
let nativeInt = make Nativeint.compare
let string = make String.compare

let toEquality (comparator : 'a t) =
    (fun x  ->
         fun y  -> if (comparator x y) == Ordering.equal then true else false : 
                                                                            'a Equality.t)