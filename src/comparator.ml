type 'a t = 'a -> 'a -> Ordering.t

let make (compare: 'a -> 'a -> int) : 'a t = fun this that ->
  let r = compare this that in 
  if r = 0 then Ordering.Equal
  else begin
    if r > 0 then Ordering.Greater
    else Ordering.Less
  end 
