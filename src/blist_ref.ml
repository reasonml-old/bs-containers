  type 'a t = 'a list ref

  let push l x = l := x :: !l

  let pop l = match !l with
    | [] -> None
    | x::tail ->
      l := tail;
      Some x

  let popOrRaise l = match !l with
    | [] -> failwith "BList.Ref.popOrRaise"
    | x::tail ->
      l := tail;
      x

  let create() = ref []

  let clear l = l := []

  let lift f l = f !l

  let pushList r l =
    r := List.rev_append l !r

  (*$T
    let l = Ref.create() in Ref.push l 1; Ref.push_list l [2;3]; !l = [3;2;1]
  *)