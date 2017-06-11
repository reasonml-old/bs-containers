open Perf

let bigArray = Barray.makeWithInit 1_000_000 (fun _ -> Random.int 1024)
let bigSeq = Barray.toSequence bigArray

let toList = Array.to_list

let fromList = Array.of_list

let bigList = toList bigArray

let fromSequenceArrayPush i =
  let open Bsequence in
  let arr = ref [||] in
  let rec aux i =
    match i () with
    | Nil -> !arr
    | Cons(x, r) -> ignore @@ Js.Array.push x !arr; aux r  
  in aux i

let listToSequence (l: 'a list) : 'a Bsequence.t =
  let rec aux ll () = match ll with
  | [] -> Bsequence.Nil
  | (e::r) -> Bsequence.Cons(e, aux (r))
  in aux l

let getSeqLength (s:'a Bsequence.t) : int = 
  let open Bsequence in
  let rec aux i ss = 
    match ss () with
    | Nil -> i
    | Cons (_, r) -> aux (i+1) r
  in aux 0 s

let getFirstFromSeq (s:'a Bsequence.t) : 'a option =
  match s () with 
  | Bsequence.Nil -> None
  | Bsequence.Cons (x, _) -> Some x

let fromSequencePreAllocArray (s:'a Bsequence.t) :'a Barray.t =
  let len = getSeqLength s in 
  let first = getFirstFromSeq s in
  match first with
  | None -> [||]
  | Some f -> begin 
    let arr = ref (Barray.make len f) in
    let rec aux idx ss = 
      match ss () with 
      | Bsequence.Nil -> !arr
      | Bsequence.Cons (x, r) -> (!arr).(idx) <- x; aux (idx+1) r 
    in aux 0 s  
    end 

(** Do some correctness test first *)
let _ = 
  let lst =  [1; 2; 3; 4] in
  let arr0 = fromList lst in
  let arr1 = fromSequenceArrayPush (listToSequence [1; 2; 3; 4]) in
  let arr2 = fromSequencePreAllocArray (listToSequence [1; 2; 3; 4]) in
  Js.log arr0;
  Js.log arr1;
  Js.log arr2

let _ =
  runPerfTest "listToArray - fromList" ~repeatTimes:100 (fun _ ->
    ignore @@ fromList bigList
  );
  runPerfTest "listToArray - fromSequence" ~repeatTimes:100 (fun _ -> 
    ignore @@ fromSequenceArrayPush (listToSequence bigList)
  );
  let listSeq = listToSequence bigList in 
  runPerfTest "listToArray - fromSequence - predefined seq" ~repeatTimes:100 (fun _ -> 
    ignore @@ fromSequenceArrayPush listSeq
  );
  runPerfTest "listToArray - fromSequence - preallocated array" ~repeatTimes: 100 (fun _ ->
    ignore @@ fromSequencePreAllocArray (listToSequence bigList)
  )

