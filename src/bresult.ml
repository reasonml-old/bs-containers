
(* This file is free software, part of containers. See file "license" for more details. *)

(*

- [x] Use `camelCase` instead of `snake_case`
- [x] Get rid of scary monadic and math-y terminology
- [x] Follow the conventions used in the `Js.*` modules
- [x] Type functions correctly, e.g. `compare` should return a proper variant, not `0`, `1` or `-1`
- [x] Remove operators and aliases, e.g. `pure` as an alias for `return`
- [ ] Remove the use of exceptions for internal logic (maybe? seems very sketchy to me but perhaps there's a really really good reason, see [this example](https://github.com/BuckleTypes/bs-containers/blob/master/src/bopt.ml#L163))
- [x] Replace `Format.printf` calls, they pull in a lot stuff for little benefit
- [ ] Document everything properly, with examples
- [ ] Add tests for everything

*)

(** {1 Error Monad} *)

type 'a sequence = ('a -> unit) -> unit
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> Comparison.comparison


type (+'good, +'bad) t = ('good, 'bad) Result.result =
  | Ok of 'good
  | Error of 'bad


let make x = Ok x

let fail s = Error s

let fromException e =
  let msg = Printexc.to_string e in
  Error msg

(* TODO: Doesn't seem to work properly in bs *)
let fromExceptionTrace e =
  let res = (Printexc.to_string e) ^ "\n" ^ (Printexc.get_backtrace ()) in
  Error res

let guard f =
  try Ok (f ())
  with e -> Error e

let guardToString f =
  try Ok (f())
  with e -> fromException e

let guardToStringTrace f =
  try Ok (f())
  with e -> fromExceptionTrace e

let wrap f x =
  try make (f x)
  with e -> Error e

let wrap2 f x y =
  try make (f x y)
  with e -> Error e

let wrap3 f x y z =
  try make (f x y z)
  with e -> Error e


let isOk = function
  | Ok _ -> true
  | Error _ -> false

let isError = function
  | Ok _ -> false
  | Error _ -> true

let equal ?(err=Pervasives.(=)) eq a b = match a, b with
  | Ok x, Ok y -> eq x y
  | Error e, Error e' -> err e e'
  | _ -> false

let compare ?(err=Comparison.compare) cmp a b = match a, b with
  | Ok x, Ok y -> cmp x y
  | Ok _, _  -> Comparison.Greater
  | _, Ok _ -> Comparison.Less
  | Error e, Error e' -> err e e'


exception GetError

let get default = function
  | Ok x -> x
  | Error _ -> default

let getOrRaise = function
  | Ok x -> x
  | Error _ -> raise GetError

let getOr ~default = function
  | Ok x -> x
  | Error _ -> default

let getLazy defaultFn = function
  | Ok x -> x
  | Error _ -> defaultFn ()


let forEach f e = match e with
  | Ok x -> f x
  | Error _ -> ()

let map f = function
  | Ok x -> Ok (f x)
  | Error e -> Error e

let mapOr ~default f = function
  | Ok x -> f x
  | Error _ -> default

let mapOrLazy ~default f = function
  | Ok x -> f x
  | Error _ -> default ()

let mapError f = function
  | Ok _ as res -> res
  | Error e -> Error (f e)

let maybe f default = mapOr ~default f

let map2 f a b = match a, b with
  | Error e, _
  | _, Error e -> Error e
  | Ok x, Ok y -> Ok (f x y)

let mapEither f g = function
  | Ok x -> Ok (f x)
  | Error e -> Error (g e)

let catch e ~ok ~err = match e with
  | Ok x -> ok x
  | Error e -> err e

let flatMap f e = match e with
  | Ok x -> f x
  | Error e -> Error e

let reduce f acc = function
  | Error _ -> acc
  | Ok x -> f acc x

let filter p = function
  | Ok x when p x -> Ok x
  | _ -> Error ()

let and_ b = function
  | Ok _ -> b
  | Error e -> Error e

let flatten = function
  | Ok (Ok x) -> Ok x
  | Ok (Error e) -> Error e
  | (Error _) as e -> e

let zip a b = match a, b with
  | Ok x, Ok y -> Ok (x, y)
  | Ok _, Error e -> Error e
  | Error e, _  -> Error e


let apply f a = match f with
  | Error e -> fail e
  | Ok f -> map f a


let or_ ~else_ a = match a with
  | Ok _ -> a
  | Error _ -> else_

let orLazy ~else_ a = match a with
  | Ok _ -> a
  | Error _ -> else_ ()

let any l =
  let rec find_ = function
    | [] -> raise Not_found
    | ((Ok _) as res) :: _ -> res
    | (Error _) :: rest -> find_ rest
  in
  try find_ l
  with Not_found ->
    Error (List.map (function Error e -> e | Ok _ -> assert false) l)


let exists p = function
  | Error _ -> false
  | Ok x -> p x

let forAll p = function
  | Error _ -> true
  | Ok x -> p x


let mapList f l =
  let rec map acc = function
    | [] -> Ok (List.rev acc)
    | a::rest ->
      match f a with
        | Error e -> Error e
        | Ok x -> map (x::acc) rest
  in map [] l

exception LocalExit

let reduceSeq f acc seq =
  let err = ref None in
  try
    let acc = ref acc in
    seq
      (fun a -> match f !acc a with
         | Error e ->
          err := Some e;
          raise LocalExit
         | Ok x -> acc := x);
    Ok !acc
  with LocalExit ->
    match !err with
      | None -> assert false
      | Some e -> Error e

let reduceList f acc l = reduceSeq f acc (fun k -> List.iter k l)


let retry n f =
  let rec retry n acc = match n with
    | 0 -> fail (List.rev acc)
    | _ ->
      match f () with
      | Ok _ as res -> res
      | Error e -> retry (n-1) (e::acc)
  in retry n []


let toOption = function
  | Ok x -> Some x
  | Error _ -> None

let fromOption = function
  | None -> Error ()
  | Some x -> Ok x

let toList = function
  | Error _ -> []
  | Ok x -> [x]

let toSeq e k = match e with
  | Ok x -> k x
  | Error _ -> ()