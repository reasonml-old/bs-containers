
(* This file is free software, part of containers. See file "license" for more details. *)

(*
TODO:

- [x] Use `camelCase` instead of `snake_case`
- [x] Get rid of scary monadic and math-y terminology
- [x] Follow the conventions used in the `Js.*` modules
- [ ] Type functions correctly, e.g. `compare` should return a proper variant, not `0`, `1` or `-1`
- [x] Remove operators and aliases, e.g. `pure` as an alias for `return`
- [x] Remove the use of exceptions for internal logic (maybe? seems very sketchy to me but perhaps there's a really really good reason, see [this example](https://github.com/BuckleTypes/bs-containers/blob/master/src/bopt.ml#L163))
- [x] Replace `Format.printf` calls, they pull in a lot stuff for little benefit
- [ ] Document everything properly, with examples
- [ ] Add tests for everything
*)

(** {1 Options} *)

type 'a t = 'a option

let make x = Some x

let fromList = function
  | x::_ -> Some x
  | [] -> None

let if_ p x =
  if p x
  then Some x
  else None

let wrap ?(handler=fun _ -> true) f x =
  try Some (f x)
  with e ->
    if handler e then None else raise e

let wrap2 ?(handler=fun _ -> true) f x y =
  try Some (f x y)
  with e ->
    if handler e then None else raise e


let isSome = function
  | None -> false
  | Some _ -> true

let isNone = function
  | None -> true
  | Some _ -> false

let equal f o1 o2 = match o1, o2 with
  | None, None -> true
  | Some _, None
  | None, Some _ -> false
  | Some x, Some y -> f x y

let compare f o1 o2 = match o1, o2 with
  | None, None -> Comparison.Equal
  | Some _, None -> Comparison.Greater
  | None, Some _ -> Comparison.Less
  | Some x, Some y -> f x y


let get default x = match x with
  | None -> default
  | Some y -> y

let getOr ~default x = match x with
  | None -> default
  | Some y -> y

let getOrRaise = function
  | Some x -> x
  | None -> invalid_arg "CCOpt.get_exn"

let getLazy defaultFn x = match x with
  | None -> defaultFn ()
  | Some y -> y
  

let forEach f o = match o with
  | None -> ()
  | Some x -> f x

let map f = function
  | None -> None
  | Some x -> Some (f x)

let mapOr ~default f = function
  | None -> default
  | Some x -> f x

let mapOrLazy ~default f = function
  | None -> default ()
  | Some x -> f x

let maybe f default = mapOr ~default f

let map2 f o1 o2 = match o1, o2 with
  | None, _
  | _, None -> None
  | Some x, Some y -> Some (f x y)

let flatMap f o = match o with
  | None -> None
  | Some x -> f x

let reduce f acc o = match o with
  | None -> acc
  | Some x -> f acc x

let filter p o = match o with
  | Some x when p x -> o
  | _ -> None


let apply f x = match f, x with
  | None, _
  | _, None -> None
  | Some f, Some x -> Some (f x)


let and_ o = function
  | None -> None
  | Some _ -> o


let or_ ~else_ a = match a with
  | None -> else_
  | Some _ -> a

let orLazy ~else_ a = match a with
  | None -> else_ ()
  | Some _ -> a

let any l = List.fold_left (fun a b -> or_ ~else_:b a) None l


let exists p = function
  | None -> false
  | Some x -> p x

let forAll p = function
  | None -> true
  | Some x -> p x


let okOr e = function
  | Some x -> Result.Ok x
  | None -> Result.Error e

let okOrLazy errFn = function
  | Some x -> Result.Ok x
  | None -> Result.Error (errFn ())

let toList o = match o with
  | None -> []
  | Some x -> [x]

type 'a sequence = ('a -> unit) -> unit
let toSeq o k = match o with
  | None -> ()
  | Some x -> k x
