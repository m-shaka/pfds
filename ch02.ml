exception Failure of string

module type Stack = sig
  type 'a t
  val empty: 'a t
  val isEmpty: 'a t -> bool
  val cons: 'a -> 'a t -> 'a t
  val head: 'a t -> 'a
  val tail: 'a t -> 'a t
  val (++): 'a t -> 'a t -> 'a t
  val update: 'a t -> int -> 'a -> 'a t
end

module ListStack: Stack = struct
  type 'a t = 'a list
  let empty = []
  let isEmpty l = l = []

  let cons x xs = x :: xs
  let head = function
    | [] -> raise (Failure "empty")
    | h :: t -> h
  let tail = function
    | [] -> raise (Failure "empty")
    | h :: t -> t
  let rec (++) xs ys = match xs with
    | [] -> xs
    | h :: t -> h :: t ++ ys
  let rec update xs i y = match xs, i with
    | [], _ -> raise (Failure "empty")
    | _ :: t, 0 -> y :: t
    | h :: t, _ -> h :: update t (i-1) y
end

module CustomStack: Stack = struct
  type 'a t = Nil | Cons of 'a * 'a t
  let empty = Nil
  let isEmpty l = l = Nil

  let cons x xs = Cons(x, xs)
  let head = function
    | Nil -> raise (Failure "empty")
    | Cons(h, t) -> h
  let tail = function
    | Nil -> raise (Failure "empty")
    | Cons(h, t) -> t
  let rec (++) xs ys = match xs with
    | Nil -> ys
    | Cons(h, t) -> Cons(h, t ++ ys)
  let rec update xs i y = match xs, i with
    | Nil, _ -> raise (Failure "empty")
    | Cons(_, t), 0 -> Cons(y, t)
    | Cons(h, t), _ -> Cons(h, update t (i-1) y)
end

(* Exercise 2.1 *)
let rec suffixes xs = match xs with
  | [] -> []
  | _ :: t -> xs :: suffixes t

module type Set = sig
  type el
  type t
  val empty: t
  val insert: el -> t -> t
  val member: el -> t -> bool
  val member2: el -> t -> bool
  val insert2: el -> t -> t
  val insert3: el -> t -> t
  val complete: el -> int -> t
  val create: el -> int -> t
end

module type Ordered = sig
  type t
  val eq: t -> t -> bool
  val lt: t -> t -> bool
  val leq: t -> t -> bool
end

module UnbalancedSet(El: Ordered) : (Set with type el = El.t) = struct
  type el = El.t
  type t = E | T of t * el * t

  let empty = E
  let rec member x = function
    | E -> false
    | T(left, y, right) ->
      if El.lt x y then member x left
      else if El.lt y x then member x right
      else true
  let rec insert x = function
    | E -> T(E, x, E)
    | T(left, y, right) as tree ->
      if El.eq x y then tree
      else if El.lt x y then T(insert x left, y, right)
      else T(left, y, insert x right)

  (* Exercise 2.2 *)
  let member2 x = function
    | E -> false
    | T(_, y, _) as tree ->
      let rec loop tree acc = match tree with
        | E -> x = acc
        | T(left, y', right) ->
          if El.lt x y' then loop left acc
          else loop right y'
      in loop tree y

  (* Exercise 2.3 *)
  let rec insert2 x = function
    | E -> T(E, x, E)
    | T(left, y, right) ->
      if El.eq x y then raise (Failure "")
      else if El.lt x y then T(insert2 x left, y, right)
      else T(left, y, insert2 x right)

  (* Exercise 2.4 *)
  let insert3 x = function
    | E -> T(E, x, E)
    | T(_, y, _) as tree ->
      let rec loop tree acc = match tree with
        | E -> if El.eq x acc then raise (Failure "") else T(E, x, E)
        | T(left, y, right) ->
          if El.lt x y then T(loop left acc, y, right)
          else T(left, y, loop right y)
      in loop tree y

  (* Exercise 2.5a *)
  let rec complete x d = match d with
    | 0 -> E
    | _ -> let subTree = complete x (d-1) in T(subTree, x, subTree)

  let rec create x s =
    let rec create2 m = match m with
      | 0 -> (E, T(E, x, E))
      | _ when m mod 2 = 1 ->
        let (t0, t1) = create2 (m / 2) in
        (T(t0, x, t0), T(t0, x, t1))
      | _ ->
        let (t0, t1) = create2 ((m - 1) / 2) in
        (T(t0, x, t1), T(t1, x, t1))
    in let res, _ = create2 s in res
end

(* Exercise 2.6 *)
exception NotFound
module type FiniteMap = sig
  type key
  type 'a map

  val empty: 'a map
  val bind: key -> 'a -> 'a map -> 'a map
  val lookup: key -> 'a map -> 'a
end

module UnbalancedMap(Key: Ordered) : (FiniteMap with type key = Key.t) = struct
  type key = Key.t
  type 'a map = E | M of 'a map * key * 'a * 'a map

  let empty = E
  let rec bind k v = function
    | E -> M(E, k, v, E)
    | M(left, k', v', right) ->
      if Key.eq k k' then raise (Failure "")
      else if Key.lt k k' then M(bind k v left, k', v', right)
      else M(left, k', v', bind k v right)

  let rec lookup k = function
    | E -> raise NotFound
    | M(left, k', v', right) ->
      if Key.eq k k' then v'
      else if Key.lt k k' then lookup k left
      else lookup k right
end
