module type Heap = sig
  module El: Ch02.Ordered
  type t
  val empty: t
  val isEmpty: t -> bool

  val merge: t -> t -> t
  val insert: El.t -> t -> t

  val findMin: t -> El.t
  val deleteMin: t -> t
end

module LeftistHeap(Elem: Ch02.Ordered) : (Heap with module El = Elem) = struct
  module El = Elem
  type t = E | T of int * El.t * t * t
  exception Empty

  let empty = E
  let isEmpty h = h = E
  let singleton x = T(1, x, E, E)
  let rank = function | E -> 0 | T(r, _, _, _) -> r
  let makeT x a b =
    if rank a >= rank b then T((rank b)+1, x, a, b)
    else T((rank a)+1, x, b, a)

  let rec merge h1 h2 = match h1, h2 with
    | _, E -> h1
    | E, _ -> h2
    | T(_, x, l1, r1), T(_, y, l2, r2) ->
      if El.leq x y then makeT x l1 (merge r1 h2)
      else makeT y l2 (merge h1 r2)

  let insert x h = merge (T(1, x, E, E)) h

  let findMin = function
    | E -> raise Empty
    | T(_, x, _, _) -> x

  let deleteMin = function
    | E -> raise Empty
    | T(_, _, l, r) -> merge l r

  (* Exercise 3.2 *)
  let insert2 x = function
    | E -> singleton x
    | T(r, y, left, right) as tree ->
      if El.leq x y then T(1, x, tree, E)
      else makeT y left (insert x right)

  (* Exercise 3.3 *)
  let fromList = function
    | [] -> E
    | [x] -> singleton x
    | _ as lst ->
      let singletons = List.map singleton lst in
      let rec mergeLoop l =
        let rec mergePair l = match l with
          | h1 :: h2 :: rest -> (merge h1 h2) :: mergePair rest
          | _ -> l
        in match l with
        | [] -> E
        | [x] -> x
        | _ -> mergeLoop (mergePair l)
      in mergeLoop singletons
end
