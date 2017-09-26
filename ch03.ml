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
  let rank = function | E -> 0 | T(r, _, _, _) -> r

  let rec merge h1 h2 = match h1, h2 with
    | _, E -> h1
    | E, _ -> h2
    | T(_, x, l1, r1), T(_, y, l2, r2) ->
      let makeT x a b =
        if rank a >= rank b then T((rank b)+1, x, a, b)
        else T((rank a)+1, x, b, a)
      in
      if El.leq x y then makeT x l1 (merge r1 h2)
      else makeT y l2 (merge h1 r2)

  let insert x h = merge (T(1, x, E, E)) h

  let findMin = function
    | E -> raise Empty
    | T(_, x, _, _) -> x

  let deleteMin = function
    | E -> raise Empty
    | T(_, _, l, r) -> merge l r
end
